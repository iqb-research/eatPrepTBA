policy_fixture <- function(types, page = rep(1L, length(types)),
                            element = rep(NA_integer_, length(types))) {
  ids <- names(types)
  if (is.null(ids)) ids <- paste0("V", seq_along(types))
  codes <- tibble::tibble(
    variable_id = ids, variable_ref = ids, variable_source_type = "BASE",
    variable_level = 0L, variable_page = page, variable_section = 1L,
    variable_element = element
  )
  units <- tibble::tibble(unit_key = "U1", unit_codes = list(codes))
  design <- tibble::tibble(
    login_code = "P1", booklet_id = "B1", booklet_no = 1L, testlet_no = 1L,
    unit_booklet_no = 1L, unit_key = "U1", unit_alias = "first", variable_id = ids
  )
  ids_by_type <- c(FULL_CREDIT = 1, MISSING_NOT_REACHED = -96,
                   MISSING_BY_OMISSION = -99, MISSING_INVALID_RESPONSE = -98)
  status_by_type <- c(FULL_CREDIT = "CODING_COMPLETE", MISSING_NOT_REACHED = "NOT_REACHED",
                      MISSING_BY_OMISSION = "DISPLAYED", MISSING_INVALID_RESPONSE = "INVALID")
  coded <- design %>% dplyr::mutate(
    code_type = unname(types), code_status = unname(status_by_type[types]),
    code_id = unname(ids_by_type[types]),
    code_score = ifelse(types == "FULL_CREDIT", 1,
                        ifelse(types == "MISSING_NOT_REACHED", NA_real_, 0)),
    value = ifelse(types == "FULL_CREDIT", "answer", NA_character_)
  )
  list(units = units, design = design, coded = coded)
}

policy_capture <- function(expr) {
  messages <- character()
  value <- withCallingHandlers(expr, message = function(condition) {
    messages <<- c(messages, conditionMessage(condition))
    invokeRestart("muffleMessage")
  })
  list(value = value, messages = messages)
}

test_that("same-page recoding requires a confirmed name order or physical positions", {
  for (types in list(c("MISSING_NOT_REACHED", "FULL_CREDIT"),
                     c("FULL_CREDIT", "MISSING_BY_OMISSION"))) {
    f <- policy_fixture(types)
    safe <- complete_design(f$coded, f$units, f$design,
                             recode_omissions_to_not_reached = TRUE, diagnostics = "none")
    expect_equal(safe$code_type, types)
    expect_equal(safe$code_score, f$coded$code_score)
    confirmed <- complete_design(f$coded, f$units, f$design,
                                  recode_omissions_to_not_reached = TRUE,
                                  use_variable_names_for_recoding = TRUE, diagnostics = "none")
    expected <- if (types[1] == "FULL_CREDIT") c("FULL_CREDIT", "MISSING_NOT_REACHED") else
      c("MISSING_BY_OMISSION", "FULL_CREDIT")
    expect_equal(confirmed$code_type, expected)
    expect_equal(confirmed$code_status, f$coded$code_status)
    f$units$unit_codes[[1]]$variable_element <- 1:2
    physical <- complete_design(f$coded, f$units, f$design,
                                 recode_omissions_to_not_reached = TRUE, diagnostics = "none")
    expect_equal(physical$code_type, expected)
  }
})

test_that("unknown pages are displayed by name but do not invent trailing evidence", {
  f <- policy_fixture(c("FULL_CREDIT", "MISSING_BY_OMISSION", "MISSING_NOT_REACHED"),
                       page = c(1L, NA_integer_, 2L))
  positions <- get_design_order(f$design, f$units)
  expect_equal(positions$variable_id, c("V1", "V2", "V3"))
  safe <- complete_design(f$coded, f$units, f$design,
                           recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  expect_equal(safe$code_type, f$coded$code_type)
  trusted <- complete_design(f$coded, f$units, f$design,
                              recode_omissions_to_not_reached = TRUE,
                              use_variable_names_for_recoding = TRUE, diagnostics = "none")
  expect_equal(trusted$code_type, c("FULL_CREDIT", rep("MISSING_NOT_REACHED", 2)))
  raw <- complete_design(f$coded, f$units, f$design,
                          recode_omissions_to_not_reached = NULL, diagnostics = "none")
  separate <- recode_missings(raw, f$units, positions = positions,
                               recode_omissions_to_not_reached = TRUE,
                               use_variable_names_for_recoding = TRUE, diagnostics = "none")
  expect_equal(separate, trusted)
})

test_that("unknown-page work remains evidence across known unit boundaries", {
  f <- policy_fixture("MISSING_NOT_REACHED", page = NA_real_)
  later <- dplyr::mutate(f$design, unit_alias = "second", unit_booklet_no = 2L)
  design <- dplyr::bind_rows(f$design, later)
  worked <- dplyr::mutate(f$coded, unit_alias = "second", unit_booklet_no = 2L,
                          code_type = "FULL_CREDIT", code_status = "CODING_COMPLETE",
                          code_id = 1, code_score = 1, value = "answer")
  coded <- dplyr::bind_rows(f$coded, worked)
  out <- complete_design(coded, f$units, design, diagnostics = "none")
  expect_equal(out$code_type, c("MISSING_BY_OMISSION", "FULL_CREDIT"))
  expect_equal(out$code_score, c(0, 1))
})

test_that("confirmed names conflict explicitly and complete overrides resolve them", {
  f <- policy_fixture(c("FULL_CREDIT", "MISSING_BY_OMISSION", "MISSING_NOT_REACHED"),
                       page = c(2L, NA_integer_, 1L))
  expect_error(complete_design(f$coded, f$units, f$design,
                               use_variable_names_for_recoding = TRUE, diagnostics = "none"),
                "conflicts with physical metadata")
  override <- tibble::tibble(unit_key = "U1", variable_id = c("V1", "V2", "V3"), local_order = 1:3)
  out <- complete_design(f$coded, f$units, f$design, order_overrides = override,
                          use_variable_names_for_recoding = TRUE,
                          recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  expect_equal(out$code_type, c("FULL_CREDIT", rep("MISSING_NOT_REACHED", 2)))
})

test_that("renaming ambiguous variables cannot change analytical outcomes by default", {
  f <- policy_fixture(c("FULL_CREDIT", "MISSING_BY_OMISSION", "MISSING_NOT_REACHED"))
  original <- complete_design(f$coded, f$units, f$design,
                               recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  new_ids <- c("Z", "A", "M")
  f$units$unit_codes[[1]]$variable_id <- new_ids
  f$units$unit_codes[[1]]$variable_ref <- new_ids
  f$design$variable_id <- new_ids
  f$coded$variable_id <- new_ids
  renamed <- complete_design(f$coded, f$units, f$design,
                              recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  aligned <- renamed[match(new_ids, renamed$variable_id), ]
  expect_equal(aligned$code_type, original$code_type)
  expect_equal(aligned$code_score, original$code_score)
})

test_that("completion reports actual additions once and leaves uncertain new rows unresolved", {
  f <- policy_fixture(c("FULL_CREDIT", "MISSING_BY_OMISSION"))
  first <- policy_capture(complete_design(f$coded[1, ], f$units, f$design))
  expect_length(first$messages, 1)
  expect_match(first$messages, "Added: 1 rows (0 classified; 1 still unclassified)", fixed = TRUE)
  expect_true(is.na(first$value$code_type[2]))
  expect_false(first$value$response_present[2])
  second <- policy_capture(complete_design(first$value, f$units, f$design))
  expect_length(second$messages, 1)
  expect_match(second$messages, "Added: 0 rows", fixed = TRUE)
  expect_match(second$messages, "0 analytically changed; 2 unchanged", fixed = TRUE)
  expect_match(second$messages, "1 lack reliable order", fixed = TRUE)
  expect_equal(second$value$response_present, first$value$response_present)
  expect_equal(second$value, first$value)
  silent <- policy_capture(complete_design(f$coded, f$units, f$design, diagnostics = "none"))
  expect_length(silent$messages, 0)
  raw <- policy_capture(complete_design(f$coded[1, ], f$units, f$design,
                                        recode_omissions_to_not_reached = NULL))
  expect_length(raw$messages, 1)
  expect_match(raw$messages, "Completion only", fixed = TRUE)
  standalone <- policy_capture(recode_missings(first$value, f$units))
  expect_length(standalone$messages, 1)
  expect_match(standalone$messages, "recode_missings()", fixed = TRUE)
})

test_that("provided positions detect a completely removed testlet", {
  f <- policy_fixture("FULL_CREDIT")
  second <- dplyr::mutate(f$design, testlet_no = 2L, unit_booklet_no = 2L, unit_alias = "second")
  positions <- get_design_order(dplyr::bind_rows(f$design, second), f$units)
  expect_error(recode_missings(f$coded, f$units, positions = positions, diagnostics = "none"),
                "missing unit occurrences")
})

test_that("empty completion and classification retain a usable schema and zero counts", {
  f <- policy_fixture("FULL_CREDIT")
  completed <- policy_capture(complete_design(f$coded[0, ], f$units, f$design[0, ]))
  expect_equal(nrow(completed$value), 0)
  expect_type(completed$value$variable_order, "integer")
  expect_match(completed$messages, "0 analytically changed; 0 unchanged", fixed = TRUE)
  again <- recode_missings(completed$value, f$units, diagnostics = "none")
  expect_equal(again, completed$value)
})

test_that("standalone classification rejects lexical unit positions", {
  f <- policy_fixture("FULL_CREDIT")
  f$coded$unit_booklet_no <- "10"
  f$coded$variable_order <- 1L
  expect_error(recode_missings(f$coded, f$units, diagnostics = "none"), "unit_booklet_no")
})
