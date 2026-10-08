legacy_positions <- function(...) eatPrepTBA:::get_design_order(..., order_method = 'structure')

legacy_complete_design <- function(..., order_method = "structure", recode_existing_not_reached = TRUE) {
  complete_design(..., order_method = order_method,
                   recode_existing_not_reached = recode_existing_not_reached)
}

legacy_recode_missings <- function(..., order_method = "structure", recode_existing_not_reached = TRUE) {
  eatPrepTBA:::recode_missings(..., order_method = order_method,
                             recode_existing_not_reached = recode_existing_not_reached)
}

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
  list(value = value, messages = messages, text = paste(messages, collapse = "\n"))
}

test_that("same-page recoding requires a confirmed name order or physical positions", {
  for (types in list(c("MISSING_NOT_REACHED", "FULL_CREDIT"),
                     c("FULL_CREDIT", "MISSING_BY_OMISSION"))) {
    f <- policy_fixture(types)
    safe <- legacy_complete_design(f$coded, f$units, f$design,
                             recode_omissions_to_not_reached = TRUE, diagnostics = "none")
    expect_equal(safe$code_type, types)
    expect_equal(safe$code_score, f$coded$code_score)
    confirmed <- legacy_complete_design(f$coded, f$units, f$design,
                                  recode_omissions_to_not_reached = TRUE,
                                  use_variable_names_for_recoding = TRUE, diagnostics = "none")
    expected <- if (types[1] == "FULL_CREDIT") c("FULL_CREDIT", "MISSING_NOT_REACHED") else
      c("MISSING_BY_OMISSION", "FULL_CREDIT")
    expect_equal(confirmed$code_type, expected)
    expect_equal(confirmed$code_status, f$coded$code_status)
    f$units$unit_codes[[1]]$variable_element <- 1:2
    physical <- legacy_complete_design(f$coded, f$units, f$design,
                                 recode_omissions_to_not_reached = TRUE, diagnostics = "none")
    expect_equal(physical$code_type, expected)
  }
})

test_that("unknown pages are displayed by name but do not invent trailing evidence", {
  f <- policy_fixture(c("FULL_CREDIT", "MISSING_BY_OMISSION", "MISSING_NOT_REACHED"),
                       page = c(1L, NA_integer_, 2L))
  positions <- legacy_positions(f$design, f$units)
  expect_equal(positions$variable_id, c("V1", "V2", "V3"))
  safe <- legacy_complete_design(f$coded, f$units, f$design,
                           recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  expect_equal(safe$code_type, f$coded$code_type)
  trusted <- legacy_complete_design(f$coded, f$units, f$design,
                              recode_omissions_to_not_reached = TRUE,
                              use_variable_names_for_recoding = TRUE, diagnostics = "none")
  expect_equal(trusted$code_type, c("FULL_CREDIT", rep("MISSING_NOT_REACHED", 2)))
  raw <- legacy_complete_design(f$coded, f$units, f$design,
                          recode_omissions_to_not_reached = NULL, diagnostics = "none")
  trusted_positions <- eatPrepTBA:::get_design_order(f$design, f$units,
    order_method = "structure", use_variable_names_for_recoding = TRUE)
  separate <- legacy_recode_missings(raw, f$units, positions = trusted_positions,
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
  out <- legacy_complete_design(coded, f$units, design, diagnostics = "none")
  expect_equal(out$code_type, c("MISSING_BY_OMISSION", "FULL_CREDIT"))
  expect_equal(out$code_score, c(0, 1))
})

test_that("confirmed names preserve physical order and complete overrides take precedence", {
  f <- policy_fixture(c("FULL_CREDIT", "MISSING_BY_OMISSION", "MISSING_NOT_REACHED"),
                       page = c(2L, NA_integer_, 1L))
  expect_warning(safe <- legacy_complete_design(f$coded, f$units, f$design,
                               use_variable_names_for_recoding = TRUE, diagnostics = "none"),
                "Order conflict")
  expect_equal(safe$code_type, c("FULL_CREDIT", "MISSING_BY_OMISSION", "MISSING_BY_OMISSION"))
  expect_equal(safe$code_score, c(1, 0, 0))
  override <- tibble::tibble(unit_key = "U1", variable_id = c("V1", "V2", "V3"), local_order = 1:3)
  out <- legacy_complete_design(f$coded, f$units, f$design, order_overrides = override,
                          use_variable_names_for_recoding = TRUE,
                          recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  expect_equal(out$code_type, c("FULL_CREDIT", rep("MISSING_NOT_REACHED", 2)))
})

test_that("renaming ambiguous variables cannot change analytical outcomes by default", {
  f <- policy_fixture(c("FULL_CREDIT", "MISSING_BY_OMISSION", "MISSING_NOT_REACHED"))
  original <- legacy_complete_design(f$coded, f$units, f$design,
                               recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  new_ids <- c("Z", "A", "M")
  f$units$unit_codes[[1]]$variable_id <- new_ids
  f$units$unit_codes[[1]]$variable_ref <- new_ids
  f$design$variable_id <- new_ids
  f$coded$variable_id <- new_ids
  renamed <- legacy_complete_design(f$coded, f$units, f$design,
                              recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  aligned <- renamed[match(new_ids, renamed$variable_id), ]
  expect_equal(aligned$code_type, original$code_type)
  expect_equal(aligned$code_score, original$code_score)
})

test_that("completion reports actual additions once and leaves uncertain new rows unresolved", {
  f <- policy_fixture(c("FULL_CREDIT", "MISSING_BY_OMISSION"))
  first <- policy_capture(legacy_complete_design(f$coded[1, ], f$units, f$design))
  expect_equal(sum(grepl("complete_design()", first$messages, fixed = TRUE)), 1L)
  expect_match(first$text, "Added: 1 rows (0 classified; 1 still unclassified)", fixed = TRUE)
  expect_true(is.na(first$value$code_type[2]))
  expect_false(first$value$response_present[2])
  second <- policy_capture(legacy_complete_design(first$value, f$units, f$design))
  expect_equal(sum(grepl("complete_design()", second$messages, fixed = TRUE)), 1L)
  expect_match(second$text, "Added: 0 rows", fixed = TRUE)
  expect_match(second$text, "0 analytically changed; 2 unchanged", fixed = TRUE)
  expect_match(second$text, "1 lack reliable order", fixed = TRUE)
  expect_equal(second$value$response_present, first$value$response_present)
  expect_equal(second$value, first$value)
  silent <- policy_capture(legacy_complete_design(f$coded, f$units, f$design, diagnostics = "none"))
  expect_length(silent$messages, 0)
  raw <- policy_capture(legacy_complete_design(f$coded[1, ], f$units, f$design,
                                        recode_omissions_to_not_reached = NULL))
  expect_length(raw$messages, 1)
  expect_match(raw$messages, "Completion only", fixed = TRUE)
  standalone <- policy_capture(legacy_recode_missings(first$value, f$units))
  expect_equal(sum(grepl("recode_missings()", standalone$messages, fixed = TRUE)), 1L)
  expect_match(standalone$text, "recode_missings()", fixed = TRUE)
})

test_that("provided positions detect a completely removed testlet", {
  f <- policy_fixture("FULL_CREDIT")
  second <- dplyr::mutate(f$design, testlet_no = 2L, unit_booklet_no = 2L, unit_alias = "second")
  positions <- legacy_positions(dplyr::bind_rows(f$design, second), f$units)
  expect_error(legacy_recode_missings(f$coded, f$units, positions = positions, diagnostics = "none"),
                "missing unit occurrences")
})

test_that("empty completion and classification retain a usable schema and zero counts", {
  f <- policy_fixture("FULL_CREDIT")
  completed <- policy_capture(legacy_complete_design(f$coded[0, ], f$units, f$design[0, ]))
  expect_equal(nrow(completed$value), 0)
  expect_type(completed$value$variable_order, "integer")
  expect_match(completed$text, "0 analytically changed; 0 unchanged", fixed = TRUE)
  again <- legacy_recode_missings(completed$value, f$units, diagnostics = "none")
  expect_equal(again, completed$value)
})

test_that("standalone classification rejects lexical unit positions", {
  f <- policy_fixture("FULL_CREDIT")
  f$coded$unit_booklet_no <- "10"
  f$coded$variable_order <- 1L
  expect_error(legacy_recode_missings(f$coded, f$units, diagnostics = "none"), "unit_booklet_no")
})

policy_add_items <- function(fixture, ids = fixture$design$variable_id) {
  fixture$units$items_list <- list(tibble::tibble(
    variable_id = ids, item_id = paste0("I", ids), item_no = seq_along(ids)))
  fixture
}

test_that("public order defaults supplement eatPrepTBA with structure and preserve Box VOMD", {
  expect_identical(eatPrepTBA:::missing_policy_settings()$order_method, "hybrid")
  expect_identical(eatPrepTBA:::missing_policy_settings("coding_box")$order_method, "vomd")
  f <- policy_fixture(c("FULL_CREDIT", "MISSING_BY_OMISSION"), page = 1:2)
  hybrid <- complete_design(f$coded, f$units, f$design,
    recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  expect_identical(hybrid$code_type, c("FULL_CREDIT", "MISSING_NOT_REACHED"))
  expect_identical(attr(hybrid, "missing_policy")$order_method, "hybrid")
  vomd <- complete_design(f$coded, f$units, f$design, order_method = "vomd",
    recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  expect_identical(vomd$code_type, f$coded$code_type)
  f$units$unit_codes[[1]]$variable_page_always_visible <- c(NA, TRUE)
  persistent <- complete_design(f$coded, f$units, f$design,
    recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  expect_identical(persistent$code_type, f$coded$code_type)
  expect_identical(persistent$code_status, f$coded$code_status)
})

test_that("public defaults protect numerical NR while unencoded NR remains classifiable", {
  f <- policy_add_items(policy_fixture(c("MISSING_NOT_REACHED", "FULL_CREDIT")))
  protected <- complete_design(f$coded, f$units, f$design, diagnostics = "none")
  expect_identical(protected$code_type, f$coded$code_type)
  repaired <- complete_design(f$coded, f$units, f$design,
    recode_existing_not_reached = TRUE, diagnostics = "none")
  expect_identical(repaired$code_type, c("MISSING_BY_OMISSION", "FULL_CREDIT"))
  expect_equal(repaired$code_id, c(-99, 1))
  expect_identical(repaired$code_status, f$coded$code_status)
  f$coded$code_type[1] <- NA_character_
  f$coded$code_id[1] <- NA_real_
  f$coded$code_score[1] <- NA_real_
  unencoded <- complete_design(f$coded, f$units, f$design, diagnostics = "none")
  expect_identical(unencoded$code_type, c("MISSING_BY_OMISSION", "FULL_CREDIT"))
  expect_identical(unencoded$code_status, f$coded$code_status)
})

test_that("changing trailing-omission settings reuses original analytical input", {
  f <- policy_add_items(policy_fixture(c("FULL_CREDIT", "MISSING_BY_OMISSION")))
  first <- complete_design(f$coded, f$units, f$design,
    recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  expect_identical(first$code_type, c("FULL_CREDIT", "MISSING_NOT_REACHED"))
  reverted <- complete_design(first, f$units, f$design,
    recode_omissions_to_not_reached = FALSE, diagnostics = "none")
  expect_identical(reverted$code_type, f$coded$code_type)
  expect_equal(reverted$code_id, f$coded$code_id)
  expect_equal(reverted$code_score, f$coded$code_score)
  expect_identical(reverted$code_status, f$coded$code_status)
  expect_identical(reverted$value, f$coded$value)
  repeated <- complete_design(first, f$units, f$design,
    recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  expect_equal(repeated, first)
})

test_that("policy switches restore an existing valid zero-score derivation", {
  f <- policy_fixture(c("MISSING_NOT_REACHED", "FULL_CREDIT"))
  f$units$unit_codes[[1]]$variable_source_type <- c("BASE", "SUM_CODE")
  f$units$unit_codes[[1]]$variable_level <- c(0L, 1L)
  f$units$unit_codes[[1]]$derive_sources <- list(character(), "V1")
  f$units$items_list <- list(tibble::tibble(variable_id = "V2", item_id = "ID", item_no = 1L))
  f$coded$code_id[2] <- 0
  f$coded$code_score[2] <- 0
  standard <- complete_design(f$coded, f$units, f$design, diagnostics = "none")
  expect_identical(standard$code_type, rep("MISSING_NOT_REACHED", 2))
  expect_true(all(is.na(standard$code_score)))
  box <- complete_design(standard, f$units, f$design,
    missing_policy = "coding_box", diagnostics = "none")
  expect_identical(box$code_type, f$coded$code_type)
  expect_equal(box$code_id, c(-96, 0))
  expect_equal(box$code_score, c(NA_real_, 0))
  expect_identical(box$code_status, f$coded$code_status)
  expect_identical(box$value, f$coded$value)
  standard_again <- complete_design(box, f$units, f$design, diagnostics = "none")
  expect_equal(standard_again, standard)
})

test_that("custom output IDs do not discard recognizable standard input IDs", {
  f <- policy_add_items(policy_fixture(c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED")))
  f$coded$code_type <- NA_character_
  f$coded$code_status <- NA_character_
  output_profile <- tibble::tibble(code_type = "MISSING_BY_OMISSION", code_id = -199,
    code_status = "DISPLAYED", code_score = 0)
  out <- complete_design(f$coded, f$units, f$design, missings = output_profile,
    diagnostics = "none")
  expect_identical(out$code_type, c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED"))
  expect_equal(out$code_id, c(-199, -96))
  expect_true(all(is.na(out$code_status)))

  f <- policy_add_items(policy_fixture(c("MISSING_NOT_REACHED", "MISSING_INVALID_RESPONSE")))
  f$coded$code_type <- NA_character_
  f$coded$code_status <- NA_character_
  output_profile <- tibble::tibble(code_type = "MISSING_INVALID_RESPONSE", code_id = -198,
    code_status = "INVALID", code_score = 0)
  out <- complete_design(f$coded, f$units, f$design, missings = output_profile,
    recode_existing_not_reached = TRUE, diagnostics = "none")
  expect_identical(out$code_type, c("MISSING_BY_OMISSION", "MISSING_INVALID_RESPONSE"))
  expect_equal(out$code_id, c(-99, -198))
})

test_that("unit-only orphan work protects previous units without inventing within-unit order", {
  f <- policy_fixture(c("FULL_CREDIT", "MISSING_BY_OMISSION"), page = rep(NA_real_, 2))
  within <- complete_design(f$coded, f$units, f$design,
    recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  expect_identical(within$code_type, f$coded$code_type)
  expect_true(all(is.na(within$position_group)))
  earlier_design <- dplyr::mutate(f$design, unit_alias = "earlier", unit_booklet_no = 0L)
  earlier_coded <- dplyr::mutate(f$coded, unit_alias = "earlier", unit_booklet_no = 0L,
    code_type = "MISSING_BY_OMISSION", code_status = "DISPLAYED", code_id = -99,
    code_score = 0, value = NA_character_)
  across <- complete_design(dplyr::bind_rows(earlier_coded, f$coded), f$units,
    dplyr::bind_rows(earlier_design, f$design),
    recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  expect_true(all(across$code_type[across$unit_alias == "earlier"] == "MISSING_BY_OMISSION"))
})

test_that("a late overridden derivation cannot leave an earlier omission as a false work boundary", {
  f <- policy_fixture(c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED", "FULL_CREDIT"))
  f$units$unit_codes[[1]]$variable_source_type <- c("BASE", "BASE", "SUM_CODE")
  f$units$unit_codes[[1]]$variable_level <- c(0L, 0L, 1L)
  f$units$unit_codes[[1]]$derive_sources <- list(character(), character(), "V2")
  f$units$items_list <- list(tibble::tibble(
    variable_id = c("V1", "V3"), item_id = c("IX", "ID"), item_no = 1:2))
  f$coded$code_id[3] <- 0
  f$coded$code_score[3] <- 0
  out <- complete_design(f$coded, f$units, f$design,
    recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  expect_identical(out$code_type, rep("MISSING_NOT_REACHED", 3))
  expect_equal(complete_design(out, f$units, f$design,
    recode_omissions_to_not_reached = TRUE, diagnostics = "none"), out)
})

test_that("the public namespace exposes completion without exporting its internal stages", {
  exports <- getNamespaceExports("eatPrepTBA")
  expect_true("complete_design" %in% exports)
  expect_false("get_design_order" %in% exports)
  expect_false("recode_missings" %in% exports)
})

test_that("unassigned testlets and aliases still support a single expected unit occurrence", {
  f <- policy_add_items(policy_fixture(c("FULL_CREDIT", "MISSING_BY_OMISSION")))
  f$design$testlet_no <- NA_integer_
  f$design$unit_alias <- NA_character_
  f$coded$testlet_no <- NA_integer_
  f$coded$unit_alias <- NA_character_
  standard <- complete_design(f$coded, f$units, f$design,
    recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  expect_identical(standard$code_type, c("FULL_CREDIT", "MISSING_NOT_REACHED"))
  expect_true(all(is.na(standard$testlet_no)))
  expect_true(all(is.na(standard$unit_alias)))
  box <- complete_design(f$coded, f$units, f$design,
    missing_policy = "coding_box", diagnostics = "none")
  expect_identical(box$code_type, f$coded$code_type)
})

test_that("standard scope can extend from testlets to the whole booklet", {
  f <- policy_add_items(policy_fixture("MISSING_BY_OMISSION"))
  later_design <- dplyr::mutate(f$design, testlet_no = 2L,
    unit_booklet_no = 2L, unit_alias = "later")
  later_coded <- dplyr::mutate(f$coded, testlet_no = 2L,
    unit_booklet_no = 2L, unit_alias = "later", code_type = "FULL_CREDIT",
    code_status = "CODING_COMPLETE", code_id = 1, code_score = 1, value = "answer")
  coded <- dplyr::bind_rows(f$coded, later_coded)
  design <- dplyr::bind_rows(f$design, later_design)
  testlets <- complete_design(coded, f$units, design,
    recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  booklet <- complete_design(coded, f$units, design, not_reached_scope = "booklet",
    recode_omissions_to_not_reached = TRUE, diagnostics = "none")
  expect_identical(testlets$code_type, c("MISSING_NOT_REACHED", "FULL_CREDIT"))
  expect_identical(booklet$code_type, c("MISSING_BY_OMISSION", "FULL_CREDIT"))
})

test_that("standard derived protection can be selected without using the Box rule set", {
  f <- policy_fixture(c("MISSING_NOT_REACHED", "FULL_CREDIT"))
  f$units$unit_codes[[1]]$variable_source_type <- c("BASE", "SUM_CODE")
  f$units$unit_codes[[1]]$variable_level <- c(0L, 1L)
  f$units$unit_codes[[1]]$derive_sources <- list(character(), "V1")
  f$units$items_list <- list(tibble::tibble(variable_id = "V2", item_id = "ID", item_no = 1L))
  f$coded$code_id[2] <- 0
  f$coded$code_score[2] <- 0
  protected <- complete_design(f$coded, f$units, f$design,
    derived_not_reached = "preserve", diagnostics = "none")
  expect_identical(protected$code_type, f$coded$code_type)
  expect_equal(protected$code_id, f$coded$code_id)
  expect_equal(protected$code_score, f$coded$code_score)
  expect_identical(attr(protected, "missing_policy")$missing_policy, "eatPrepTBA")
})

test_that("conflicting numerical input meanings are diagnosed until an input schema is supplied", {
  f <- policy_add_items(policy_fixture(c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED")))
  f$coded$code_type <- NA_character_
  f$coded$code_status <- NA_character_
  f$coded$code_id[1] <- -98
  profile <- tibble::tibble(
    code_type = c("MISSING_INVALID_RESPONSE", "MISSING_BY_OMISSION", "MISSING_NOT_REACHED"),
    code_id = c(-198, -98, -196), code_status = c("INVALID", "DISPLAYED", "NOT_REACHED"),
    code_score = c(0, 0, NA_real_))
  unresolved <- complete_design(f$coded, f$units, f$design,
    missings = profile, diagnostics = "none")
  expect_true(is.na(unresolved$code_type[1]))
  expect_equal(unresolved$code_id[1], -98)
  expect_identical(attr(unresolved, "missing_diagnostics")$reason[1], "ambiguous-input-code")
  omission_schema <- tibble::tibble(
    code_type = c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED"),
    code_id = c(-98, -96))
  declared <- complete_design(f$coded, f$units, f$design,
    missings = profile, input_missings = omission_schema, diagnostics = "none")
  expect_identical(declared$code_type, c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED"))
  expect_equal(declared$code_id, c(-98, -196))
  invalid_schema <- tibble::tibble(
    code_type = c("MISSING_INVALID_RESPONSE", "MISSING_NOT_REACHED"),
    code_id = c(-98, -96))
  declared_invalid <- complete_design(f$coded, f$units, f$design,
    missings = profile, input_missings = invalid_schema, diagnostics = "none")
  expect_identical(declared_invalid$code_type, c("MISSING_INVALID_RESPONSE", "MISSING_NOT_REACHED"))
  expect_equal(declared_invalid$code_id, c(-198, -196))
})
