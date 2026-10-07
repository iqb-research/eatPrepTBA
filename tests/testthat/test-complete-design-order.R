legacy_positions <- function(...) eatPrepTBA:::get_design_order(..., order_method = 'structure')

legacy_complete_design <- function(..., order_method = 'structure', recode_existing_not_reached = TRUE) {
  complete_design(..., order_method = order_method, recode_existing_not_reached = recode_existing_not_reached)
}

legacy_recode_missings <- function(..., order_method = 'structure', recode_existing_not_reached = TRUE) {
  eatPrepTBA:::recode_missings(..., order_method = order_method, recode_existing_not_reached = recode_existing_not_reached)
}

complete_order_units <- function(derived = FALSE) {
  ids <- if (derived) c("01a", "01b", "01", "02a", "02b", "02") else sprintf("%02d", 1:4)
  refs <- paste0("ref_", ids)
  sources <- lapply(ids, function(id) {
    src <- if (id %in% c("01", "02") && derived) paste0(id, c("a", "b")) else character()
    tibble::tibble(
      variable_source_id = src,
      variable_source_ref = if (length(src)) paste0("ref_", src) else character(),
      variable_source_level = rep(0L, length(src)),
      variable_source_direct = rep(TRUE, length(src))
    )
  })
  # Keep the source refs distinct from aliases: the real source tree uses refs.
  codes <- tibble::tibble(
    variable_id = ids,
    variable_ref = refs,
    variable_source_type = ifelse(derived & ids %in% c("01", "02"), "SUM", "BASE"),
    variable_level = ifelse(derived & ids %in% c("01", "02"), 1L, 0L),
    variable_page = if (derived) c(1L, 1L, 1L, 2L, 2L, 2L) else c(1L, 1L, 2L, 2L),
    variable_section = 1L,
    variable_element = if (derived) c(1L, 2L, NA_integer_, 1L, 2L, NA_integer_) else c(1L, 2L, 1L, 2L),
    variable_page_always_visible = FALSE,
    variable_sources = sources
  )
  items <- tibble::tibble(
    variable_id = if (derived) c("01", "02") else c("01", "03", "04"),
    item_id = if (derived) c("I01", "I02") else c("I01", "I03", "I04")
  )
  tibble::tibble(unit_key = "U1", unit_codes = list(codes), items_list = list(items))
}

complete_order_design <- function(units, persons = "P1", repeats = FALSE) {
  occurrences <- tibble::tibble(
    unit_alias = if (repeats) c("first", "second") else "first",
    unit_booklet_no = if (repeats) 1:2 else 1L
  )
  tidyr::crossing(
    login_code = persons,
    occurrences,
    variable_id = units$unit_codes[[1]]$variable_id
  ) %>%
    dplyr::mutate(
      group_id = "G1", login_name = paste0("L", login_code),
      booklet_id = "B1", booklet_no = 1L, testlet_no = 1L, unit_key = "U1"
    )
}

complete_order_coded <- function(design, types, statuses = NULL) {
  if (is.null(statuses)) {
    status_lookup <- c(
      FULL_CREDIT = "CODING_COMPLETE", MISSING_BY_OMISSION = "DISPLAYED",
      MISSING_NOT_REACHED = "NOT_REACHED", MISSING_INVALID_RESPONSE = "INVALID"
    )
    statuses <- unname(status_lookup[types])
  }
  id_lookup <- c(FULL_CREDIT = 1L, MISSING_BY_OMISSION = -99L,
                  MISSING_NOT_REACHED = -96L, MISSING_INVALID_RESPONSE = -98L)
  score_lookup <- c(FULL_CREDIT = 1, MISSING_BY_OMISSION = 0,
                     MISSING_NOT_REACHED = NA_real_, MISSING_INVALID_RESPONSE = 0)
  design %>%
    dplyr::select(group_id, login_name, login_code, booklet_id, unit_key, unit_alias, variable_id) %>%
    dplyr::mutate(
      code_type = types,
      code_status = statuses,
      code_id = unname(id_lookup[types]),
      code_score = unname(score_lookup[types]),
      value = ifelse(types == "FULL_CREDIT", "A", NA_character_)
    )
}

complete_order_compare <- function(data) {
  data %>%
    dplyr::arrange(login_code, booklet_id, testlet_no, unit_booklet_no, variable_id) %>%
    dplyr::select(login_code, booklet_id, testlet_no, unit_booklet_no, unit_alias,
                  unit_key, variable_id, code_status, code_type, code_id, code_score,
                  value, variable_order, item_order, order_group, order_source)
}

complete_order_attach <- function(data, positions) {
  keys <- c("booklet_id", "testlet_no", "unit_booklet_no", "unit_key", "unit_alias", "variable_id")
  positions <- positions %>%
    dplyr::select(dplyr::all_of(keys), variable_order, item_order, order_group, order_source)
  dplyr::left_join(data, positions, by = keys, relationship = "many-to-one")
}

test_that("NULL completes rows while preserving raw code fields and missing statuses", {
  units <- complete_order_units()
  design <- complete_order_design(units, persons = c("P1", "P2"))
  supplied <- design %>% dplyr::filter(login_code == "P1", variable_id %in% c("01", "02"))
  coded <- complete_order_coded(supplied, c("FULL_CREDIT", NA_character_), statuses = c("CODING_COMPLETE", NA_character_))
  coded$code_id[[2]] <- 78L
  coded$code_score[[2]] <- 0.25
  coded$value[[2]] <- "raw retained value"

  out <- legacy_complete_design(coded, units, design, recode_omissions_to_not_reached = NULL)
  expect_equal(nrow(out), nrow(design))
  expect_false(any(c("variable_order", "item_order", "order_group", "order_source") %in% names(out)))
  observed <- out %>% dplyr::filter(login_code == "P1", variable_id %in% c("01", "02")) %>% dplyr::arrange(variable_id)
  expect_equal(observed %>% dplyr::select(code_status, code_type, code_id, code_score, value),
               coded %>% dplyr::select(code_status, code_type, code_id, code_score, value))
  added <- out %>% dplyr::filter(login_code == "P2" | variable_id %in% c("03", "04"))
  expect_true(all(is.na(added$code_status)))
  expect_true(all(is.na(added$code_type)))
  expect_true(all(is.na(added$code_id)))
  expect_true(all(is.na(added$code_score)))
  expect_true(all(out$id_used[out$login_code == "P1"]))
  expect_true(all(!out$id_used[out$login_code == "P2"]))
  expect_equal(sum(out$response_present), 2L)
  expect_true(all(observed$response_present))
  expect_true(all(!added$response_present))
})

test_that("missing classification uses variable order inside a boundary unit", {
  units <- complete_order_units()
  design <- complete_order_design(units)
  coded <- complete_order_coded(design, c("FULL_CREDIT", "MISSING_NOT_REACHED", "FULL_CREDIT", "MISSING_NOT_REACHED"))
  false <- legacy_complete_design(coded, units, design)
  expect_equal(false$code_type, c("FULL_CREDIT", "MISSING_BY_OMISSION", "FULL_CREDIT", "MISSING_NOT_REACHED"))
  expect_equal(false$code_id, c(1, -99, 1, -96))
  expect_equal(false$code_score, c(1, 0, 1, NA_real_))
  expect_equal(false$code_status, coded$code_status)

  coded <- complete_order_coded(design, c("FULL_CREDIT", "MISSING_BY_OMISSION", "FULL_CREDIT", "MISSING_BY_OMISSION"))
  false <- legacy_complete_design(coded, units, design)
  true <- legacy_complete_design(coded, units, design, recode_omissions_to_not_reached = TRUE)
  expect_equal(false$code_type, coded$code_type)
  expect_equal(true$code_type, c("FULL_CREDIT", "MISSING_BY_OMISSION", "FULL_CREDIT", "MISSING_NOT_REACHED"))
  expect_equal(true$code_id, c(1, -99, 1, -96))
  expect_equal(true$code_score, c(1, 0, 1, NA_real_))
  expect_equal(true$code_status, coded$code_status)
})

test_that("nonnegative codes remain valid evidence when type and value are unavailable", {
  units <- complete_order_units()
  design <- complete_order_design(units)
  coded <- complete_order_coded(design,
    c("MISSING_BY_OMISSION", "FULL_CREDIT", "MISSING_NOT_REACHED", "MISSING_NOT_REACHED"))
  coded$code_type[coded$variable_id == "02"] <- NA_character_
  coded$value[coded$variable_id == "02"] <- NA_character_
  for (mode in c(FALSE, TRUE)) {
    out <- legacy_complete_design(coded, units, design, recode_omissions_to_not_reached = mode)
    valid <- out %>% dplyr::filter(variable_id == "02")
    expect_equal(valid$code_id, 1)
    expect_equal(valid$code_score, 1)
    expect_true(is.na(valid$code_type))
    expect_true(is.na(valid$value))
    expect_equal(valid$code_status, "CODING_COMPLETE")
    expect_equal(out$code_type[out$variable_id == "01"], "MISSING_BY_OMISSION")
  }
})

test_that("NULL preserves existing variable positions without copying them to new rows", {
  units <- complete_order_units()
  design <- complete_order_design(units) %>%
    dplyr::filter(variable_id == "02") %>%
    dplyr::mutate(variable_order = 12L, item_order = 3L, item_id = "manual_item",
                  order_group = 4L, order_source = "manual")
  coded <- complete_order_coded(design, "FULL_CREDIT")
  out <- legacy_complete_design(coded, units, design, recode_omissions_to_not_reached = NULL)
  supplied <- out %>% dplyr::filter(variable_id == "02")
  added <- out %>% dplyr::filter(variable_id != "02")
  expect_equal(supplied$variable_order, 12L)
  expect_equal(supplied$item_order, 3L)
  expect_equal(supplied$item_id, "manual_item")
  expect_equal(supplied$order_group, 4L)
  expect_equal(supplied$order_source, "manual")
  expect_true(all(is.na(added$variable_order)))
  expect_true(all(is.na(added$item_order)))
  expect_true(all(is.na(added$item_id)))
  expect_true(all(is.na(added$order_group)))
  expect_true(all(is.na(added$order_source)))
  expect_true(all(!added$response_present))
})

test_that("classification preserves NA code_status and uses only original statuses for id_used", {
  units <- complete_order_units()
  design <- complete_order_design(units)
  coded <- complete_order_coded(design[1, ], "FULL_CREDIT", statuses = NA_character_)
  for (mode in list(NULL, FALSE, TRUE)) {
    out <- legacy_complete_design(coded, units, design, recode_omissions_to_not_reached = mode)
    expect_true(all(is.na(out$code_status)))
    expect_true(all(!out$id_used))
    expect_equal(out$code_type[out$variable_id == "01"], "FULL_CREDIT")
    expect_equal(out$code_score[out$variable_id == "01"], 1)
  }
})

test_that("not-reached boundaries reset at each testlet", {
  units <- complete_order_units()
  design <- complete_order_design(units, repeats = TRUE) %>%
    dplyr::mutate(testlet_no = unit_booklet_no)
  coded <- complete_order_coded(design, ifelse(design$testlet_no == 1L, "MISSING_BY_OMISSION", "FULL_CREDIT"))
  out <- legacy_complete_design(coded, units, design, recode_omissions_to_not_reached = TRUE)
  expect_true(all(out$code_type[out$testlet_no == 1L] == "MISSING_NOT_REACHED"))
  expect_true(all(out$code_type[out$testlet_no == 2L] == "FULL_CREDIT"))
  expect_equal(out$code_status, coded$code_status)
})

test_that("derived Invalid recoding automatically follows a proven basis-only boundary", {
  units <- complete_order_units(derived = TRUE)
  design <- complete_order_design(units)
  types <- c("MISSING_INVALID_RESPONSE", "FULL_CREDIT", "MISSING_BY_OMISSION",
             "MISSING_INVALID_RESPONSE", "MISSING_BY_OMISSION", "MISSING_NOT_REACHED")
  # crossing() orders aliases: derived 01 precedes 01a, and derived 02 precedes 02a.
  coded <- complete_order_coded(design, types)
  false <- legacy_complete_design(coded, units, design)
  false_derived <- false %>% dplyr::filter(variable_id %in% c("01", "02")) %>% dplyr::arrange(variable_id)
  original_derived <- coded %>% dplyr::filter(variable_id %in% c("01", "02")) %>% dplyr::arrange(variable_id)
  fields <- c("code_status", "code_type", "code_id", "code_score")
  expect_equal(lapply(fields, function(column) false_derived[[column]]),
               lapply(fields, function(column) original_derived[[column]]))

  true <- legacy_complete_design(coded, units, design, recode_omissions_to_not_reached = TRUE)
  earlier <- true %>% dplyr::filter(variable_id == "01")
  later <- true %>% dplyr::filter(variable_id == "02")
  expect_equal(earlier$code_type, "MISSING_INVALID_RESPONSE")
  expect_equal(earlier$code_score, 0)
  expect_equal(later$code_type, "MISSING_NOT_REACHED")
  expect_true(is.na(later$code_score))
  expect_equal(later$code_id, -96)
  expect_equal(later$code_status, "INVALID")

  # A synthetic valid derived value cannot move the frontier; all-NR sources correct it.
  valid <- complete_order_coded(design, ifelse(design$variable_id == "02", "FULL_CREDIT", "MISSING_BY_OMISSION"))
  out <- legacy_complete_design(valid, units, design, recode_omissions_to_not_reached = TRUE)
  expect_true(all(out$code_type[out$variable_source_type == "BASE"] == "MISSING_NOT_REACHED"))
  expect_equal(out$code_type[out$variable_id == "02"], "MISSING_NOT_REACHED")
  expect_equal(out$code_id[out$variable_id == "02"], -96)
  expect_true(is.na(out$code_score[out$variable_id == "02"]))
  expect_equal(out$code_status[out$variable_id == "02"], "CODING_COMPLETE")
  expect_equal(out$code_type[out$variable_id == "01"], "MISSING_NOT_REACHED")
  expect_equal(out$code_id[out$variable_id == "01"], -96)
  expect_equal(out$code_status[out$variable_id == "01"], "DISPLAYED")
  expect_true(is.na(out$code_score[out$variable_id == "01"]))

  # Missing sources before later physical work must not trigger Invalid -> NR.
  earlier_missing <- complete_order_coded(design,
    ifelse(design$variable_id == "02b", "FULL_CREDIT",
      ifelse(design$variable_id %in% c("01", "02"), "MISSING_INVALID_RESPONSE", "MISSING_BY_OMISSION")))
  out <- legacy_complete_design(earlier_missing, units, design, recode_omissions_to_not_reached = TRUE)
  expect_equal(out$code_type[out$variable_id == "01"], "MISSING_INVALID_RESPONSE")
  expect_equal(out$code_score[out$variable_id == "01"], 0)
})

test_that("valid and invalid derived corrections follow transitive sources separately per person", {
  units <- complete_order_units(derived = TRUE)
  codes <- units$unit_codes[[1]]
  total <- codes[codes$variable_id == "02", ]
  total$variable_id <- "TOTAL"
  total$variable_ref <- "ref_TOTAL"
  total$variable_level <- 2L
  total$variable_sources <- list(tibble::tibble(
    variable_source_id = c("01", "02"),
    variable_source_ref = c("ref_01", "ref_02"),
    variable_source_level = 1L, variable_source_direct = TRUE
  ))
  units$unit_codes[[1]] <- dplyr::bind_rows(codes, total)
  units$items_list[[1]] <- dplyr::bind_rows(units$items_list[[1]],
    tibble::tibble(variable_id = "TOTAL", item_id = "ITOTAL"))
  design <- complete_order_design(units, persons = c("P1", "P2"))
  derived <- design$variable_id %in% c("01", "02", "TOTAL")
  types <- ifelse(derived | (design$login_code == "P2" & design$variable_id == "01a"),
                   "FULL_CREDIT", "MISSING_NOT_REACHED")
  coded <- complete_order_coded(design, types)
  coded$code_score[derived] <- 0
  total_rows <- coded$variable_id == "TOTAL"
  coded$code_type[total_rows] <- "MISSING_INVALID_RESPONSE"
  coded$code_status[total_rows] <- "INVALID"
  coded$code_id[total_rows] <- -98
  expect_message(out <- legacy_complete_design(coded, units, design),
                  "MISSING_INVALID_RESPONSE -> MISSING_NOT_REACHED (derived): 1", fixed = TRUE)
  p1 <- dplyr::filter(out, login_code == "P1")
  expect_true(all(p1$code_type == "MISSING_NOT_REACHED"))
  expect_true(all(is.na(p1$code_score)))
  p2 <- dplyr::filter(out, login_code == "P2", variable_id %in% c("01", "02", "TOTAL")) %>%
    dplyr::arrange(variable_id)
  # TOTAL has a genuinely worked source and remains Invalid. Its later item
  # position anchors work, so 02's missing sources are not proven trailing.
  expect_equal(p2$code_type, c("FULL_CREDIT", "FULL_CREDIT", "MISSING_INVALID_RESPONSE"))
  expect_equal(p2$code_score, c(0, 0, 0))
  expect_true(all(out$code_status[out$variable_id %in% c("01", "02")] == "CODING_COMPLETE"))
  expect_true(all(out$code_status[out$variable_id == "TOTAL"] == "INVALID"))

  expect_message(repeated <- legacy_complete_design(out, units, design),
                  "0 analytically changed", fixed = TRUE)
  expect_identical(repeated, out)
  expect_identical(legacy_recode_missings(out, units, diagnostics = "none"), out)
  completed_only <- legacy_complete_design(coded, units, design,
                                     recode_omissions_to_not_reached = NULL, diagnostics = "none")
  expect_true(all(completed_only$code_type[completed_only$variable_id == "TOTAL"] == "MISSING_INVALID_RESPONSE"))
  expect_true(all(completed_only$code_type[completed_only$variable_id %in% c("01", "02")] == "FULL_CREDIT"))
})

test_that("repeated classification preserves consistent recoded derived Invalid fields", {
  units <- complete_order_units(derived = TRUE)
  design <- complete_order_design(units)
  types <- ifelse(design$variable_id %in% c("01", "02"),
                  "MISSING_INVALID_RESPONSE", "MISSING_BY_OMISSION")
  for (status in c("INVALID", NA_character_)) {
    coded <- complete_order_coded(design, types)
    coded$code_status[coded$variable_id %in% c("01", "02")] <- status
    first <- legacy_complete_design(coded, units, design, recode_omissions_to_not_reached = TRUE)
    second <- legacy_recode_missings(first, units, recode_omissions_to_not_reached = TRUE)
    integrated_second <- legacy_complete_design(first, units, design, recode_omissions_to_not_reached = TRUE)
    derived <- first %>% dplyr::filter(variable_id %in% c("01", "02"))
    expect_true(all(derived$code_type == "MISSING_NOT_REACHED"))
    expect_true(all(derived$code_id == -96))
    expect_true(all(is.na(derived$code_score)))
    expect_equal(derived$code_status, rep(status, 2L))
    expect_equal(complete_order_compare(second), complete_order_compare(first))
    expect_equal(complete_order_compare(integrated_second), complete_order_compare(first))
    expect_equal(integrated_second$response_present, first$response_present)
  }
})

test_that("positions depend on booklet occurrences rather than persons or observed rows", {
  units <- complete_order_units()
  design <- complete_order_design(units, persons = c("P1", "P2"), repeats = TRUE) %>%
    dplyr::mutate(booklet_no = ifelse(login_code == "P1", 1L, 2L))
  coded <- complete_order_coded(design %>% dplyr::filter(login_code == "P1", unit_booklet_no == 1L, variable_id == "03"), "FULL_CREDIT")
  out <- legacy_complete_design(coded, units, design)
  p1 <- out %>% dplyr::filter(login_code == "P1") %>% dplyr::arrange(variable_order)
  p2 <- out %>% dplyr::filter(login_code == "P2") %>% dplyr::arrange(variable_order)
  expect_equal(p1$variable_order, 1:8)
  expect_equal(p2$variable_order, p1$variable_order)
  expect_equal(p2$item_order, p1$item_order)
  expect_equal(p2$order_group, p1$order_group)
  expect_equal(p1$unit_booklet_no, rep(1:2, each = 4L))
  expect_equal(p1$unit_alias, rep(c("first", "second"), each = 4L))
})

test_that("supplied empty derived results retain provenance while missing sources classify them", {
  units <- complete_order_units(derived = TRUE)
  design <- complete_order_design(units)
  types <- ifelse(design$variable_id %in% c("01", "02"), NA_character_, "MISSING_BY_OMISSION")
  coded <- complete_order_coded(design, types)
  for (mode in c(FALSE, TRUE)) {
    out <- legacy_complete_design(coded, units, design, recode_omissions_to_not_reached = mode)
    derived <- out %>% dplyr::filter(variable_id %in% c("01", "02"))
    expect_true(all(derived$response_present))
    expect_true(all(derived$code_type == if (mode) "MISSING_NOT_REACHED" else "MISSING_BY_OMISSION"))
    expect_true(all(derived$code_id == if (mode) -96 else -99))
    if (mode) expect_true(all(is.na(derived$code_score))) else expect_true(all(derived$code_score == 0))
    expect_true(all(is.na(derived$code_type_input)))
    expect_true(all(is.na(derived$code_status)))
    raw <- legacy_complete_design(coded, units, design, recode_omissions_to_not_reached = NULL)
    positions <- legacy_positions(design, units)
    separate <- legacy_recode_missings(complete_order_attach(raw, positions), units, positions = positions,
                                recode_omissions_to_not_reached = mode)
    expect_equal(complete_order_compare(out), complete_order_compare(separate))
  }
})

test_that("integrated and separate operations agree and are invariant to input row order", {
  units <- complete_order_units(derived = TRUE)
  design <- complete_order_design(units, persons = c("P1", "P2"))
  types <- ifelse(design$variable_id == "01a", "FULL_CREDIT", "MISSING_BY_OMISSION")
  types[design$variable_id == "02"] <- "MISSING_INVALID_RESPONSE"
  coded <- complete_order_coded(design, types)
  for (mode in c(FALSE, TRUE)) {
    direct <- legacy_complete_design(coded, units, design, recode_omissions_to_not_reached = mode)
    raw <- legacy_complete_design(coded, units, design, recode_omissions_to_not_reached = NULL)
    positions <- legacy_positions(design, units)
    separate <- legacy_recode_missings(complete_order_attach(raw, positions), units,
                                recode_omissions_to_not_reached = mode)
    expect_equal(complete_order_compare(direct), complete_order_compare(separate))
    shuffled_units <- units
    shuffled_units$unit_codes[[1]] <- units$unit_codes[[1]][nrow(units$unit_codes[[1]]):1L, ]
    shuffled <- legacy_complete_design(coded[nrow(coded):1L, ], shuffled_units,
                                design[nrow(design):1L, ], recode_omissions_to_not_reached = mode)
    expect_equal(complete_order_compare(direct), complete_order_compare(shuffled))
  }
})

test_that("manual order and selected items leave non-item evidence available", {
  units <- complete_order_units()
  design <- complete_order_design(units)
  coded <- complete_order_coded(design, c("MISSING_BY_OMISSION", "FULL_CREDIT", "MISSING_BY_OMISSION", "MISSING_NOT_REACHED"))
  selection <- tibble::tibble(unit_key = "U1", variable_id = "01")
  out <- legacy_complete_design(coded, units, design, recode_omissions_to_not_reached = TRUE,
                         item_selection = selection)
  expect_equal(out$code_type[out$variable_id == "01"], "MISSING_BY_OMISSION")
  expect_equal(out$item_order[out$variable_id == "01"], 1L)
  expect_true(all(is.na(out$item_order[out$variable_id != "01"])))
  expect_equal(nrow(out), 4L)

  override <- tibble::tibble(unit_key = "U1", variable_id = c("02", "01", "03", "04"), local_order = 1:4)
  overridden <- legacy_complete_design(coded, units, design, recode_omissions_to_not_reached = TRUE,
                                order_overrides = override, item_selection = selection)
  expect_equal(overridden$variable_id[order(overridden$variable_order)], c("02", "01", "03", "04"))
  expect_equal(overridden$code_type[overridden$variable_id == "01"], "MISSING_NOT_REACHED")
  expect_equal(overridden$item_order[overridden$variable_id == "01"], 1L)
})

test_that("item-filtered and unit-level designs are expanded before missing classification", {
  units <- complete_order_units(derived = TRUE)
  design <- complete_order_design(units)
  coded <- complete_order_coded(design, ifelse(design$variable_id == "02b", "FULL_CREDIT", "MISSING_NOT_REACHED"))
  full <- legacy_complete_design(coded, units, design)
  items_only <- legacy_complete_design(coded, units, design %>% dplyr::filter(variable_id %in% c("01", "02")))
  unit_only <- legacy_complete_design(coded, units, design %>% dplyr::distinct(dplyr::across(-variable_id)))
  expect_equal(complete_order_compare(full), complete_order_compare(items_only))
  expect_equal(complete_order_compare(full), complete_order_compare(unit_only))
  expect_equal(full$code_type[full$variable_id == "02a"], "MISSING_BY_OMISSION")
})
