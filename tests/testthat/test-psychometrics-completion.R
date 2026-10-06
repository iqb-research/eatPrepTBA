psychometrics_completion_fixture <- function() {
  units <- minimal_units()
  second_codes <- minimal_unit_codes()
  second_codes$variable_id <- "V2"
  second_codes$variable_ref <- "v2"
  second_codes$variable_label <- "Variable 2"
  units$unit_codes[[1L]] <- dplyr::bind_rows(units$unit_codes[[1L]], second_codes)
  units$unit_variables[[1L]] <- dplyr::bind_rows(
    units$unit_variables[[1L]],
    dplyr::mutate(units$unit_variables[[1L]], variable_id = "V2", variable_ref = "v2")
  )
  # Only V1 is an item according to units. V2 has the opposite response pattern.
  coded <- tibble::tibble(
    group_id = "G1", login_name = rep(paste0("L", 1:4), each = 2L),
    login_code = rep(paste0("C", 1:4), each = 2L),
    booklet_id = "B1", unit_key = "U1", unit_alias = "U1",
    variable_id = rep(c("V1", "V2"), 4L),
    code_status = "CODING_COMPLETE", code_id = c(1L, 0L, 1L, 0L, 0L, 1L, 0L, 1L)
  ) %>%
    dplyr::mutate(
      code_score = as.numeric(code_id),
      code_type = ifelse(code_id == 1L, "FULL_CREDIT", "RESIDUAL"),
      value = ifelse(code_id == 1L, "A", "B")
    )
  design <- coded %>%
    dplyr::select(group_id, login_name, login_code, booklet_id, unit_key,
                  unit_alias, variable_id) %>%
    dplyr::mutate(booklet_no = 1L, testlet_no = 1L, unit_booklet_no = 1L)
  list(units = units, coded = coded, design = design,
       domains = tibble::tibble(domain = "D1", unit_key = "U1"))
}

test_that("completed responses feed psychometrics with existing item identifiers", {
  fixture <- psychometrics_completion_fixture()
  completed <- complete_design(fixture$coded, fixture$units, fixture$design)
  expect_true("item_id" %in% names(completed))

  result <- evaluate_psychometrics(completed, fixture$units, domains = fixture$domains)
  expect_equal(
    result$code_pbc[result$variable_id == "V2" & result$code_id == 1L],
    -1
  )
  expect_equal(
    result$code_pbc[result$variable_id == "V1" & result$code_id == 1L],
    1
  )
})

test_that("psychometric item membership always comes from unit metadata", {
  fixture <- psychometrics_completion_fixture()
  completed <- complete_design(fixture$coded, fixture$units, fixture$design)
  without_ids <- dplyr::select(completed, -item_id)
  expected <- evaluate_psychometrics(without_ids, fixture$units,
                                     domains = fixture$domains)
  variants <- list(
    present = completed,
    missing = dplyr::mutate(completed, item_id = NA_character_),
    conflicting = dplyr::mutate(completed,
                                item_id = ifelse(variable_id == "V2", "OTHER", NA_character_))
  )
  for (variant in names(variants)) {
    actual <- evaluate_psychometrics(variants[[variant]], fixture$units,
                                     domains = fixture$domains)
    expect_equal(actual, expected, info = variant)
  }
})
