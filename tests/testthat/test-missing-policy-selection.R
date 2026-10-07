test_that("item selection keeps the full VOMD activity universe in both policies", {
  units <- tibble::tibble(unit_key = "U", unit_codes = list(tibble::tibble(
    variable_id = c("A", "B"), variable_ref = c("A", "B"),
    variable_source_type = "BASE", variable_level = 0L
  )), items_list = list(tibble::tibble(variable_id = c("A", "B"),
    item_id = c("IA", "IB"), item_no = 1:2)))
  design <- tibble::tibble(login_code = "P", booklet_id = "BK", booklet_no = 1L,
    testlet_no = 1L, unit_booklet_no = 1L, unit_key = "U", unit_alias = "U",
    variable_id = c("A", "B"))
  coded <- dplyr::mutate(design, code_type = c("MISSING_BY_OMISSION", "FULL_CREDIT"),
    code_status = c("DISPLAYED", "CODING_COMPLETE"), code_id = c(-99, 1),
    code_score = c(0, 1), value = c(NA_character_, "answer"))
  for (policy in c("eatPrepTBA", "coding_box")) {
    full <- complete_design(coded, units, design, missing_policy = policy,
      not_reached_scope = "testlet", recode_omissions_to_not_reached = TRUE,
      diagnostics = "none")
    for (selection in list(tibble::tibble(unit_key = "U", variable_id = "A"),
                           tibble::tibble(unit_key = character(), variable_id = character()))) {
      selected <- complete_design(coded, units, design, missing_policy = policy,
        not_reached_scope = "testlet", recode_omissions_to_not_reached = TRUE,
        item_selection = selection, diagnostics = "none")
      expect_equal(nrow(selected), 2L)
      expect_identical(selected$code_type, c("MISSING_BY_OMISSION", "FULL_CREDIT"))
      expect_identical(selected$code_id, full$code_id)
      expect_identical(selected$code_score, full$code_score)
      expect_identical(selected$code_status, coded$code_status)
      expect_true(all(is.na(selected$item_order[!selected$variable_id %in% selection$variable_id])))
    }
    factored <- dplyr::mutate(coded, code_type = factor(rev(code_type)),
      code_status = factor(rev(code_status)), code_id = rev(code_id),
      code_score = rev(code_score), value = rev(value))
    normalized <- complete_design(factored, units, design, missing_policy = policy,
      not_reached_scope = "testlet", recode_omissions_to_not_reached = TRUE,
      diagnostics = "none")
    expect_identical(normalized$code_type, c("FULL_CREDIT", "MISSING_NOT_REACHED"))
    expect_identical(normalized$code_status, factored$code_status)
    expect_identical(normalized$code_type_input, factored$code_type)
  }
})
