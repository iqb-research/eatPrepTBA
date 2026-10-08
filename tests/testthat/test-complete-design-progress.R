completion_progress_fixture <- function() {
  design <- tibble::tibble(
    login_code = "P1", booklet_id = "B1", booklet_no = 1L, testlet_no = 1L,
    unit_booklet_no = 1L, unit_key = "U1", unit_alias = "U1", variable_id = "V1"
  )
  list(units = minimal_units(), design = design,
       coded = dplyr::mutate(design, code_status = "CODING_COMPLETE",
                            value = "A", code_type = "FULL_CREDIT", code_id = 1,
                            code_score = 1))
}

test_that("progress preserves results and closes bars on success and failure", {
  f <- completion_progress_fixture()
  bars_before <- cli::cli_progress_num()
  for (overwrite in c(FALSE, TRUE)) {
    for (policy in c("eatPrepTBA", "coding_box")) {
      args <- c(f, list(identifiers = "login_code", diagnostics = "none",
                       missing_policy = policy, overwrite = overwrite,
                       not_reached_scope = "testlet",
                       recode_omissions_to_not_reached = TRUE))
      silent <- suppressMessages(do.call(complete_design, c(args, list(progress = FALSE))))
      visible <- suppressMessages(do.call(complete_design, c(args, list(progress = TRUE))))
      expect_identical(visible, silent)
      expect_equal(cli::cli_progress_num(), bars_before)
    }
  }
  f$design$variable_id <- "UNKNOWN"
  expect_error(suppressMessages(do.call(complete_design,
    c(f, list(progress = TRUE, unknown_variables = "error")))),
    class = "eatPrepTBA_design_metadata_error")
  expect_equal(cli::cli_progress_num(), bars_before)
})

test_that("reused unit ordering respects an override in only one booklet", {
  f <- completion_progress_fixture()
  first <- dplyr::mutate(minimal_unit_codes(), variable_page = 1L)
  second <- dplyr::mutate(first, variable_id = "V2", variable_ref = "v2",
                          variable_page = 2L)
  f$units$unit_codes[[1L]] <- dplyr::bind_rows(first, second)
  design <- dplyr::bind_rows(f$design, dplyr::mutate(f$design, booklet_id = "B2"),
                            dplyr::mutate(f$design, booklet_id = "B3"))
  overrides <- tibble::tibble(unit_key = "U1", booklet_id = "B2",
                              variable_id = c("V1", "V2"), local_order = c(2L, 1L))
  out <- get_design_order(design, f$units, order_method = "structure",
                           order_overrides = overrides)
  expect_equal(out$variable_id[out$booklet_id == "B1"], c("V1", "V2"))
  expect_equal(out$variable_id[out$booklet_id == "B2"], c("V2", "V1"))
  expect_equal(out$variable_id[out$booklet_id == "B3"], c("V1", "V2"))
  expect_equal(out$variable_order, rep(1:2, 3))
})
