preflight_fixture <- function() {
  codes <- tibble::tibble(variable_id = c("A", "B"), variable_ref = c("A", "B"),
    variable_source_type = "BASE", variable_level = 0L,
    variable_page = 1:2, variable_section = 0L, variable_element = 0L)
  units <- tibble::tibble(unit_key = "U1", unit_codes = list(codes),
    items_list = list(tibble::tibble(variable_id = c("A", "B"),
      item_id = c("IA", "IB"), item_no = 1:2)))
  design <- tibble::tibble(login_code = "P1", booklet_id = "B1", booklet_no = 1L,
    testlet_no = 1L, unit_booklet_no = 1L, unit_key = "U1", unit_alias = "first")
  coded <- dplyr::mutate(design, variable_id = "A", code_status = "CODING_COMPLETE",
    value = "answer", code_id = 1, code_score = 1, code_type = "FULL_CREDIT")
  list(coded = coded, units = units, design = design)
}

test_that("bad overrides are rejected before allocating response rows", {
  f <- preflight_fixture()
  testthat::local_mocked_bindings(
    complete_design_rows = function(...) stop("response expansion must not run"),
    .package = "eatPrepTBA")
  partial <- tibble::tibble(unit_key = "U1", variable_id = "A", local_order = 1L)
  expect_error(complete_design(f$coded, f$units, f$design,
    order_overrides = partial, diagnostics = "none", progress = FALSE),
    "full basis-variable order")
})

test_that("duplicate response keys are rejected before preparing metadata", {
  f <- preflight_fixture()
  f$coded <- dplyr::bind_rows(f$coded, dplyr::mutate(f$coded, booklet_id = "b1"))
  testthat::local_mocked_bindings(
    design_order_metadata = function(...) stop("metadata must not run"),
    .package = "eatPrepTBA")
  expect_error(complete_design(f$coded, f$units, f$design,
    diagnostics = "none", progress = FALSE), "duplicate response keys")
})

test_that("position preparation is independent of participant count", {
  f <- preflight_fixture()
  f$design <- f$design[rep(1L, 50L), ]
  f$design$login_code <- paste0("P", seq_len(50L))
  seen <- integer()
  actual <- eatPrepTBA:::get_design_order
  testthat::local_mocked_bindings(get_design_order = function(design, ...) {
    seen <<- c(seen, nrow(design))
    actual(design, ...)
  }, .package = "eatPrepTBA")
  out <- complete_design(f$coded, f$units, f$design, diagnostics = "none", progress = FALSE)
  expect_identical(seen, 1L)
  expect_equal(nrow(out), 100L)
  expect_equal(out$code_score[out$login_code == "P1" & out$variable_id == "A"], 1)
})

test_that("compact report omits only the unused unit aggregation", {
  f <- preflight_fixture()
  out <- complete_design(f$coded, f$units, f$design, diagnostics = "none", progress = FALSE)
  full <- eatPrepTBA:::missing_change_report(out, out)
  compact <- eatPrepTBA:::missing_change_report(out, out, detail = FALSE)
  expect_identical(compact$totals, full$totals)
  expect_identical(compact$transitions, full$transitions)
  expect_equal(nrow(compact$by_unit), 0L)
  expect_equal(nrow(full$by_unit), 1L)
})
