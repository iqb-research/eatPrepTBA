large_keys_fixture <- function() {
  codes <- tibble::tibble(variable_id = c("A", "B", "C"),
    variable_ref = c("A", "B", "C"), variable_source_type = "BASE",
    variable_level = 0L, variable_page = 1:3,
    variable_section = 0L, variable_element = 0L)
  units <- tibble::tibble(unit_key = "U1", unit_codes = list(codes),
    items_list = list(tibble::tibble(variable_id = c("A", "B", "C"),
      item_id = c("IA", "IB", "IC"), item_no = 1:3)))
  design <- tibble::tibble(login_code = "P1", booklet_id = "B1", booklet_no = 1L,
    testlet_no = 1L, unit_booklet_no = 1L, unit_key = "U1", unit_alias = "first")
  coded <- dplyr::mutate(design, variable_id = "A", code_status = "CODING_COMPLETE",
    value = "answer", code_id = 1, code_score = 1, code_type = "FULL_CREDIT")
  list(coded = coded, units = units, design = design)
}

large_keys_complete <- function(fixture) {
  do.call(complete_design, c(fixture, list(
    recode_omissions_to_not_reached = NULL, diagnostics = "none", progress = FALSE)))
}

test_that("unit constants and variable-only columns retain their propagation rules", {
  f <- large_keys_fixture()
  f$design <- dplyr::bind_rows(
    dplyr::mutate(f$design, variable_id = "A", note = "first"),
    dplyr::mutate(f$design, variable_id = "B", note = NA_character_))
  f$design$label <- factor("label", levels = c("unused", "label"))
  f$design$session_note <- NA_character_
  f$design$variable_note <- "variable only"
  f$design$constant_list <- rep(list(c(2, 3)), 2L)
  out <- large_keys_complete(f)
  expect_identical(out$variable_id, c("A", "B", "C"))
  expect_identical(out$note, c("first", NA_character_, NA_character_))
  expect_identical(out$label, factor(rep("label", 3L), levels = c("unused", "label")))
  expect_identical(out$session_note, rep(NA_character_, 3L))
  expect_identical(out$variable_note, c("variable only", "variable only", NA_character_))
  expect_identical(out$constant_list, rep(list(c(2, 3)), 3L))
})

test_that("duplicate design keys and normalized response keys still fail", {
  f <- large_keys_fixture()
  f$design <- dplyr::mutate(f$design, variable_id = "A")
  f$design <- dplyr::bind_rows(f$design, f$design)
  expect_error(large_keys_complete(f), "duplicate variable/occurrence keys")
  f <- large_keys_fixture()
  f$coded <- dplyr::bind_rows(f$coded, dplyr::mutate(f$coded, booklet_id = "b1"))
  expect_error(large_keys_complete(f), "duplicate response keys")
})

test_that("repeated aliases require occurrence keys only for actual matching responses", {
  f <- large_keys_fixture()
  f$design <- dplyr::bind_rows(f$design,
    dplyr::mutate(f$design, testlet_no = 2L, unit_booklet_no = 2L))
  f$coded <- dplyr::select(f$coded, -dplyr::all_of(c("testlet_no", "unit_booklet_no")))
  expect_error(large_keys_complete(f), "Repeated unit occurrences cannot be distinguished")
  f$coded <- f$coded[0, ]
  out <- large_keys_complete(f)
  expect_equal(nrow(out), 6L)
  expect_false(any(out$response_present))

  f$coded <- large_keys_fixture()$coded
  out <- large_keys_complete(f)
  expect_equal(sum(out$response_present), 1L)
  expect_identical(out$code_id[out$response_present], 1)
})

test_that("repeated booklet assignments are disambiguated by booklet_no", {
  f <- large_keys_fixture()
  f$design <- dplyr::bind_rows(f$design, dplyr::mutate(f$design, booklet_no = 2L))
  out <- large_keys_complete(f)
  expect_equal(nrow(out), 6L)
  expect_equal(sum(out$response_present), 1L)
  expect_equal(out$booklet_no[out$response_present], 1L)
  f$coded$booklet_no <- NULL
  expect_error(large_keys_complete(f), "Repeated unit occurrences cannot be distinguished")
})

test_that("missing identifiers and testlet numbers match consistently", {
  f <- large_keys_fixture()
  f$design$login_code <- f$coded$login_code <- NA_character_
  f$design$testlet_no <- f$coded$testlet_no <- NA_integer_
  out <- large_keys_complete(f)
  expect_equal(nrow(out), 3L)
  expect_true(all(is.na(out$login_code)))
  expect_true(all(is.na(out$testlet_no)))
  expect_identical(out$response_present, c(TRUE, FALSE, FALSE))
  expect_identical(out$id_used, rep(TRUE, 3L))
})

test_that("full report retains every occurrence and literal labels", {
  f <- large_keys_fixture()
  out <- large_keys_complete(f)
  out$unit_alias <- c("first{literal}", "second", NA_character_)
  out$unit_booklet_no <- 1:3
  report <- eatPrepTBA:::missing_change_report(out, out,
    reasons = c("order", "sources", NA_character_))
  messages <- character()
  withCallingHandlers(eatPrepTBA:::emit_missing_report(report, diagnostics = "full"),
    message = function(condition) {
      messages <<- c(messages, conditionMessage(condition))
      invokeRestart("muffleMessage")
    })
  expect_length(messages, 1L)
  expect_match(messages, "unit_alias=first{literal}", fixed = TRUE)
  expect_match(messages, "unit_alias=second", fixed = TRUE)
  expect_match(messages, "unit_alias=NA", fixed = TRUE)
  expect_equal(lengths(regmatches(messages, gregexpr("Among unchanged:", messages, fixed = TRUE))), 2L)
})

test_that("large full reports retain all details in one suppressible message", {
  rows <- 1001L
  input <- tibble::tibble(booklet_id = sprintf("B%04d", seq_len(rows)),
    testlet_no = 1L, unit_booklet_no = 1L, unit_key = "U{literal}",
    unit_alias = sprintf("alias_%04d", seq_len(rows)),
    login_code = "PRIVATE_PERSON", value = "PRIVATE_RESPONSE",
    code_status = "CODING_COMPLETE", code_id = 1, code_score = 1,
    code_type = "FULL_CREDIT")
  report <- eatPrepTBA:::missing_change_report(input, input,
    reasons = rep(c("order", "sources", NA_character_), length.out = rows))
  messages <- character()
  withCallingHandlers(eatPrepTBA:::emit_missing_report(report, diagnostics = "full"),
    message = function(condition) {
      messages <<- c(messages, conditionMessage(condition))
      invokeRestart("muffleMessage")
    })
  expect_length(messages, 1L)
  expect_match(messages, "1,001 output rows", fixed = TRUE)
  expect_match(messages, "counts overlap", fixed = TRUE)
  expect_match(messages, "unit_alias=alias_0001", fixed = TRUE)
  expect_match(messages, "unit_alias=alias_1001", fixed = TRUE)
  expect_equal(lengths(regmatches(messages,
    gregexpr("unit_key=U{literal}", messages, fixed = TRUE))), rows)
  expect_equal(lengths(regmatches(messages,
    gregexpr("Among unchanged:", messages, fixed = TRUE))), 668L)
  expect_false(grepl("PRIVATE_PERSON|PRIVATE_RESPONSE|login_code|value=", messages))
  expect_silent(suppressMessages(
    eatPrepTBA:::emit_missing_report(report, diagnostics = "full")))
})
