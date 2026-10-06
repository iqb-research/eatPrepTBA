missing_report_fixture <- function() {
  before <- tibble::tibble(
    booklet_id = "B1", testlet_no = 1L, unit_booklet_no = 1L,
    unit_key = "U1", unit_alias = "U1", login_name = "PRIVATE_PERSON",
    code_type = c(NA, "MISSING_NOT_REACHED", "MISSING_BY_OMISSION",
                  "MISSING_INVALID_RESPONSE", "MISSING_BY_OMISSION",
                  NA, NA, "FULL_CREDIT", NA, "MISSING_BY_OMISSION"),
    code_id = c(NA, -96, -99, -198, -99, NA, NA, 1, NA, -99),
    code_score = c(NA, NA, 0, 0.75, 0, NA, NA, 1, NA, 0),
    code_status = rep(NA_character_, 10),
    response_present = c(rep(TRUE, 5), FALSE, FALSE, TRUE, FALSE, TRUE)
  )
  after <- before
  after$code_type[c(1, 2, 3, 4, 6)] <- c("MISSING_BY_OMISSION", "MISSING_BY_OMISSION",
    "MISSING_NOT_REACHED", "MISSING_NOT_REACHED", "MISSING_NOT_REACHED")
  after$code_id[c(1, 2, 3, 4, 6)] <- c(-99, -99, -96, -196, -96)
  after$code_score[c(1, 2, 3, 4)] <- c(0, 0, NA, NA)
  after$code_id[5] <- -199
  after$code_score[10] <- NA_real_
  list(before = before, after = after,
       added = seq_len(10) %in% c(6, 7),
       reasons = c(NA, "order", rep(NA_character_, 4), "sources", NA, "order", NA),
       basis = seq_len(10) != 4)
}

capture_missing_report <- function(...) {
  messages <- character()
  withCallingHandlers(emit_missing_report(...), message = function(condition) {
    messages <<- c(messages, conditionMessage(condition))
    invokeRestart("muffleMessage")
  })
  messages
}

test_that("reports partition current additions and net changes without counting persistent origins", {
  f <- missing_report_fixture()
  report <- do.call(missing_change_report, f)
  n <- report$totals
  expect_equal(c(n$n_rows, n$n_added, n$n_existing, n$n_changed, n$n_unchanged),
               c(10, 2, 8, 6, 2))
  expect_equal(c(n$n_added_classified, n$n_added_unresolved), c(1, 1))
  expect_equal(c(n$n_first_classified, n$n_reclassified, n$n_only_id_or_score), c(1, 3, 2))
  expect_equal(n$n_first_classified + n$n_reclassified + n$n_only_id_or_score, n$n_changed)
  expect_equal(n$n_derived_invalid_to_not_reached, 1)
  expect_equal(c(n$n_type_changed, n$n_id_changed, n$n_score_changed), c(4, 5, 5))
  expect_equal(c(n$n_unchanged_order, n$n_unchanged_sources), c(1, 0))
  expect_equal(sum(report$transitions$n), n$n_type_changed)
  expect_equal(report$transitions$n[report$transitions$change == "derived_invalid_to_not_reached"], 1)
  expect_equal(sum(report$by_unit$n_changed), n$n_changed)
  expect_false("login_name" %in% names(report$by_unit))
})

test_that("repeated unchanged data reports zero changes including equal missing values", {
  f <- missing_report_fixture()
  report <- missing_change_report(f$after, f$after)
  expect_equal(report$totals$n_added, 0)
  expect_equal(report$totals$n_changed, 0)
  expect_equal(report$totals$n_unchanged, 10)
  expect_equal(report$totals$n_status_changed, 0)
  expect_equal(nrow(report$transitions), 0)
  expect_equal(report$totals$n_only_id_or_score, 0)
})

test_that("technical changes are independent of analytical changes", {
  f <- missing_report_fixture()
  after <- f$before
  after$code_status[1] <- "DISPLAYED"
  report <- missing_change_report(f$before, after)
  expect_equal(report$totals$n_changed, 0)
  expect_equal(report$totals$n_status_changed, 1)
  expect_equal(report$totals$n_unchanged, 10)
})

test_that("full reports group by occurrences and never print person identifiers", {
  f <- missing_report_fixture()
  f$after$unit_booklet_no[8:10] <- 2L
  f$after$unit_alias[8:10] <- "SECOND"
  report <- do.call(missing_change_report, f)
  expect_equal(nrow(report$by_unit), 2)
  expect_equal(sum(report$by_unit$n_rows), 10)
  output <- capture_missing_report(report, diagnostics = "full", source = "complete_design")
  expect_length(output, 1)
  expect_match(output, "complete_design()", fixed = TRUE)
  expect_match(output, "counts overlap", fixed = TRUE)
  expect_match(output, "unit_alias=SECOND", fixed = TRUE)
  expect_match(output, "Previously unclassified -> MISSING_BY_OMISSION", fixed = TRUE)
  expect_false(grepl("PRIVATE_PERSON|login_name", output))
})

test_that("compact reports provide disjoint row counts and actual transitions in one block", {
  report <- do.call(missing_change_report, missing_report_fixture())
  output <- capture_missing_report(report)
  expect_length(output, 1)
  expect_match(output, "Added: 2 rows (1 classified; 1 still unclassified)", fixed = TRUE)
  expect_match(output, "Existing rows: 6 analytically changed; 2 unchanged", fixed = TRUE)
  expect_match(output, "MISSING_INVALID_RESPONSE -> MISSING_NOT_REACHED (derived): 1", fixed = TRUE)
  expect_match(output, "Only code ID or score changed: 2 further existing rows", fixed = TRUE)
  expect_false(grepl("unit_key=|counts overlap", output))
  expect_silent(emit_missing_report(report, diagnostics = "none"))
})

test_that("completion-only reports explicitly say classification was not run", {
  f <- missing_report_fixture()
  report <- missing_change_report(f$before, f$before, added = f$added, classified = FALSE)
  output <- capture_missing_report(report, source = "complete_design")
  expect_match(output, "Completion only", fixed = TRUE)
  expect_match(output, "were not run", fixed = TRUE)
  expect_match(output, "newly added coding fields remain missing", fixed = TRUE)
  expect_false(grepl("still unclassified|First classifications", output))
})

test_that("empty inputs and absent optional grouping columns remain reportable", {
  empty <- missing_report_fixture()$before[0, c("code_type", "code_id", "code_score", "code_status")]
  report <- missing_change_report(empty, empty)
  expect_true(all(unlist(report$totals) == 0))
  expect_equal(nrow(report$by_unit), 0)
  expect_match(capture_missing_report(report), "0 output rows", fixed = TRUE)
})

test_that("unit keys are literal diagnostic text rather than cli expressions", {
  f <- missing_report_fixture()
  f$after$unit_key <- "U{literal_key}"
  report <- do.call(missing_change_report, f)
  output <- capture_missing_report(report, diagnostics = "full")
  expect_match(output, "unit_key=U{literal_key}", fixed = TRUE)
})
