manual_match_fixture <- function() {
  responses <- tibble::tibble(
    group_id = "G1", login_name = "L1", login_code = c("P1", "P2"),
    booklet_id = "B1", unit_key = "U1",
    responses = '[{"id":"V1","status":"VALUE_CHANGED","value":"A"}]'
  )
  manual <- dplyr::mutate(
    dplyr::select(responses, -responses), variable_id = "V1", code_id = 1L
  )
  list(responses = responses, manual = manual, units = minimal_units(),
       keys = c("group_id", "login_code", "login_name", "booklet_id", "unit_key"))
}

capture_manual_match_warning <- function(expr) {
  warnings <- list()
  value <- withCallingHandlers(expr, eatPrepTBA_unmatched_manual_codes = function(w) {
    warnings[[length(warnings) + 1L]] <<- w
    invokeRestart("muffleWarning")
  })
  expect_length(warnings, 1L)
  list(value = value, warning = warnings[[1L]])
}

test_that("matching manual records remain quiet and are applied", {
  f <- manual_match_fixture()
  expect_no_warning(out <- suppressMessages(code_responses(
    f$responses, f$units, prepare = TRUE, codes_manual = f$manual
  )))
  expect_equal(out$code_id, c(1L, 1L))
  expect_equal(out$code_status, rep("CODING_COMPLETE", 2L))

  f$responses$login_code <- f$manual$login_code <- NA_character_
  expect_no_warning(warn_unmatched_manual_codes(f$manual, f$responses, f$keys))
  expect_no_warning(warn_unmatched_manual_codes(f$manual[0, ], f$responses, f$keys))
})

test_that("NA versus empty login codes raises one warning without changing coding", {
  f <- manual_match_fixture()
  f$responses$login_code <- ""
  f$manual$login_code <- NA_character_
  original_manual <- f$manual
  original_responses <- f$responses
  auto <- suppressMessages(code_responses(f$responses, f$units, prepare = TRUE))

  result <- capture_manual_match_warning(suppressMessages(code_responses(
    f$responses, f$units, prepare = TRUE, codes_manual = f$manual
  )))
  expect_identical(result$value, auto)
  expect_identical(f$manual, original_manual)
  expect_identical(f$responses, original_responses)
  expect_equal(result$warning$n_manual, 2L)
  expect_equal(result$warning$n_unmatched, 2L)
  expect_equal(result$warning$matches_without_key[["login_code"]], 2)
  expect_match(conditionMessage(result$warning), "Without.*login_code")
  expect_match(conditionMessage(result$warning), "empty strings", fixed = TRUE)
})

test_that("each matching key receives the same diagnostic when all records fail", {
  f <- manual_match_fixture()
  # Repeated manual variables and response rows must not inflate the counts.
  manual <- f$manual[rep(1:2, each = 3), ]
  responses <- f$responses[rep(1:2, each = 2), ]
  for (key in f$keys) {
    changed <- manual
    changed[[key]] <- "different"
    result <- capture_manual_match_warning(
      warn_unmatched_manual_codes(changed, responses, f$keys)
    )
    expected <- stats::setNames(rep(0, length(f$keys)), f$keys)
    expected[[key]] <- nrow(manual)
    expect_equal(result$warning$matches_without_key, expected, info = key)
    expect_equal(result$warning$n_unmatched, nrow(manual))
  }
})

test_that("partly unmatched records warn while valid manual codes are applied", {
  f <- manual_match_fixture()
  f$manual$login_code[2] <- "unknown"
  result <- capture_manual_match_warning(suppressMessages(code_responses(
    f$responses, f$units, prepare = TRUE, codes_manual = f$manual
  )))
  expect_equal(result$warning$n_unmatched, 1L)
  expect_length(result$warning$matches_without_key, 0L)
  expect_equal(result$value$code_id[result$value$login_code == "P1"], 1L)
  expect_true(is.na(result$value$code_id[result$value$login_code == "P2"]))
})

test_that("multiple identifier differences are not blamed on a single key", {
  f <- manual_match_fixture()
  f$manual$login_code <- "unknown"
  f$manual$group_id <- "other"
  result <- capture_manual_match_warning(
    warn_unmatched_manual_codes(f$manual, f$responses, f$keys)
  )
  expect_true(all(result$warning$matches_without_key == 0))
  expect_match(conditionMessage(result$warning), "Omitting any single key")
})

test_that("unavailable payloads and manual records outside the response units are reported", {
  f <- manual_match_fixture()
  f$responses$responses[2] <- NA_character_
  result <- capture_manual_match_warning(suppressMessages(code_responses(
    f$responses, f$units, codes_manual = f$manual
  )))
  expect_equal(result$warning$n_unmatched, 1L)

  f <- manual_match_fixture()
  f$manual <- dplyr::bind_rows(
    f$manual, dplyr::mutate(f$manual[1, ], unit_key = "U2")
  )
  result <- capture_manual_match_warning(suppressMessages(code_responses(
    f$responses, f$units, codes_manual = f$manual
  )))
  expect_equal(result$warning$n_manual, 3L)
  expect_equal(result$warning$n_unmatched, 1L)
  expect_equal(result$value$code_id, c(1L, 1L))
})
