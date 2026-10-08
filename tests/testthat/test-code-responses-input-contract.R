input_contract_responses <- function(logins = "L1") {
  tibble::tibble(
    group_id = "G1", login_name = logins, login_code = "C1",
    booklet_id = "B1", unit_key = "U1", unit_alias = "U1",
    responses = "[{}]"
  )
}

test_that("all-unscored autocoder output retains a numeric score column", {
  testthat::local_mocked_bindings(
    code_responses_array = function(coding_scheme, unit_responses) {
      tibble::tibble(
        group_id = "G1", login_name = "L1", login_code = "C1",
        booklet_id = "B1", unit_alias = "U1",
        id = "V1", code = NA_integer_, status = "NOT_REACHED",
        value = list(NA_character_)
      )
    },
    .package = "eatAutoCode"
  )
  for (prepare in c(FALSE, TRUE)) {
    result <- code_responses(input_contract_responses(), minimal_units(),
                             prepare = prepare)
    expect_true("code_score" %in% names(result))
    expect_type(result$code_score, "double")
    expect_identical(result$code_score, NA_real_)
    expect_identical(result$code_status, "NOT_REACHED")
    if (prepare) {
      expect_true("code_type" %in% names(result))
      expect_identical(result$value, NA_character_)
      design <- dplyr::mutate(
        input_contract_responses()[c("group_id", "login_name", "login_code",
                                      "booklet_id", "unit_key", "unit_alias")],
        booklet_no = 1L, testlet_no = 1L, unit_booklet_no = 1L
      )
      expect_no_error(completed <- complete_design(
        result, minimal_units(), design,
        recode_omissions_to_not_reached = NULL, diagnostics = "none"
      ))
      expect_identical(completed$code_score, NA_real_)
    }
  }
})

test_that("real uncoded responses can be prepared and design-classified", {
  responses <- input_contract_responses()
  responses$responses <- '[{"id":"V1","status":"NOT_REACHED","value":null}]'
  coded <- code_responses(responses, minimal_units(), prepare = TRUE)
  expect_identical(coded$code_id, NA_integer_)
  expect_identical(coded$code_score, NA_real_)
  expect_true("code_type" %in% names(coded))
  expect_identical(coded$code_status, "NOT_REACHED")
  expect_true(all(is.na(coded$value)))
  design <- dplyr::mutate(
    responses[c("group_id", "login_name", "login_code", "booklet_id",
                  "unit_key", "unit_alias")],
    booklet_no = 1L, testlet_no = 1L, unit_booklet_no = 1L
  )
  completed <- complete_design(coded, minimal_units(), design, diagnostics = "none")
  expect_identical(completed$code_type, "MISSING_NOT_REACHED")
  expect_equal(completed$code_id, -96)
  expect_identical(completed$code_score, NA_real_)
  expect_identical(completed$code_status, "NOT_REACHED")
})

test_that("partial manual missing profiles retain unrelated standard codes", {
  captured <- NULL
  testthat::local_mocked_bindings(
    code_responses_array = function(coding_scheme, unit_responses) {
      captured <<- lapply(unit_responses$manual, function(payload) {
        jsonlite::fromJSON(payload)
      })
      tibble::tibble(id = "V1", code = 1L, score = 1,
                     status = "CODING_COMPLETE", value = list("A"))
    },
    .package = "eatAutoCode"
  )
  responses <- input_contract_responses(paste0("L", 1:5))
  manual <- responses[c("group_id", "login_name", "login_code", "booklet_id", "unit_key")]
  manual$variable_id <- "V1"
  manual$code_id <- c(-96L, -97L, -98L, -99L, -199L)
  custom <- tibble::tribble(
    ~code_id, ~code_status, ~code_score, ~code_type,
    -199, "DISPLAYED", 0, "MISSING_BY_OMISSION"
  )
  code_responses(responses, minimal_units(), codes_manual = manual,
                  missings = custom)
  inserted <- dplyr::bind_rows(captured)
  expect_equal(inserted$code, manual$code_id)
  expect_identical(inserted$status,
                   c("NOT_REACHED", "CODING_ERROR", "INVALID", "DISPLAYED", "DISPLAYED"))
  expect_equal(inserted$score, rep(0, 5))
})

test_that("custom manual missing entries replace only matching code IDs", {
  captured <- NULL
  testthat::local_mocked_bindings(
    code_responses_array = function(coding_scheme, unit_responses) {
      captured <<- jsonlite::fromJSON(unit_responses$manual[[1L]])
      tibble::tibble(id = "V1", code = -99L, score = NA_real_,
                     status = "DISPLAYED", value = list(NA_character_))
    },
    .package = "eatAutoCode"
  )
  responses <- input_contract_responses()
  manual <- responses[c("group_id", "login_name", "login_code", "booklet_id", "unit_key")]
  manual$variable_id <- "V1"
  manual$code_id <- -99L
  custom <- tibble::tribble(
    ~code_id, ~code_status, ~code_score, ~code_type,
    -99, "DISPLAYED", NA_real_, "MISSING_BY_OMISSION"
  )
  result <- code_responses(responses, minimal_units(), codes_manual = manual,
                           missings = custom)
  expect_identical(captured$status, "DISPLAYED")
  # A JSON-null score is absent in jsonlite's simplified one-row frame.
  expect_true(is.null(captured$score) || all(is.na(captured$score)))
  expect_equal(nrow(captured), 1L)
  expect_identical(result$code_score, NA_real_)
})

test_that("real manual standard codes remain classified with a partial profile", {
  responses <- input_contract_responses(paste0("L", 1:3))
  responses$responses <- '[{"id":"V1","status":"VALUE_CHANGED","value":"A"}]'
  manual <- responses[c("group_id", "login_name", "login_code", "booklet_id", "unit_key")]
  manual$variable_id <- "V1"
  manual$code_id <- c(-96L, -98L, -99L)
  custom <- tibble::tribble(
    ~code_id, ~code_status, ~code_score, ~code_type,
    -199, "DISPLAYED", 0, "MISSING_BY_OMISSION"
  )
  result <- code_responses(responses, minimal_units(), prepare = TRUE,
                           codes_manual = manual, missings = custom)
  result <- result[match(responses$login_name, result$login_name), ]
  expect_identical(result$code_status, c("NOT_REACHED", "INVALID", "DISPLAYED"))
  expect_equal(result$code_id, manual$code_id)
})
