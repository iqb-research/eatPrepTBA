test_that("unpack_response_jsons auto-detects wide response JSON columns", {
  responses <- tibble::tibble(
    unit_key = c("U1", "U2"),
    unit_alias = c("U1_ALIAS", "U2_ALIAS"),
    group_id = c("G1", "G1"),
    login_name = c("L1", "L2"),
    login_code = c("C1", "C2"),
    booklet_id = c("B1", "B1"),
    file = c("not-json.csv", "not-json.csv"),
    question_0_content = c(
      paste0(
        "[",
        "{\"id\":\"value\",\"status\":\"CODING_COMPLETE\",\"value\":4,",
        "\"subform\":\"0\",\"code\":1,\"score\":1},",
        "{\"id\":\"time\",\"status\":\"VALUE_CHANGED\",\"value\":3146,",
        "\"subform\":\"0\"}",
        "]"
      ),
      NA_character_
    ),
    question_0_ts = c(1000, NA),
    sums_content = c(
      "[{\"id\":\"total_correct\",\"status\":\"VALUE_CHANGED\",\"value\":1}]",
      NA_character_
    ),
    sums_ts = c(1100, NA),
    responses = c(
      NA_character_,
      "[{\"id\":\"button_1\",\"status\":\"DISPLAYED\",\"value\":null}]"
    ),
    responses_ts = c(NA, 2000),
    geometry_variables = c("[]", "[]")
  )

  out <- unpack_response_jsons(responses)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 4)
  expect_equal(
    unique(out$response_column),
    c("question_0_content", "sums_content", "responses")
  )
  expect_equal(out$question_index[out$response_column == "question_0_content"], c(0L, 0L))
  expect_equal(out$response_ts[out$response_id == "value"], 1000)
  expect_equal(out$response_ts[out$response_id == "total_correct"], 1100)
  expect_equal(out$response_ts[out$response_id == "button_1"], 2000)
  expect_equal(out$code_id[out$response_id == "value"], 1L)
  expect_equal(out$code_score[out$response_id == "value"], 1)
  expect_type(out$value, "list")
})

test_that("unpack_response_jsons accepts explicit response columns", {
  responses <- tibble::tibble(
    unit_key = "U1",
    question_0_content = paste0(
      "[",
      "{\"id\":\"value\",\"status\":\"CODING_COMPLETE\",\"value\":4,",
      "\"subform\":\"0\",\"code\":1,\"score\":1}",
      "]"
    ),
    question_0_ts = 1000,
    responses = "[{\"id\":\"button_1\",\"status\":\"DISPLAYED\",\"value\":null}]",
    responses_ts = 2000
  )

  out <- unpack_response_jsons(responses, response_cols = "question_0_content")

  expect_equal(nrow(out), 1)
  expect_equal(out$response_column, "question_0_content")
  expect_equal(out$response_ts, 1000)
})

test_that("prepare_unpacked_codes creates code_responses-like columns", {
  responses <- tibble::tibble(
    unit_key = "U1",
    unit_alias = "U1_ALIAS",
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    question_0_content = paste0(
      "[",
      "{\"id\":\"value\",\"status\":\"CODING_COMPLETE\",\"value\":4,",
      "\"subform\":\"0\",\"code\":1,\"score\":1},",
      "{\"id\":\"time\",\"status\":\"VALUE_CHANGED\",\"value\":3146,",
      "\"subform\":\"0\"}",
      "]"
    ),
    question_0_ts = 1000
  )

  out <-
    responses %>%
    unpack_response_jsons() %>%
    prepare_unpacked_codes()

  expect_equal(nrow(out), 1)
  expect_equal(out$variable_id, "0")
  expect_equal(out$code_status, "CODING_COMPLETE")
  expect_equal(out$code_id, 1L)
  expect_equal(out$code_score, 1)
  expect_equal(out$value[[1]], 4L)
})
test_that("unpack_response_jsons handles empty and malformed payloads", {
  responses <- tibble::tibble(
    unit_key = c("U1", "U2", "U3"),
    other = c("keep", "keep", "keep"),
    payload = c(NA_character_, "[]", "{not-json")
  )

  empty <- unpack_response_jsons(responses)

  expect_s3_class(empty, "tbl_df")
  expect_equal(nrow(empty), 0)
  expect_true(all(c(
    "response_row", "unit_key", "other", "payload", "response_id",
    "response_status", "value", "code_id", "code_score"
  ) %in% names(empty)))

  out <- unpack_response_jsons(
    responses,
    response_cols = "payload",
    id_cols = "unit_key",
    keep_empty = TRUE
  )

  expect_equal(nrow(out), 3)
  expect_equal(out$unit_key, c("U1", "U2", "U3"))
  expect_true(".parse_error" %in% names(out))
  expect_match(out$.parse_error[3], "lexical error")
  expect_true(all(is.na(out$response_ts)))
})

test_that("prepare_unpacked_codes can keep uncoded rows and derive fallback variable ids", {
  unpacked <- tibble::tibble(
    response_row = 1:3,
    unit_key = "U1",
    response_column = c("question_0_content", "question_0_content", "sums_content"),
    response_name = c("question_0", "question_0", "sums"),
    response_ts = c(1000, 1000, 1100),
    question_index = c(0L, 0L, NA_integer_),
    response_id = c("value", "value", "total_correct"),
    response_status = c("CODING_COMPLETE", "VALUE_CHANGED", "VALUE_CHANGED"),
    value = list(1L, 2L, 3L),
    subform = c(NA_character_, "explicit", NA_character_),
    code_id = c(1L, NA_integer_, NA_integer_),
    code_score = c(1, NA_real_, NA_real_)
  )

  coded <- prepare_unpacked_codes(
    unpacked,
    response_id = c("value", "total_correct"),
    variable_id_from = "response_name",
    keep_uncoded = TRUE
  )

  expect_equal(nrow(coded), 3)
  expect_equal(coded$variable_id, c("question_0", "question_0", "sums"))
  expect_equal(coded$code_status, coded$response_status)
})
