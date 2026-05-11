test_that("prepare_responses handles already parsed response lists", {
  responses <- tibble::tibble(
    group_id = "G",
    login_name = "L",
    login_code = "C",
    booklet_id = "B",
    unit_key = "U",
    responses = list(list(
      list(id = "text_4", status = "DISPLAYED", value = list()),
      list(id = "01", status = "VALUE_CHANGED", value = "geo")
    ))
  )

  result <- prepare_responses(responses)

  expect_equal(result$variable_id, c("text_4", "01"))
  expect_equal(result$variable_status, c("DISPLAYED", "VALUE_CHANGED"))
})

test_that("prepare_responses still handles response JSON strings", {
  response_json <- as.character(jsonlite::toJSON(
    list(
      list(id = "text_4", status = "DISPLAYED", value = list()),
      list(id = "01", status = "VALUE_CHANGED", value = "geo")
    ),
    auto_unbox = TRUE
  ))

  responses <- tibble::tibble(
    group_id = "G",
    login_name = "L",
    login_code = "C",
    booklet_id = "B",
    unit_key = "U",
    responses = response_json
  )

  result <- prepare_responses(responses)

  expect_equal(result$variable_id, c("text_4", "01"))
  expect_equal(result$variable_status, c("DISPLAYED", "VALUE_CHANGED"))
})
