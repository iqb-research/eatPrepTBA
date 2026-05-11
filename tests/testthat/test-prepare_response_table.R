test_that("prepare_response_table uses unitname for empty originalUnitId", {
  responses <- list(
    list(
      id = "elementCodes",
      content = list(
        list(id = "text_4", status = "DISPLAYED", value = list())
      ),
      ts = 1,
      responseType = "iqb-standard@1.0"
    )
  )

  responses_raw <- tibble::tibble(
    file = "responses.csv",
    groupname = "GROUP",
    loginname = "LOGIN",
    code = "CODE",
    bookletname = "BOOKLET",
    originalUnitId = "",
    unitname = "UNIT_ALIAS",
    responses = as.character(jsonlite::toJSON(responses, auto_unbox = TRUE)),
    laststate = "[{\"PLAYER\":\"RUNNING\"}]"
  )

  result <- prepare_response_table(responses_raw, responses_are_parsed = FALSE)

  expect_equal(result$unit_key, "UNIT_ALIAS")
  expect_equal(result$unit_alias, "UNIT_ALIAS")
})
