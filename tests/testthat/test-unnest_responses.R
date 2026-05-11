test_that("geometryVariableCodes are merged into elementCodes", {
  responses <- list(
    list(
      id = "elementCodes",
      content = list(
        list(id = "text_4", status = "DISPLAYED", value = list())
      ),
      ts = 1772611011699,
      responseType = "iqb-standard@1.0"
    ),
    list(
      id = "geometryVariableCodes",
      content = list(
        list(id = "01", status = "VALUE_CHANGED", value = "geo")
      ),
      ts = 1772610923295,
      responseType = "iqb-standard@1.0"
    ),
    list(
      id = "stateVariableCodes",
      content = list(
        list(id = "Zeit", status = "VALUE_CHANGED", value = "1")
      ),
      ts = 1772611011699,
      responseType = "iqb-standard@1.0"
    )
  )

  result <- unnest_responses(responses, is_parsed = TRUE)

  expect_equal(result$id, c("elementCodes", "stateVariableCodes"))
  expect_equal(purrr::map_chr(result$content[[1]], "id"), c("text_4", "01"))
})

test_that("responses without geometryVariableCodes stay unchanged", {
  responses <- list(
    list(
      id = "elementCodes",
      content = list(
        list(id = "text_4", status = "DISPLAYED", value = list()),
        list(id = "01", status = "VALUE_CHANGED", value = "geo")
      ),
      ts = 1719816932904,
      responseType = "iqb-standard@1.0"
    ),
    list(
      id = "stateVariableCodes",
      content = list(
        list(id = "Zeit", status = "UNSET", value = "0")
      ),
      ts = 1719816932904,
      responseType = "iqb-standard@1.0"
    )
  )

  result <- unnest_responses(responses, is_parsed = TRUE)

  expect_equal(result$id, c("elementCodes", "stateVariableCodes"))
  expect_equal(purrr::map_chr(result$content[[1]], "id"), c("text_4", "01"))
})
