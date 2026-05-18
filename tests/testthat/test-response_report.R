test_that("response reports keep units with empty nested response data", {
  resp <- list(
    list(
      list(
        groupname = "group",
        loginname = "login",
        code = "7kfe",
        bookletname = "booklet",
        unitname = "SB_2631",
        responses = list(
          list(id = "elementCodes", content = "[]", ts = "1")
        ),
        laststate = NA_character_
      ),
      list(
        groupname = "group",
        loginname = "login",
        code = "7kfe",
        bookletname = "booklet",
        unitname = "SB_2632",
        responses = list(),
        laststate = NA_character_
      )
    )
  )

  responses_raw <- response_report_to_tibble(resp)

  expect_equal(responses_raw$unitname, c("SB_2631", "SB_2632"))
  expect_equal(length(responses_raw$responses[[1]]), 1)
  expect_equal(length(responses_raw$responses[[2]]), 0)
})

test_that("empty parsed responses are retained as empty element codes", {
  responses <- unnest_responses(list(), is_parsed = TRUE)

  expect_equal(responses$id, "elementCodes")
  expect_true(is.na(responses$content))
})
