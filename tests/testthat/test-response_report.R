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

test_that("code_responses ignores rows with missing response payloads", {
  units <- tibble::tibble(
    ws_id = 1,
    ws_label = "workspace",
    unit_key = "UNIT_1",
    unit_id = 1,
    unit_label = "Unit 1",
    coding_scheme = list(list(variableCodings = list())),
    unit_variables = list(list())
  )

  responses <- tibble::tibble(
    group_id = "group",
    login_name = "login",
    login_code = "code",
    booklet_id = "booklet",
    unit_key = "UNIT_1",
    responses = NA_character_
  )

  coded <- NULL

  expect_error(
    coded <- suppressWarnings(code_responses(responses, units, prepare = FALSE)),
    NA
  )
  expect_equal(nrow(coded), 0)
})
