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

test_that("empty file response payloads are retained as empty element codes", {
  responses <- unnest_responses(NA_character_, is_parsed = FALSE)

  expect_equal(responses$id, "elementCodes")
  expect_true(is.na(responses$content))
})

test_that("read_responses keeps mixed rows with missing response payloads", {
  response_file <- tempfile(fileext = ".csv")
  responses_raw <- tibble::tibble(
    groupname = "group",
    loginname = "login",
    code = "7kfe",
    bookletname = "booklet",
    unitname = c("SB_2631", "SB_2632"),
    responses = c(
      '[{"id":"elementCodes","content":"[]","ts":"1"}]',
      NA_character_
    ),
    laststate = NA_character_
  )

  readr::write_delim(responses_raw, response_file, delim = ";")

  responses <- read_responses(response_file)

  expect_equal(responses$unit_key, c("SB_2631", "SB_2632"))
  expect_equal(responses$responses[responses$unit_key == "SB_2631"], "[]")
  expect_true(is.na(responses$responses[responses$unit_key == "SB_2632"]))
})

test_that("read_responses keeps all rows when all response payloads are missing", {
  response_file <- tempfile(fileext = ".csv")
  responses_raw <- tibble::tibble(
    groupname = "group",
    loginname = "login",
    code = c("7kfe", "q3u4"),
    bookletname = "booklet",
    unitname = c("SB_2631", "SB_2632"),
    responses = NA_character_,
    laststate = NA_character_
  )

  readr::write_delim(responses_raw, response_file, delim = ";")

  responses <- read_responses(response_file)

  expect_equal(responses$unit_key, c("SB_2631", "SB_2632"))
  expect_true(all(is.na(responses$responses)))
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

test_that("code_responses returns structured empty output for missing payloads with preparation", {
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
    coded <- code_responses(responses, units, prepare = TRUE),
    NA
  )
  expect_equal(nrow(coded), 0)
  expect_true(all(c(
    "group_id", "login_name", "login_code", "booklet_id", "unit_key",
    "unit_alias", "variable_id", "code_status", "code_type"
  ) %in% names(coded)))
})
