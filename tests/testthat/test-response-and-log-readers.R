test_that("read_logs reads one or multiple CSV files and normalizes unit aliases", {
  file1 <- tempfile(fileext = ".csv")
  file2 <- tempfile(fileext = ".csv")
  writeLines(
    c(
      "groupname;loginname;code;bookletname;unitname;originalUnitId;timestamp;logentry",
      "G1;L1;C1;B1;Alias1;U1;1000;PLAYER = RUNNING"
    ),
    file1
  )
  writeLines(
    c(
      "groupname;loginname;code;bookletname;unitname;originalUnitId;timestamp;logentry",
      "G2;L2;C2;B2;Alias2;;2000;PLAYER = LOADING"
    ),
    file2
  )

  out <- read_logs(c(file1, file2))

  expect_equal(out$file, c(file1, file2))
  expect_equal(out$unit_key, c("U1", "Alias2"))
  expect_equal(out$unit_alias, c("Alias1", "Alias2"))
  expect_equal(out$log_entry, c("PLAYER = RUNNING", "PLAYER = LOADING"))
})

test_that("reader functions validate file paths", {
  missing_file <- file.path(tempdir(), "does-not-exist.csv")

  expect_error(read_logs(missing_file), "do not exist")
  expect_error(read_responses(missing_file), "do not exist")
  expect_error(read_system_checks(missing_file), "do not exist")
})

test_that("read_responses expands response and last-state JSON columns", {
  file <- tempfile(fileext = ".csv")
  responses <- jsonlite::toJSON(
    list(list(id = "elementCodes", content = "[{\"id\":\"V1\",\"value\":\"A\"}]", ts = 10)),
    auto_unbox = TRUE
  )
  laststate <- jsonlite::toJSON(
    list(list(PLAYER = "RUNNING", RESPONSE_PROGRESS = "complete", PRESENTATION_PROGRESS = "some")),
    auto_unbox = TRUE
  )

  writeLines(
    c(
      "groupname;loginname;code;bookletname;unitname;responses;laststate",
      paste("G1", "L1", "C1", "B1", "U1", responses, laststate, sep = ";")
    ),
    file
  )

  out <- read_responses(file)

  expect_equal(out$group_id, "G1")
  expect_equal(out$unit_key, "U1")
  expect_equal(out$responses, "[{\"id\":\"V1\",\"value\":\"A\"}]")
  expect_equal(out$player, "RUNNING")
  expect_equal(out$response_progress, "complete")
})

test_that("read_system_checks unnests JSON response content from semicolon CSV", {
  file <- tempfile(fileext = ".csv")
  response_content <- jsonlite::toJSON(
    list(list(id = "V1", value = "A", status = "VALUE_CHANGED")),
    auto_unbox = TRUE
  )
  responses <- jsonlite::toJSON(
    list(list(content = response_content)),
    auto_unbox = TRUE
  )
  writeLines(
    c(
      "Name;Responses",
      paste("check1", gsub("\"", "`", responses), sep = ";")
    ),
    file
  )

  out <- read_system_checks(file)

  expect_equal(out$Name, "check1")
  expect_equal(out$variable_id, "V1")
  expect_equal(out$value, "A")
  expect_equal(out$status, "VALUE_CHANGED")
})

test_that("read_system_checks keeps system check rows with missing responses", {
  file <- tempfile(fileext = ".csv")
  response_content <- jsonlite::toJSON(
    list(list(id = "download_speed", value = "20", status = "VALUE_CHANGED")),
    auto_unbox = TRUE
  )
  responses <- jsonlite::toJSON(
    list(list(content = response_content)),
    auto_unbox = TRUE
  )
  writeLines(
    c(
      "Name;groupname;downlink;rtt;Responses",
      paste("check1", "G1", "12.5", "50", gsub("\"", "`", responses), sep = ";"),
      "check2;;10;40;NA"
    ),
    file
  )

  out <- read_system_checks(file)
  summary <- summarise_system_checks(out)
  check2 <- summary[summary$Name == "check2", ]

  expect_equal(out$Name, c("check1", "check2"))
  expect_true(is.na(out$groupname[2]))
  expect_equal(out$variable_id[1], "download_speed")
  expect_true(is.na(out$variable_id[2]))
  expect_equal(check2$n_system_check_rows, 1L)
  expect_true(check2$has_network_metrics)
  expect_equal(check2$system_check_download_value, "10")
  expect_equal(check2$system_check_rtt_value, "40")
})

test_that("prepare_responses and prepare_coded unnest response JSON", {
  responses <- tibble::tibble(
    login_name = "L1",
    responses = jsonlite::toJSON(
      list(list(id = "V1", value = "A", status = "VALUE_CHANGED")),
      auto_unbox = TRUE
    )
  )
  coded <- tibble::tibble(
    login_name = "L1",
    coded = jsonlite::toJSON(
      list(list(id = "V1", value = list("A"), code = 1, score = 1, status = "CODING_COMPLETE")),
      auto_unbox = TRUE
    )
  )

  responses_out <- prepare_responses(responses)
  coded_out <- prepare_coded(coded)

  expect_equal(responses_out$variable_id, "V1")
  expect_equal(responses_out$variable_status, "VALUE_CHANGED")
  expect_equal(coded_out$variable_id, "V1")
  expect_equal(coded_out$code_id, 1)
  expect_equal(coded_out$value, "A")

  expect_error(prepare_responses(tibble::tibble(x = "[]")), "responses")
  expect_error(prepare_coded(tibble::tibble(x = "[]")), "coded")
})

test_that("prepare_logs parses selected log event types", {
  logs <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    ts = c("1", "2", "3", "4"),
    log_entry = c(
      "CURRENT_PAGE_ID = 12",
      "PLAYER = RUNNING",
      "CONNECTION : \"LOST\"",
      "FOCUS : \"HAS_NOT\""
    )
  )

  out <- prepare_logs(logs, log_events = c("current_page_id", "player", "connection", "focus"))

  expect_equal(out$current_page_id[1], "12")
  expect_equal(out$player[2], "running")
  expect_equal(out$connection[3], "LOST")
  expect_equal(out$focus[4], "HAS_NOT")

  expect_error(prepare_logs(tibble::tibble(x = "PLAYER = RUNNING")), "log_entry")
})

test_that("unnest helpers handle legacy response shapes and duplicated last states", {
  old_response <- list(list(id = "lastSeenPageIndex", content = "2"))
  latest_response <- list(
    list(id = "V1", content = "old", ts = 1),
    list(id = "V1", content = "new", ts = 2)
  )
  laststate <- c(
    jsonlite::toJSON(list(list(PLAYER = "LOADING", RESPONSE_PROGRESS = "none")), auto_unbox = TRUE),
    jsonlite::toJSON(list(list(PLAYER = "RUNNING", RESPONSE_PROGRESS = "complete")), auto_unbox = TRUE)
  )

  expect_equal(eatPrepTBA:::unnest_responses(old_response)$id, "elementCodes")
  expect_equal(eatPrepTBA:::unnest_responses(latest_response)$content, "new")
  expect_equal(eatPrepTBA:::unnest_laststate(laststate)$PLAYER, "RUNNING")
  expect_equal(eatPrepTBA:::update_laststate("RESPONSE_PROGRESS", c("none", "complete")), "complete")
})
