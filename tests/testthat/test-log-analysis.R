test_that("summarise_log_inventory classifies supported and unsupported event types", {
  logs <- tibble::tibble(
    group_id = c("G1", "G1", "G1", "G2", "G2"),
    login_name = c("L1", "L1", "L1", "L2", "L2"),
    login_code = c("C1", "C1", "C1", "C2", "C2"),
    booklet_id = c("B1", "B1", "B1", "B2", "B2"),
    ts = c(1, 2, 3, 4, 5),
    log_entry = c(
      "PLAYER = RUNNING",
      "CONNECTION : \"LOST\"",
      "\"CONNECTION\" : \"POLLING\"",
      "Runtime Error: PLAYER_FAILED",
      "BOOKLETLOCKEDbyOPERATOR"
    )
  )

  out <- summarise_log_inventory(logs)

  connection <- out[out$log_type == "CONNECTION", ]
  runtime_error <- out[out$log_type == "Runtime Error", ]
  lock_message <- out[out$log_type == "BOOKLETLOCKEDbyOPERATOR", ]

  expect_equal(connection$n, 2)
  expect_equal(connection$log_family, "connection")
  expect_true(connection$known_log_type)
  expect_true(connection$supported_parser)

  expect_equal(runtime_error$log_family, "runtime_error")
  expect_true(runtime_error$known_log_type)
  expect_false(runtime_error$supported_parser)

  expect_equal(lock_message$log_family, "other")
  expect_false(lock_message$known_log_type)
  expect_false(lock_message$supported_parser)
})

test_that("summarise_log_environment parses escaped and nested LOADCOMPLETE payloads", {
  escaped_loadcomplete <- paste0(
    "LOADCOMPLETE : ",
    "\"{\\\"browserVersion\\\":\\\"18.3\\\",",
    "\\\"browserName\\\":\\\"Safari\\\",",
    "\\\"osName\\\":\\\"iOS 18.6.2\\\",",
    "\\\"device\\\":\\\"Apple iPad tablet\\\",",
    "\\\"screenSizeWidth\\\":820,",
    "\\\"screenSizeHeight\\\":1180,",
    "\\\"loadTime\\\":30926}\""
  )
  nested_loadcomplete <- paste0(
    "LOADCOMPLETE : ",
    jsonlite::toJSON(
      list(
        environment = list(
          browserVersion = "126.0",
          browserName = "Chrome",
          osName = "Windows 11",
          device = "Windows desktop",
          screenSizeWidth = 1920,
          screenSizeHeight = 1080,
          loadTime = 1000
        )
      ),
      auto_unbox = TRUE
    )
  )
  logs <- tibble::tibble(
    group_id = c("G1", "G1"),
    login_name = c("L1", "L1"),
    login_code = c("C1", "C1"),
    booklet_id = c("B1", "B1"),
    ts = c(100, 200),
    log_entry = c(escaped_loadcomplete, nested_loadcomplete)
  )

  out <- summarise_log_environment(logs)

  expect_equal(nrow(out), 1)
  expect_equal(out$n_loadcomplete_events, 2)
  expect_equal(out$n_loadcomplete_parsed, 2)
  expect_true(out$loadcomplete_parse_ok)
  expect_true(out$loadcomplete_multiple)
  expect_true(out$loadcomplete_conflicting)
  expect_equal(out$browser_name, "Safari")
  expect_equal(out$browser_version, "18.3")
  expect_equal(out$os_family, "iOS")
  expect_equal(out$os_version, "18.6.2")
  expect_equal(out$device_class, "tablet")
  expect_equal(out$screen_size_width, 820)
  expect_equal(out$screen_size_height, 1180)
  expect_equal(out$screen_orientation, "portrait")
  expect_equal(out$load_time, 30926)
})

test_that("summarise_log_environment keeps malformed LOADCOMPLETE rows diagnosable", {
  logs <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    ts = 100,
    log_entry = "LOADCOMPLETE : not-json"
  )

  out <- summarise_log_environment(logs)

  expect_equal(out$n_loadcomplete_events, 1)
  expect_equal(out$n_loadcomplete_parsed, 0)
  expect_false(out$loadcomplete_parse_ok)
  expect_false(is.na(out$loadcomplete_parse_error))
})

test_that("prepare_logs reuses shared parsers for LOADCOMPLETE and quoted CONNECTION", {
  loadcomplete <- paste0(
    "LOADCOMPLETE : ",
    "\"{\\\"browserVersion\\\":\\\"18.3\\\",",
    "\\\"browserName\\\":\\\"Safari\\\",",
    "\\\"osName\\\":\\\"iOS 18.6.2\\\",",
    "\\\"device\\\":\\\"Apple iPad tablet\\\",",
    "\\\"screenSizeWidth\\\":820,",
    "\\\"screenSizeHeight\\\":1180,",
    "\\\"loadTime\\\":30926}\""
  )
  logs <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    ts = c(1, 2),
    log_entry = c(loadcomplete, "\"CONNECTION\" : \"POLLING\"")
  )

  out <- prepare_logs(logs, log_events = c("loadcomplete", "connection"))

  expect_equal(out$browser_name[1], "Safari")
  expect_equal(out$screen_size_width[1], 820)
  expect_equal(out$load_time[1], 30926)
  expect_equal(out$connection[2], "POLLING")
})
