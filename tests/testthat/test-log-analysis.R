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

test_that("summarise_log_environment keeps sessions without LOADCOMPLETE", {
  loadcomplete <- paste0(
    "LOADCOMPLETE : ",
    jsonlite::toJSON(
      list(browserName = "Safari", device = "iPad", loadTime = 100),
      auto_unbox = TRUE
    )
  )
  logs <- tibble::tibble(
    group_id = c("G1", "G1"),
    login_name = c("L1", "L2"),
    login_code = c("C1", "C2"),
    booklet_id = c("B1", "B1"),
    ts = c(100, 100),
    log_entry = c(loadcomplete, "PLAYER = RUNNING")
  )

  out <- summarise_log_environment(logs)
  missing <- out[out$login_name == "L2", ]

  expect_equal(nrow(out), 2)
  expect_equal(missing$n_loadcomplete_events, 0)
  expect_equal(missing$n_loadcomplete_parsed, 0)
  expect_false(missing$loadcomplete_parse_ok)
  expect_false(missing$loadcomplete_multiple)
  expect_false(missing$loadcomplete_conflicting)
  expect_true(is.na(missing$browser_name))
})

test_that("summarise_log_environment parses valid LOADCOMPLETE JSON with empty strings", {
  logs <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    ts = 100,
    log_entry = 'LOADCOMPLETE : {"browserName":"Safari","device":"","loadTime":1}'
  )

  out <- summarise_log_environment(logs)

  expect_equal(out$n_loadcomplete_events, 1)
  expect_equal(out$n_loadcomplete_parsed, 1)
  expect_true(out$loadcomplete_parse_ok)
  expect_equal(out$browser_name, "Safari")
  expect_true(is.na(out$device))
  expect_equal(out$load_time, 1)
  expect_true(is.na(out$loadcomplete_parse_error))
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

test_that("detect_log_anomalies reports environment and player anomalies", {
  loadcomplete_1 <- paste0(
    "LOADCOMPLETE : ",
    "\"{\\\"browserVersion\\\":\\\"18.3\\\",",
    "\\\"browserName\\\":\\\"Safari\\\",",
    "\\\"osName\\\":\\\"iOS 18.6.2\\\",",
    "\\\"device\\\":\\\"Apple iPad tablet\\\",",
    "\\\"screenSizeWidth\\\":820,",
    "\\\"screenSizeHeight\\\":1180,",
    "\\\"loadTime\\\":30926}\""
  )
  loadcomplete_2 <- paste0(
    "LOADCOMPLETE : ",
    "\"{\\\"browserVersion\\\":\\\"126.0\\\",",
    "\\\"browserName\\\":\\\"Chrome\\\",",
    "\\\"osName\\\":\\\"Windows 11\\\",",
    "\\\"device\\\":\\\"Windows desktop\\\",",
    "\\\"screenSizeWidth\\\":1920,",
    "\\\"screenSizeHeight\\\":1080,",
    "\\\"loadTime\\\":1000}\""
  )
  logs <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    unit_key = "U1",
    unit_alias = "U1",
    ts = c(100, 200, 300, 400, 500),
    log_entry = c(
      loadcomplete_1,
      loadcomplete_2,
      "PLAYER = RUNNING",
      "PLAYER = LOADING",
      "PLAYER = LOADING"
    )
  )

  out <- detect_log_anomalies(logs)

  expect_true("multiple_loadcomplete_in_session" %in% out$anomaly_code)
  expect_true("conflicting_loadcomplete" %in% out$anomaly_code)
  expect_true("player_running_without_loading" %in% out$anomaly_code)
  expect_true("loading_without_running" %in% out$anomaly_code)
  expect_true("repeated_loading" %in% out$anomaly_code)
  expect_true("last_event_is_loading" %in% out$anomaly_code)
})

test_that("detect_log_anomalies reports missing and malformed LOADCOMPLETE", {
  missing_logs <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    ts = 100,
    log_entry = "PLAYER = RUNNING"
  )
  malformed_logs <- tibble::tibble(
    group_id = "G2",
    login_name = "L2",
    login_code = "C2",
    booklet_id = "B2",
    ts = 100,
    log_entry = "LOADCOMPLETE : not-json"
  )

  missing <- detect_log_anomalies(missing_logs)
  malformed <- detect_log_anomalies(malformed_logs)

  expect_true("missing_loadcomplete" %in% missing$anomaly_code)
  expect_true("malformed_loadcomplete" %in% malformed$anomaly_code)
})

test_that("detect_log_anomalies reports connection, focus, runtime, timestamp, and page anomalies", {
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
    login_code = "C1",
    booklet_id = "B1",
    unit_key = "U1",
    unit_alias = "U1",
    ts = c(100, 200, 300, 400, 500, 600, 550, 0, 700, 800, 900, 1000, 1100),
    log_entry = c(
      loadcomplete,
      "CONNECTION : \"OK\"",
      "CONNECTION : \"LOST\"",
      "CONNECTION : \"POLLING\"",
      "CONNECTION : \"LOST\"",
      "FOCUS : \"HAS_NOT\"",
      "FOCUS : \"HAS_NOT\"",
      "PLAYER = LOADING",
      "Runtime Error: PLAYER_FAILED",
      "PAGE_COUNT = 2",
      "PAGE_COUNT = 3",
      "CURRENT_PAGE_NR = 4",
      "CONNECTION : \"LOST\""
    )
  )

  out <- detect_log_anomalies(
    logs,
    focus_loss_threshold_ms = 50,
    connection_transition_threshold = 1
  )

  expect_true("connection_lost" %in% out$anomaly_code)
  expect_true("many_connection_transitions" %in% out$anomaly_code)
  expect_true("last_connection_lost" %in% out$anomaly_code)
  expect_true("focus_lost_never_regained" %in% out$anomaly_code)
  expect_true("repeated_focus_lost_before_regain" %in% out$anomaly_code)
  expect_true("runtime_error" %in% out$anomaly_code)
  expect_true("timestamp_decreases_in_input" %in% out$anomaly_code)
  expect_true("zero_timestamp" %in% out$anomaly_code)
  expect_true("page_count_inconsistent" %in% out$anomaly_code)
  expect_true("page_nr_exceeds_page_count" %in% out$anomaly_code)
})

test_that("detect_log_anomalies flags long focus loss across repeated HAS_NOT events", {
  logs <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    ts = c(100, 200, 300),
    log_entry = c(
      "FOCUS : \"HAS_NOT\"",
      "FOCUS : \"HAS_NOT\"",
      "FOCUS : \"HAS\""
    )
  )

  out <- detect_log_anomalies(logs, focus_loss_threshold_ms = 150)
  long_focus <- out[out$anomaly_code == "very_long_focus_loss", ]

  expect_true("repeated_focus_lost_before_regain" %in% out$anomaly_code)
  expect_equal(nrow(long_focus), 1)
  expect_equal(long_focus$ts_start, 100)
  expect_equal(long_focus$ts_end, 300)
  expect_equal(long_focus$evidence, "duration_ms=200")
})

test_that("detect_log_anomalies can include unknown log types as info", {
  logs <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    ts = c(100, 200),
    log_entry = c("LOADCOMPLETE : not-json", "SOME_PLAYER_EVENT : \"x\"")
  )

  out_without <- detect_log_anomalies(logs)
  out_with <- detect_log_anomalies(logs, include_unknown_events = TRUE)

  expect_false("unknown_log_type" %in% out_without$anomaly_code)
  expect_true("unknown_log_type" %in% out_with$anomaly_code)
})

test_that("summarise_log_qc condenses anomalies and keeps ok sessions", {
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
    group_id = c("G1", "G2", "G2"),
    login_name = c("L1", "L2", "L2"),
    login_code = c("C1", "C2", "C2"),
    booklet_id = c("B1", "B2", "B2"),
    ts = c(100, 100, 200),
    log_entry = c(loadcomplete, loadcomplete, "Runtime Error: PLAYER_FAILED")
  )

  anomalies <- detect_log_anomalies(logs)
  out <- summarise_log_qc(logs, anomalies = anomalies)

  ok_row <- out[out$login_name == "L1", ]
  critical_row <- out[out$login_name == "L2", ]

  expect_equal(ok_row$log_qc_flag, "ok")
  expect_equal(ok_row$n_anomalies, 0)
  expect_equal(critical_row$log_qc_flag, "critical")
  expect_true(critical_row$has_critical_anomaly)
  expect_true(stringr::str_detect(critical_row$anomaly_codes, "runtime_error"))
})
