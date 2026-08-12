test_that("summarise_log_connections counts states and transitions per session", {
  logs <- tibble::tibble(
    group_id = c(rep("G1", 4), "G2"),
    login_name = c(rep("L1", 4), "L2"),
    login_code = c(rep("C1", 4), "C2"),
    booklet_id = c(rep("B1", 4), "B2"),
    ts = c(100, 200, 300, 400, 100),
    log_entry = c(
      "CONNECTION : \"OK\"",
      "CONNECTION : \"LOST\"",
      "CONNECTION : \"POLLING\"",
      "\"CONNECTION\" : \"WEBSOCKET\"",
      "PLAYER = RUNNING"
    )
  )

  out <- summarise_log_connections(logs)
  g1 <- out[out$group_id == "G1", ]
  g2 <- out[out$group_id == "G2", ]

  expect_equal(g1$n_connection_events, 4)
  expect_equal(g1$n_connection_transitions, 3)
  expect_equal(g1$n_connection_lost, 1)
  expect_equal(g1$first_connection_state, "OK")
  expect_equal(g1$last_connection_state, "WEBSOCKET")
  expect_true(g1$has_connection_lost)
  expect_false(g1$last_connection_state_lost)

  expect_equal(g2$n_connection_events, 0)
  expect_false(g2$has_connection_lost)
  expect_false(g2$last_connection_state_lost)
})

test_that("log metric summaries support missing session and unit columns", {
  logs <- tibble::tibble(
    ts = c(100, 200, 300, 400, 500, 600),
    log_entry = c(
      "CONNECTION : \"LOST\"",
      "FOCUS : \"HAS_NOT\"",
      "FOCUS : \"HAS\"",
      "PLAYER = RUNNING",
      "PAGE_COUNT = 1",
      "RESPONSE_PROGRESS = complete"
    )
  )

  connections <- summarise_log_connections(logs)
  focus <- summarise_log_focus(logs)
  player <- summarise_log_player(logs)
  pages <- summarise_log_pages(logs)
  progress <- summarise_log_progress(logs)

  expect_equal(nrow(connections), 1)
  expect_equal(connections$n_connection_events, 1)
  expect_true(connections$has_connection_lost)

  expect_equal(nrow(focus), 1)
  expect_equal(focus$total_focus_lost_time, 100)

  expect_equal(nrow(player), 1)
  expect_equal(player$n_player_running, 1)

  expect_equal(nrow(pages), 1)
  expect_equal(pages$page_count, 1)

  expect_equal(nrow(progress), 1)
  expect_true(progress$response_final_complete)
})

test_that("summarise_log_focus computes raw focus loss durations and unresolved losses", {
  logs <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    ts = c(100, 250, 300, 400),
    log_entry = c(
      "FOCUS : \"HAS_NOT\"",
      "FOCUS : \"HAS\"",
      "FOCUS : \"HAS_NOT\"",
      "FOCUS : \"HAS_NOT\""
    )
  )

  out <- summarise_log_focus(logs, focus_loss_threshold_ms = 100)

  expect_equal(out$n_focus_events, 4)
  expect_equal(out$n_focus_lost, 3)
  expect_equal(out$n_focus_regained, 1)
  expect_equal(out$n_focus_loss_intervals, 1)
  expect_equal(out$total_focus_lost_time, 150)
  expect_equal(out$max_focus_lost_time, 150)
  expect_equal(out$n_very_long_focus_loss, 1)
  expect_equal(out$n_focus_lost_never_regained, 2)
  expect_equal(out$n_repeated_focus_lost_before_regain, 1)
  expect_true(out$has_focus_loss)
  expect_true(out$has_unresolved_focus_loss)
})

test_that("summarise_log_focus measures repeated focus loss until regain", {
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

  out <- summarise_log_focus(logs, focus_loss_threshold_ms = 150)

  expect_equal(out$n_focus_loss_intervals, 1)
  expect_equal(out$total_focus_lost_time, 200)
  expect_equal(out$max_focus_lost_time, 200)
  expect_equal(out$n_very_long_focus_loss, 1)
  expect_equal(out$n_repeated_focus_lost_before_regain, 1)
})

test_that("summarise_log_player counts player states per session and unit", {
  logs <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    unit_key = c("U1", "U1", "U1", "U2"),
    unit_alias = c("U1", "U1", "U1", "U2"),
    ts = c(100, 200, 300, 400),
    log_entry = c(
      "PLAYER = LOADING",
      "PLAYER = RUNNING",
      "PLAYER = PAUSED",
      "PLAYER = RUNNING"
    )
  )

  out <- summarise_log_player(logs)
  u1 <- out[out$unit_key == "U1", ]
  u2 <- out[out$unit_key == "U2", ]

  expect_equal(u1$n_player_events, 3)
  expect_equal(u1$n_player_loading, 1)
  expect_equal(u1$n_player_running, 1)
  expect_equal(u1$n_player_paused, 1)
  expect_equal(u1$first_player_state, "LOADING")
  expect_equal(u1$last_player_state, "PAUSED")
  expect_true(u1$has_player_running)

  expect_equal(u2$n_player_loading, 0)
  expect_equal(u2$n_player_running, 1)
})

test_that("summarise_log_pages captures page counts and inconsistencies", {
  logs <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    unit_key = c("U1", "U1", "U1", "U1", "U2", "U2"),
    unit_alias = c("U1", "U1", "U1", "U1", "U2", "U2"),
    ts = c(100, 200, 300, 400, 500, 600),
    log_entry = c(
      "PAGE_COUNT = 2",
      "CURRENT_PAGE_ID = page_1",
      "CURRENT_PAGE_NR = 1",
      "CURRENT_PAGE_NR = 2",
      "PAGE_COUNT = 2",
      "CURRENT_PAGE_NR = 3"
    )
  )

  out <- summarise_log_pages(logs)
  u1 <- out[out$unit_key == "U1", ]
  u2 <- out[out$unit_key == "U2", ]

  expect_equal(u1$n_current_page_id_events, 1)
  expect_equal(u1$n_current_page_nr_events, 2)
  expect_equal(u1$page_count, 2)
  expect_equal(u1$max_current_page_nr, 2)
  expect_true(u1$page_count_consistent)
  expect_true(u1$observed_pages_complete)
  expect_true(u1$reached_last_page_nr)
  expect_false(u1$observed_page_nr_gaps)
  expect_true(is.na(u1$missing_page_nrs))
  expect_false(u1$page_nr_exceeds_page_count)

  expect_true(u2$page_nr_exceeds_page_count)
  expect_true(u2$reached_last_page_nr)
  expect_false(u2$observed_pages_complete)
  expect_true(u2$observed_page_nr_gaps)
  expect_equal(u2$missing_page_nrs, "1, 2")
})

test_that("summarise_log_pages distinguishes reaching the last page from complete observation", {
  logs <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    unit_key = "U1",
    unit_alias = "U1",
    ts = c(100, 200),
    log_entry = c(
      "PAGE_COUNT = 3",
      "CURRENT_PAGE_NR = 3"
    )
  )

  out <- summarise_log_pages(logs)

  expect_equal(out$observed_page_nrs, "3")
  expect_true(out$reached_last_page_nr)
  expect_false(out$observed_pages_complete)
  expect_true(out$observed_page_nr_gaps)
  expect_equal(out$missing_page_nrs, "1, 2")
})

test_that("summarise_log_progress returns final response and presentation progress", {
  logs <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    unit_key = "U1",
    unit_alias = "U1",
    ts = c(100, 200, 300, 400),
    log_entry = c(
      "RESPONSE_PROGRESS = none",
      "PRESENTATION_PROGRESS = some",
      "RESPONSE_PROGRESS = complete",
      "PRESENTATION_PROGRESS = complete"
    )
  )

  out <- summarise_log_progress(logs)

  expect_equal(out$n_response_progress_events, 2)
  expect_equal(out$n_presentation_progress_events, 2)
  expect_equal(out$first_response_progress, "none")
  expect_equal(out$final_response_progress, "complete")
  expect_equal(out$first_presentation_progress, "some")
  expect_equal(out$final_presentation_progress, "complete")
  expect_true(out$response_reached_complete)
  expect_true(out$presentation_reached_complete)
  expect_true(out$response_final_complete)
  expect_true(out$presentation_final_complete)
})

test_that("summarise_log_progress returns false final flags without progress events", {
  logs <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    unit_key = "U1",
    unit_alias = "U1",
    ts = 100,
    log_entry = "PLAYER = RUNNING"
  )

  out <- summarise_log_progress(logs)

  expect_equal(out$n_response_progress_events, 0)
  expect_equal(out$n_presentation_progress_events, 0)
  expect_false(out$response_reached_complete)
  expect_false(out$presentation_reached_complete)
  expect_false(out$response_final_complete)
  expect_false(out$presentation_final_complete)
})
