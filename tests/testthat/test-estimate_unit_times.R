# Test 1
test_that("estimate_unit_times returns tibble with expected columns", {
  # Create minimal test data
  logs <- tibble::tibble(
    group = "test_group",
    login = "user1",
    booklet_id = "BOOKLET_1",
    unit_key = "UNIT_1",
    unit_alias = "UNIT_1",
    ts = c(100, 200, 300, 400, 500),
    log_entry = c(
      "PLAYER = LOADING",
      "PLAYER = RUNNING",
      "FOCUS : \"HAS_NOT\"",
      "FOCUS : \"HAS\"",
      "PLAYER = LOADING"
    )
  )
  
  design <- tibble::tibble(
    booklet_id = "BOOKLET_1",
    unit_key = "UNIT_1",
    testlet_no = 1
  )
  
  result <- estimate_unit_times(logs, use_unit_alias = FALSE, 
                                full_design = design, block_self_switch = FALSE)
  
  expect_s3_class(result, "tbl_df")
  expect_true("unit_start_time" %in% names(result))
  expect_true("unit_loadtime" %in% names(result))
  expect_true("focus_events" %in% names(result))
  expect_true("unit_page_logs" %in% names(result))
  expect_true("unit_playbacks" %in% names(result))
})

# Test 2
test_that("estimate_unit_times returns tibble with expected columns,
          even without optional arguments", {
  # Create minimal test data
  logs <- tibble::tibble(
    group = "test_group",
    login = "user1",
    booklet_id = "BOOKLET_1",
    unit_key = "UNIT_1",
    unit_alias = "UNIT_1",
    ts = c(100, 200, 300, 400, 500),
    log_entry = c(
      "PLAYER = LOADING",
      "PLAYER = RUNNING",
      "FOCUS : \"HAS_NOT\"",
      "FOCUS : \"HAS\"",
      "PLAYER = LOADING"
    )
  )
  
  result <- estimate_unit_times(logs)
  
  expect_s3_class(result, "tbl_df")
  expect_true("unit_start_time" %in% names(result))
  expect_true("unit_loadtime" %in% names(result))
  expect_true("focus_events" %in% names(result))
  expect_true("unit_page_logs" %in% names(result))
  expect_true("unit_playbacks" %in% names(result))
})

# Test 3
test_that("estimate_unit_times calculates focus lost events correctly", {
  logs <- tibble::tibble(
    group = "test_group",
    login = "user1",
    booklet_id = "BOOKLET_1",
    unit_key = c("UNIT_1", "UNIT_1", "UNIT_1", "UNIT_1", 
                 "UNIT_2", "UNIT_2", "UNIT_2", "UNIT_2", 
                 "UNIT_3", "UNIT_3", "UNIT_3", "UNIT_3",
                 "UNIT_4"),
    unit_alias = NA,
    ts = c(100, 200, 250, 350, 400, 450, 500, 550, 650, 700, 800, 850, 900),
    log_entry = c(
      "PLAYER = LOADING",
      "FOCUS : \"HAS_NOT\"",
      "PLAYER = RUNNING",
      "FOCUS : \"HAS\"",
      "PLAYER = LOADING",
      "PLAYER = RUNNING",
      "FOCUS : \"HAS_NOT\"",
      "FOCUS : \"HAS_NOT\"",
      "PLAYER = LOADING",
      "PLAYER = RUNNING",
      "PLAYER = LOADING",
      "FOCUS : \"HAS_NOT\"",
      "PLAYER = LOADING"
    )
  )
  
  design <- tibble::tibble(
    booklet_id = "BOOKLET_1",
    unit_key = c("UNIT_1", "UNIT_1", "UNIT_1", "UNIT_1", 
                 "UNIT_2", "UNIT_2", "UNIT_2", "UNIT_2",
                 "UNIT_3", "UNIT_3", "UNIT_3", "UNIT_3",
                 "UNIT_4"),
    testlet_no = c(1,1,1,1,1,1,1,1,2,2,2,2,2)
  )
  
  result <- estimate_unit_times(logs, use_unit_alias = FALSE, 
                                full_design = design, block_self_switch = FALSE)
  
  # Check that focus_events tibble is populated
  focus_events1 <- result$focus_events[[1]]
  expect_s3_class(focus_events1, "tbl_df")
  expect_true("focus_event_ts" %in% names(focus_events1))
  expect_true("focus_event_type" %in% names(focus_events1))
  expect_true("focus_event_unfollowed" %in% names(focus_events1))
  expect_true(focus_events1$focus_lost_duration[1] == 150)
  focus_events2 <- result$focus_events[[2]]
  expect_true(is.na(focus_events2$focus_lost_duration[1]))
  expect_true(focus_events2$focus_lost_duration[2] == 250)
  focus_events3 <- result$focus_events[[3]]
  expect_true(focus_events3$focus_lost_duration[1] == 50)
})

# Test 4
test_that("estimate_unit_times respects booklet endings when flagging unfollowed focus events", {
  logs <- tibble::tibble(
    group = "test_group",
    login = "user1",
    booklet_id = "BOOKLET_1",
    unit_key = "UNIT_1",
    unit_alias = "UNIT_1",
    ts = c(100, 200, 300, 400),
    log_entry = c(
      "PLAYER = LOADING",
      "PLAYER = RUNNING",
      "FOCUS : \"HAS_NOT\"",
      "PLAYER = RUNNING"
      )
  )
  
  design <- tibble::tibble(
    booklet_id = "BOOKLET_1",
    unit_key = "UNIT_1",
    testlet_no = 1
  )
  
  result <- estimate_unit_times(logs, use_unit_alias = FALSE, 
                                full_design = design, block_self_switch = FALSE)
  
  focus_events <- result$focus_events[[1]]
  expect_true(!focus_events$focus_event_unfollowed[1])
})

# Test 5
test_that("estimate_unit_times handles multiple unit plays", {
  logs <- tibble::tibble(
    group = "test_group",
    login = "user1",
    booklet_id = "BOOKLET_1",
    unit_key = "UNIT_1",
    unit_alias = "UNIT_1",
    ts = c(100, 200, 300, 350, 450, 550),
    log_entry = c(
      "PLAYER = LOADING",
      "PLAYER = RUNNING",
      "PLAYER = LOADING",
      "PLAYER = RUNNING",
      "FOCUS : \"HAS_NOT\"",
      "PLAYER = LOADING"
    )
  )
  
  design <- tibble::tibble(
    booklet_id = "BOOKLET_1",
    unit_key = "UNIT_1",
    testlet_no = 1
  )
  
  result <- estimate_unit_times(logs, use_unit_alias = FALSE, 
                                full_design = design, block_self_switch = FALSE)
  
  expect_true("unit_playbacks" %in% names(result))
  playbacks <- result$unit_playbacks[[1]]
  expect_equal(nrow(playbacks), 2)
})

# Test 6
test_that("estimate_unit_times creates unit_ident column correctly", {
  logs <- tibble::tibble(
    group = "test_group",
    login = "user1",
    booklet_id = "BOOKLET_1",
    unit_key = "UNIT_1",
    unit_alias = "UNIT_ALIAS_1",
    ts = c(100, 200, 300),
    log_entry = c(
      "PLAYER = LOADING",
      "PLAYER = RUNNING",
      "PLAYER = LOADING"
    )
  )
  
  design <- tibble::tibble(
    booklet_id = "BOOKLET_1",
    unit_key = "UNIT_1",
    unit_alias = "UNIT_ALIAS_1",
    testlet_no = 1
  )
  
  # Test with use_unit_alias = FALSE
  result_key <- estimate_unit_times(logs, use_unit_alias = FALSE, 
                                    full_design = design, block_self_switch = FALSE)
  expect_true(result_key$unit_key == result_key$unit_ident)
  
  # Test with use_unit_alias = TRUE
  result_alias <- estimate_unit_times(logs, use_unit_alias = TRUE, 
                                      full_design = design, block_self_switch = FALSE)
  expect_true(result_alias$unit_alias == result_alias$unit_ident)
})

# Test 7
test_that("estimate_unit_times handles missing booklet_id entries", {
  logs <- tibble::tibble(
    group = "test_group",
    login = "user1",
    booklet_id = c("BOOKLET_1", NA, "BOOKLET_1", "BOOKLET_1"),
    unit_key = "UNIT_1",
    unit_alias = "UNIT_1",
    ts = c(100, 150, 200, 300),
    log_entry = c(
      "PLAYER = LOADING",
      "PLAYER = RUNNING",
      "PLAYER = RUNNING",
      "PLAYER = LOADING"
    )
  )
  
  design <- tibble::tibble(
    booklet_id = "BOOKLET_1",
    unit_key = "UNIT_1",
    testlet_no = 1
  )
  
  # Should not error and should exclude NA booklet_id entries
  result <- estimate_unit_times(logs, use_unit_alias = FALSE, 
                                full_design = design, block_self_switch = FALSE)
  expect_true(sum(is.na(result$booklet_id)) == 0)
})

# Test 8
test_that("estimate_unit_times returns NA focus_events when no focus events occur", {
  logs <- tibble::tibble(
    group = "test_group",
    login = "user1",
    booklet_id = "BOOKLET_1",
    unit_key = "UNIT_1",
    unit_alias = "UNIT_1",
    ts = c(100, 200, 300),
    log_entry = c(
      "PLAYER = LOADING",
      "PLAYER = RUNNING",
      "PLAYER = LOADING"
    )
  )
  
  design <- tibble::tibble(
    booklet_id = "BOOKLET_1",
    unit_key = "UNIT_1",
    testlet_no = 1
  )
  
  result <- estimate_unit_times(logs, use_unit_alias = FALSE, 
                                full_design = design, block_self_switch = FALSE)
  
  # focus_events should be NA when no focus events are recorded
  expect_true(is.na(result$focus_events[[1]]))
})

# Test 9
test_that("estimate_unit_times correctly calculates loading and playback times", {
  # Create a minimal test logs tibble with known loading and playback times
  logs <- tibble::tibble(
    group = c(rep("g1", 8)),
    login = c(rep("user1", 8)),
    booklet_id = c(rep("book1", 8)),
    unit_key = c("u1", "u1", "u1", "u1", "u2", "u2", "u2", "u2"),
    unit_alias = c("u1", "u1", "u1", "u1", "u2", "u2", "u2", "u2"),
    ts = c(1000, 2000, 2500, 5000, 5100, 6000, 6500, 9000),
    log_entry = c(
      "PLAYER = LOADING",       # ts=1000, unit_key="u1"
      "PLAYER = RUNNING",       # ts=2000, unit_key="u1" (loading_time = 2000-1000 = 1000)
      "PLAYER = LOADING",       # ts=2500, unit_key="u2"
      "SESSION END",            # ts=5000, session end (playback_time_u1 = 5000-2000 = 3000)
      "PLAYER = LOADING",       # ts=5100, unit_key="u2"
      "PLAYER = RUNNING",       # ts=6000, unit_key="u2" (loading_time = 6000-5100 = 900)
      "PLAYER = LOADING",       # ts=6500, unit_key="u1"
      "SESSION END"             # ts=9000, session end (playback_time_u2 = 9000-6000 = 3000)
    )
  )
  
  result <- estimate_unit_times(logs)
  
  # Verify that u1 has correct loading and playback times
  u1_row <- result[result$unit_key == "u1", ]
  expect_equal(nrow(u1_row), 1, info = "Should have one row for unit u1")
  expect_equal(u1_row$unit_loadtime, 1000)
  expect_equal(u1_row$unit_time, 500)
  expect_equal(u1_row$unit_n_play, 1, 
               info = "u1 should have 1 playback")
  
  # Verify that u2 has correct loading and playback times
  u2_row <- result[result$unit_key == "u2", ]
  expect_equal(nrow(u2_row), 1, info = "Should have one row for unit u2")
  expect_equal(u2_row$unit_loadtime, 900)
  expect_equal(u2_row$unit_time, 500)
  expect_equal(u2_row$unit_n_play, 1, 
               info = "u2 should have 1 playback")
})


# Test 10
test_that("estimate_unit_times correctly counts n_failed_loadings", {
  # Create logs with failed loading attempts
  logs <- tibble::tibble(
    group = c(rep("g1", 5)),
    login = c(rep("user1", 5)),
    booklet_id = c(rep("book1", 5)),
    unit_key = c("u1", "u1", "u1", "u1", "u1"),
    unit_alias = c("u1", "u1", "u1", "u1", "u1"),
    ts = c(1000, 1500, 2000, 3000, 4000),
    log_entry = c(
      "PLAYER = LOADING",       # ts=1000
      "PLAYER = LOADING",       # ts=1500 (duplicate attempt)
      "PLAYER = RUNNING",       # ts=2000
      "PLAYER = LOADING",       # ts=3000
      "PLAYER = LOADING"       # ts=4000 (duplicate attempt)
    )
  )
  
  result <- estimate_unit_times(logs)
  u1_row <- result[result$unit_key == "u1", ]
  expect_equal(u1_row$n_failed_loadings, 3,
               info = "Should count 3 loading attempts as failed loadings")
  
  expect_equal(u1_row$unit_n_play, 1)
})

# Test 11
test_that("estimate_unit_times handles multiple playbacks with different loading times", {
  # Create logs with multiple playbacks of the same unit
  logs <- tibble::tibble(
    group = c(rep("g1", 10)),
    login = c(rep("user1", 10)),
    booklet_id = c(rep("book1", 10)),
    unit_key = c("u1", "u1", "u1", "u1", "u1", "u1", "u1", "u1", "u1", "u1"),
    unit_alias = c("u1", "u1", "u1", "u1", "u1", "u1", "u1", "u1", "u1", "u1"),
    ts = c(1000, 2000, 3500, 4500, 5000, 6500, 7500, 8500, 9000, 12000),
    log_entry = c(
      "PLAYER = LOADING",       # ts=1000
      "PLAYER = RUNNING",       # ts=2000 
      "PLAYER = LOADING",       # ts=3500 
      "PLAYER = RUNNING",       # ts=4500 
      "PLAYER = LOADING",       # ts=5000
      "PLAYER = RUNNING",       # ts=6500 
      "PLAYER = LOADING",       # ts=7500 
      "PLAYER = LOADING",       # ts=8500 
      "PLAYER = RUNNING",       # ts=9000 
      "SESSION END"             # ts=12000
    )
  )
  
  result <- estimate_unit_times(logs)
  u1_row <- result[result$unit_key == "u1", ]
  
  # Expected total loading time = 1000 + 1000 + 1500 + 1500 = 5000
  expect_equal(u1_row$unit_loadtime, 5000,
               info = "Total loading time should be sum of all valid loading periods")
  
  # Expected total playback time = 1500 + 500 + 1000 + 3000 = 6000
  expect_equal(u1_row$unit_time, 6000,
               info = "Total playback time should be sum of all playback periods")
  
  # Should have 4 playbacks (duplicate loading should not create extra playback)
  expect_equal(u1_row$unit_n_play, 4,
               info = "Should count 4 playbacks (PLAYER = RUNNING occurrences)")
  
  # Check that unit_playbacks nested tibble has correct structure
  expect_true(!is.null(u1_row$unit_playbacks),
              info = "unit_playbacks should be populated")
  expect_equal(nrow(u1_row$unit_playbacks[[1]]), 4,
               info = "unit_playbacks should have 4 rows for 4 playbacks")
})

# Test 12
test_that("estimate_unit_times handles run_no_load cases correctly", {
  # Create logs where PLAYER = RUNNING occurs without prior PLAYER = LOADING
  logs <- tibble::tibble(
    group = c(rep("g1", 6)),
    login = c(rep("user1", 6)),
    booklet_id = c(rep("book1", 6)),
    unit_key = c("u1", "u1", "u2", "u2", "u2", "u2"),
    unit_alias = c("u1", "u1", "u2", "u2", "u2", "u2"),
    ts = c(1000, 2000, 3000, 4000, 5000, 6000),
    log_entry = c(
      "PLAYER = RUNNING",       # ts=1000 (no prior LOADING for u1 at start)
      "PLAYER = LOADING",       # ts=2000 (session end)
      "PLAYER = LOADING",       # ts=3000
      "PLAYER = RUNNING",       # ts=4000 (loading_time = 1000)
      "PLAYER = LOADING",       # ts=5000 (playback_time = 1000)
      "SESSION END"             # ts=6000 (loading_time = NA, playback_time = 1000)
    )
  )
  
  result <- estimate_unit_times(logs)
  
  u1_row <- result[result$unit_key == "u1", ]
  u2_row <- result[result$unit_key == "u2", ]
  
  # u1 should have run_no_load = TRUE, so loading time is NA
  expect_equal(u1_row$unit_loadtime, NA_real_,
               info = "u1 loading time should be NA when RUNNING occurs without prior LOADING")
  
  # u2 should have normal loading time
  expect_equal(u2_row$unit_loadtime, 1000,
               info = "u2 loading time should be 1000 (4000 - 3000)")
  
  # Check run_no_load_i in unit_playbacks
  expect_true(u1_row$unit_playbacks[[1]]$run_no_load_i[[1]],
              info = "u1's first playback should have run_no_load_i = TRUE")
  expect_false(u2_row$unit_playbacks[[1]]$run_no_load_i[[1]],
               info = "u2's first playback should have run_no_load_i = FALSE")
})

