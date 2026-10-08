completion_progress_fixture <- function() {
  design <- tibble::tibble(
    login_code = "P1", booklet_id = "B1", booklet_no = 1L, testlet_no = 1L,
    unit_booklet_no = 1L, unit_key = "U1", unit_alias = "U1", variable_id = "V1"
  )
  list(units = minimal_units(), design = design,
       coded = dplyr::mutate(design, code_status = "CODING_COMPLETE",
                            value = "A", code_type = "FULL_CREDIT", code_id = 1,
                            code_score = 1))
}

mock_missing_progress_window <- function(.env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$opened <- 0L
  state$closed <- 0L
  state$time <- 0
  state$updates <- list()
  testthat::local_mocked_bindings(
    missing_progress_backend = function() "windows",
    missing_progress_clock = function() state$time,
    missing_progress_window_open = function(label) {
      state$opened <- state$opened + 1L
      state
    },
    missing_progress_window_update = function(handle, value, label) {
      state$updates <- c(state$updates, list(list(value = value, label = label)))
    },
    missing_progress_window_close = function(handle) {
      state$closed <- state$closed + 1L
    },
    .env = .env
  )
  state
}

test_that("one progress window preserves results and closes on success and failure", {
  window <- mock_missing_progress_window()
  f <- completion_progress_fixture()
  for (overwrite in c(FALSE, TRUE)) {
    for (policy in c("eatPrepTBA", "coding_box")) {
      args <- c(f, list(identifiers = "login_code", diagnostics = "none",
                       missing_policy = policy, overwrite = overwrite,
                       not_reached_scope = "testlet",
                       recode_omissions_to_not_reached = TRUE))
      opened <- window$opened
      silent <- suppressMessages(do.call(complete_design, c(args, list(progress = FALSE))))
      expect_equal(window$opened, opened)
      visible <- suppressMessages(do.call(complete_design, c(args, list(progress = TRUE))))
      expect_identical(visible, silent)
      expect_equal(window$opened, opened + 1L)
      expect_equal(window$closed, window$opened)
      expect_null(.missing_progress_state$active)
    }
  }
  f$design$variable_id <- "UNKNOWN"
  expect_error(suppressMessages(do.call(complete_design,
    c(f, list(progress = TRUE, unknown_variables = "error")))),
    class = "eatPrepTBA_design_metadata_error")
  expect_equal(window$closed, window$opened)
  expect_null(.missing_progress_state$active)
})

test_that("nested phases start immediately, throttle redraws and measure elapsed time", {
  window <- mock_missing_progress_window()
  local({
    session <- missing_progress_session_start()
    on.exit(missing_progress_session_done(session), add = TRUE)
    phase <- missing_progress_start("Preparation")
    expect_length(window$updates, 1L)
    expect_match(window$updates[[1L]]$label, "phase 0.0 s", fixed = TRUE)
    window$time <- 2
    child <- missing_progress_start("Variables", 2000)
    expect_equal(window$opened, 1L)
    before <- length(window$updates)
    for (i in seq_len(1000L)) missing_progress_update(child)
    expect_length(window$updates, before)
    window$time <- 2.3
    missing_progress_update(child)
    expect_length(window$updates, before + 1L)
    expect_equal(utils::tail(window$updates, 1L)[[1L]]$value, 1001 / 2000)
    expect_match(utils::tail(window$updates, 1L)[[1L]]$label,
                 "phase 0.3 s | total 2.3 s", fixed = TRUE)
    window$time <- 9
    missing_progress_update(phase)
    expect_length(window$updates, before + 1L)
    missing_progress_update(child)
    expect_length(window$updates, before + 2L)
    expect_equal(utils::tail(window$updates, 1L)[[1L]]$value, 1002 / 2000)
    window$time <- 12
    missing_progress_done(child)
    expect_match(utils::tail(window$updates, 1L)[[1L]]$label,
                 "Preparation | phase 12.0 s | total 12.0 s", fixed = TRUE)
    missing_progress_done(phase)
  })
  expect_equal(window$closed, 1L)
  expect_null(.missing_progress_state$active)
})

test_that("standalone progress closes on errors and interrupts without claiming completion", {
  window <- mock_missing_progress_window()
  run <- function(interrupted) {
    stage <- missing_progress_start("Variables", 100)
    missing_progress_update(stage)
    if (interrupted) {
      stop(structure(list(message = "Interrupted", call = NULL),
                     class = c("interrupt", "condition")))
    }
    stop("Failed")
  }
  expect_error(run(FALSE), "Failed")
  interrupted <- tryCatch(run(TRUE), interrupt = identity)
  expect_s3_class(interrupted, "interrupt")
  expect_equal(window$opened, 2L)
  expect_equal(window$closed, 2L)
  expect_null(.missing_progress_state$active)
  expect_false(any(vapply(window$updates, function(x) x$value == 1, logical(1))))
})

test_that("closing a progress window falls back to text without breaking work", {
  window <- mock_missing_progress_window()
  testthat::local_mocked_bindings(missing_progress_window_update = function(...) stop("Closed"))
  messages <- character()
  result <- withCallingHandlers(local({
    stage <- missing_progress_start("Variables", 1)
    missing_progress_update(stage)
    missing_progress_done(stage)
    42
  }), message = function(cnd) {
    messages <<- c(messages, conditionMessage(cnd))
    invokeRestart("muffleMessage")
  })
  expect_equal(result, 42)
  expect_equal(window$opened, 1L)
  expect_equal(window$closed, 1L)
  expect_null(.missing_progress_state$active)
  expect_true(any(grepl("Variables", messages, fixed = TRUE)))
})

test_that("text progress is timed and progress FALSE opens no display", {
  window <- mock_missing_progress_window()
  testthat::local_mocked_bindings(missing_progress_backend = function() "text")
  messages <- character()
  withCallingHandlers(local({
    expect_null(missing_progress_session_start(FALSE))
    expect_null(missing_progress_start("Hidden", 100, FALSE))
    stage <- missing_progress_start("Variables", 2000)
    for (i in seq_len(1000L)) missing_progress_update(stage)
    expect_length(messages, 1L)
    window$time <- 5
    missing_progress_update(stage)
    expect_length(messages, 2L)
    missing_progress_done(stage)
  }), message = function(cnd) {
    messages <<- c(messages, conditionMessage(cnd))
    invokeRestart("muffleMessage")
  })
  expect_equal(window$opened, 0L)
  expect_null(.missing_progress_state$active)
  expect_false(any(grepl("\\033|\\r", messages)))
})

test_that("reused unit ordering respects an override in only one booklet", {
  f <- completion_progress_fixture()
  first <- dplyr::mutate(minimal_unit_codes(), variable_page = 1L)
  second <- dplyr::mutate(first, variable_id = "V2", variable_ref = "v2",
                          variable_page = 2L)
  f$units$unit_codes[[1L]] <- dplyr::bind_rows(first, second)
  design <- dplyr::bind_rows(f$design, dplyr::mutate(f$design, booklet_id = "B2"),
                            dplyr::mutate(f$design, booklet_id = "B3"))
  overrides <- tibble::tibble(unit_key = "U1", booklet_id = "B2",
                              variable_id = c("V1", "V2"), local_order = c(2L, 1L))
  out <- get_design_order(design, f$units, order_method = "structure",
                           order_overrides = overrides)
  expect_equal(out$variable_id[out$booklet_id == "B1"], c("V1", "V2"))
  expect_equal(out$variable_id[out$booklet_id == "B2"], c("V2", "V1"))
  expect_equal(out$variable_id[out$booklet_id == "B3"], c("V1", "V2"))
  expect_equal(out$variable_order, rep(1:2, 3))
})
