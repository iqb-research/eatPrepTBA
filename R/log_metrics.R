log_metric_groups <- function(logs, session_cols, unit_cols = character()) {
  group_cols <- unique(c(session_cols, unit_cols))

  if (length(unit_cols) > 0) {
    logs <- logs %>%
      dplyr::filter(
        dplyr::if_any(
          dplyr::all_of(unit_cols),
          ~ !is.na(.x) & .x != ""
        )
      )
  }

  if (length(group_cols) == 0) {
    return(tibble::tibble(.rows = 1L))
  }

  dplyr::distinct(logs, dplyr::across(dplyr::all_of(group_cols)))
}

log_metric_event_data <- function(logs) {
  logs %>%
    dplyr::mutate(
      .log_row = dplyr::row_number(),
      .ts_num = if ("ts" %in% names(logs)) suppressWarnings(as.numeric(.data$ts)) else NA_real_,
      log_type = log_extract_type(.data$log_entry),
      log_type_upper = stringr::str_to_upper(.data$log_type)
    )
}

log_metric_join <- function(base, summary, by) {
  if (length(by) == 0) {
    return(summary)
  }

  dplyr::left_join(base, summary, by = by)
}

log_metric_zero <- function(x) {
  dplyr::coalesce(x, 0L)
}

log_metric_zero_numeric <- function(x) {
  dplyr::coalesce(x, 0)
}

log_first_by_time <- function(value, time, row) {
  keep <- !is.na(value) & value != ""
  if (!any(keep)) {
    return(NA_character_)
  }

  order <- order(time[keep], row[keep], na.last = TRUE)
  as.character(value[keep][order][[1]])
}

log_last_by_time <- function(value, time, row) {
  keep <- !is.na(value) & value != ""
  if (!any(keep)) {
    return(NA_character_)
  }

  order <- order(time[keep], row[keep], na.last = TRUE)
  as.character(value[keep][order][[length(order)]])
}

log_count_state <- function(x, state) {
  sum(x == state, na.rm = TRUE)
}

#' Summarise connection state logs
#'
#' @param logs Tibble. Logs retrieved with [get_logs()] or read with [read_logs()].
#' @param session_cols Optional character vector with columns defining a test
#' session. By default, all available session columns are used.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Summarises `CONNECTION` events per test session. These values describe
#' Testcenter connection states and transitions; they are not download-speed or
#' bandwidth measurements.
#'
#' @return A tibble with one row per session.
#'
#' @export
summarise_log_connections <- function(logs, session_cols = NULL) {
  checkmate::assert_tibble(logs)
  assert_cols(logs, "log_entry", "logs")

  if (is.null(session_cols)) {
    session_cols <- log_session_cols(logs)
  }
  checkmate::assert_character(session_cols, null.ok = FALSE)
  assert_cols(logs, session_cols, "logs")

  base <- log_metric_groups(logs, session_cols)
  prep <- log_metric_event_data(logs) %>%
    dplyr::mutate(connection_state = stringr::str_to_upper(log_parse_connection(.data$log_entry))) %>%
    dplyr::filter(.data$log_type_upper == "CONNECTION") %>%
    dplyr::arrange(
      dplyr::across(dplyr::any_of(session_cols)),
      .data$.ts_num,
      .data$.log_row
    ) %>%
    log_group_by_cols(session_cols) %>%
    dplyr::mutate(connection_transition = .data$connection_state != dplyr::lag(.data$connection_state)) %>%
    dplyr::ungroup()

  summary <- prep %>%
    log_group_by_cols(session_cols) %>%
    dplyr::summarise(
      n_connection_events = dplyr::n(),
      n_connection_transitions = sum(.data$connection_transition %in% TRUE, na.rm = TRUE),
      first_connection_ts = log_min_or_na(.data$.ts_num),
      last_connection_ts = log_max_or_na(.data$.ts_num),
      first_connection_state = log_first_by_time(.data$connection_state, .data$.ts_num, .data$.log_row),
      last_connection_state = log_last_by_time(.data$connection_state, .data$.ts_num, .data$.log_row),
      connection_states = log_collapse_values(.data$connection_state, max_values = 10L),
      n_connection_lost = log_count_state(.data$connection_state, "LOST"),
      n_connection_ok = log_count_state(.data$connection_state, "OK"),
      n_connection_polling = log_count_state(.data$connection_state, "POLLING"),
      n_connection_websocket = log_count_state(.data$connection_state, "WEBSOCKET"),
      first_connection_lost_ts = log_min_or_na(.data$.ts_num[.data$connection_state == "LOST"]),
      last_connection_lost_ts = log_max_or_na(.data$.ts_num[.data$connection_state == "LOST"]),
      .groups = "drop"
    )

  log_metric_join(base, summary, session_cols) %>%
    dplyr::mutate(
      n_connection_events = log_metric_zero(.data$n_connection_events),
      n_connection_transitions = log_metric_zero(.data$n_connection_transitions),
      n_connection_lost = log_metric_zero(.data$n_connection_lost),
      n_connection_ok = log_metric_zero(.data$n_connection_ok),
      n_connection_polling = log_metric_zero(.data$n_connection_polling),
      n_connection_websocket = log_metric_zero(.data$n_connection_websocket),
      has_connection_lost = .data$n_connection_lost > 0L,
      last_connection_state_lost = .data$last_connection_state == "LOST"
    )
}

#' Summarise focus state logs
#'
#' @param logs Tibble. Logs retrieved with [get_logs()] or read with [read_logs()].
#' @param session_cols Optional character vector with columns defining a test
#' session. By default, all available session columns are used.
#' @param focus_loss_threshold_ms Numeric. Focus losses longer than this
#' threshold are counted as very long focus losses.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Summarises raw `FOCUS` events per session. Durations are computed from
#' `FOCUS = HAS_NOT` until the next raw `FOCUS = HAS` event.
#'
#' @return A tibble with one row per session.
#'
#' @export
summarise_log_focus <- function(logs,
                                session_cols = NULL,
                                focus_loss_threshold_ms = 5 * 60 * 1000) {
  checkmate::assert_tibble(logs)
  assert_cols(logs, "log_entry", "logs")
  checkmate::assert_number(focus_loss_threshold_ms, lower = 0)

  if (is.null(session_cols)) {
    session_cols <- log_session_cols(logs)
  }
  checkmate::assert_character(session_cols, null.ok = FALSE)
  assert_cols(logs, session_cols, "logs")

  base <- log_metric_groups(logs, session_cols)
  prep <- log_metric_event_data(logs) %>%
    dplyr::mutate(focus_state = stringr::str_to_upper(log_parse_event_value(.data$log_entry, "FOCUS"))) %>%
    dplyr::filter(.data$log_type_upper == "FOCUS", .data$focus_state %in% c("HAS_NOT", "HAS")) %>%
    dplyr::arrange(
      dplyr::across(dplyr::any_of(session_cols)),
      .data$.ts_num,
      .data$.log_row
    ) %>%
    log_group_by_cols(session_cols) %>%
    dplyr::mutate(
      next_focus_state = dplyr::lead(.data$focus_state),
      next_focus_ts = dplyr::lead(.data$.ts_num),
      focus_lost_duration = dplyr::case_when(
        .data$focus_state == "HAS_NOT" & .data$next_focus_state == "HAS" ~ .data$next_focus_ts - .data$.ts_num,
        TRUE ~ NA_real_
      ),
      has_focus_at_or_after = rev(cumsum(rev(.data$focus_state == "HAS")) > 0L),
      has_later_focus_regain = dplyr::lead(.data$has_focus_at_or_after, default = FALSE)
    ) %>%
    dplyr::ungroup()

  summary <- prep %>%
    log_group_by_cols(session_cols) %>%
    dplyr::summarise(
      n_focus_events = dplyr::n(),
      n_focus_lost = log_count_state(.data$focus_state, "HAS_NOT"),
      n_focus_regained = log_count_state(.data$focus_state, "HAS"),
      n_focus_loss_intervals = sum(!is.na(.data$focus_lost_duration)),
      n_focus_lost_never_regained = sum(
        .data$focus_state == "HAS_NOT" & !.data$has_later_focus_regain,
        na.rm = TRUE
      ),
      n_repeated_focus_lost_before_regain = sum(
        .data$focus_state == "HAS_NOT" & .data$next_focus_state == "HAS_NOT",
        na.rm = TRUE
      ),
      total_focus_lost_time = sum(.data$focus_lost_duration, na.rm = TRUE),
      max_focus_lost_time = log_max_or_na(.data$focus_lost_duration),
      mean_focus_lost_time = if (all(is.na(.data$focus_lost_duration))) {
        NA_real_
      } else {
        mean(.data$focus_lost_duration, na.rm = TRUE)
      },
      n_very_long_focus_loss = sum(.data$focus_lost_duration > focus_loss_threshold_ms, na.rm = TRUE),
      first_focus_lost_ts = log_min_or_na(.data$.ts_num[.data$focus_state == "HAS_NOT"]),
      last_focus_lost_ts = log_max_or_na(.data$.ts_num[.data$focus_state == "HAS_NOT"]),
      .groups = "drop"
    )

  log_metric_join(base, summary, session_cols) %>%
    dplyr::mutate(
      n_focus_events = log_metric_zero(.data$n_focus_events),
      n_focus_lost = log_metric_zero(.data$n_focus_lost),
      n_focus_regained = log_metric_zero(.data$n_focus_regained),
      n_focus_loss_intervals = log_metric_zero(.data$n_focus_loss_intervals),
      n_focus_lost_never_regained = log_metric_zero(.data$n_focus_lost_never_regained),
      n_repeated_focus_lost_before_regain = log_metric_zero(.data$n_repeated_focus_lost_before_regain),
      total_focus_lost_time = log_metric_zero_numeric(.data$total_focus_lost_time),
      n_very_long_focus_loss = log_metric_zero(.data$n_very_long_focus_loss),
      has_focus_loss = .data$n_focus_lost > 0L,
      has_unresolved_focus_loss = .data$n_focus_lost_never_regained > 0L
    )
}

#' Summarise player state logs
#'
#' @param logs Tibble. Logs retrieved with [get_logs()] or read with [read_logs()].
#' @param session_cols Optional character vector with columns defining a test
#' session. By default, all available session columns are used.
#' @param unit_cols Optional character vector with columns defining units. By
#' default, available columns among `unit_key` and `unit_alias` are used.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Counts raw `PLAYER` state events per session/unit. This is a compact state
#' summary and does not replace [estimate_unit_times()] for duration estimates.
#'
#' @return A tibble with one row per session/unit.
#'
#' @export
summarise_log_player <- function(logs, session_cols = NULL, unit_cols = NULL) {
  checkmate::assert_tibble(logs)
  assert_cols(logs, "log_entry", "logs")

  if (is.null(session_cols)) {
    session_cols <- log_session_cols(logs)
  }
  if (is.null(unit_cols)) {
    unit_cols <- log_unit_cols(logs)
  }
  checkmate::assert_character(session_cols, null.ok = FALSE)
  checkmate::assert_character(unit_cols, null.ok = TRUE)
  assert_cols(logs, session_cols, "logs")
  if (length(unit_cols) > 0) {
    assert_cols(logs, unit_cols, "logs")
  }

  group_cols <- unique(c(session_cols, unit_cols))
  base <- log_metric_groups(logs, session_cols, unit_cols)
  prep <- log_metric_event_data(logs) %>%
    dplyr::mutate(player_state = stringr::str_to_upper(log_parse_event_value(.data$log_entry, "PLAYER"))) %>%
    dplyr::filter(.data$log_type_upper == "PLAYER") %>%
    dplyr::arrange(
      dplyr::across(dplyr::any_of(group_cols)),
      .data$.ts_num,
      .data$.log_row
    )

  summary <- prep %>%
    log_group_by_cols(group_cols) %>%
    dplyr::summarise(
      n_player_events = dplyr::n(),
      n_player_loading = log_count_state(.data$player_state, "LOADING"),
      n_player_running = log_count_state(.data$player_state, "RUNNING"),
      n_player_paused = log_count_state(.data$player_state, "PAUSED"),
      player_states = log_collapse_values(.data$player_state, max_values = 10L),
      first_player_state = log_first_by_time(.data$player_state, .data$.ts_num, .data$.log_row),
      last_player_state = log_last_by_time(.data$player_state, .data$.ts_num, .data$.log_row),
      first_player_ts = log_min_or_na(.data$.ts_num),
      last_player_ts = log_max_or_na(.data$.ts_num),
      first_loading_ts = log_min_or_na(.data$.ts_num[.data$player_state == "LOADING"]),
      first_running_ts = log_min_or_na(.data$.ts_num[.data$player_state == "RUNNING"]),
      .groups = "drop"
    )

  log_metric_join(base, summary, group_cols) %>%
    dplyr::mutate(
      n_player_events = log_metric_zero(.data$n_player_events),
      n_player_loading = log_metric_zero(.data$n_player_loading),
      n_player_running = log_metric_zero(.data$n_player_running),
      n_player_paused = log_metric_zero(.data$n_player_paused),
      has_player_running = .data$n_player_running > 0L,
      has_player_loading = .data$n_player_loading > 0L
    )
}

#' Summarise page state logs
#'
#' @param logs Tibble. Logs retrieved with [get_logs()] or read with [read_logs()].
#' @param session_cols Optional character vector with columns defining a test
#' session. By default, all available session columns are used.
#' @param unit_cols Optional character vector with columns defining units. By
#' default, available columns among `unit_key` and `unit_alias` are used.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Summarises raw `CURRENT_PAGE_ID`, `CURRENT_PAGE_NR`, and `PAGE_COUNT` events
#' per session/unit.
#'
#' @return A tibble with one row per session/unit.
#'
#' @export
summarise_log_pages <- function(logs, session_cols = NULL, unit_cols = NULL) {
  checkmate::assert_tibble(logs)
  assert_cols(logs, "log_entry", "logs")

  if (is.null(session_cols)) {
    session_cols <- log_session_cols(logs)
  }
  if (is.null(unit_cols)) {
    unit_cols <- log_unit_cols(logs)
  }
  checkmate::assert_character(session_cols, null.ok = FALSE)
  checkmate::assert_character(unit_cols, null.ok = TRUE)
  assert_cols(logs, session_cols, "logs")
  if (length(unit_cols) > 0) {
    assert_cols(logs, unit_cols, "logs")
  }

  group_cols <- unique(c(session_cols, unit_cols))
  base <- log_metric_groups(logs, session_cols, unit_cols)
  prep <- log_metric_event_data(logs) %>%
    dplyr::mutate(
      current_page_id = log_parse_event_value(.data$log_entry, "CURRENT_PAGE_ID"),
      current_page_nr = log_parse_event_number(.data$log_entry, "CURRENT_PAGE_NR"),
      page_count = log_parse_event_number(.data$log_entry, "PAGE_COUNT")
    ) %>%
    dplyr::filter(.data$log_type_upper %in% c("CURRENT_PAGE_ID", "CURRENT_PAGE_NR", "PAGE_COUNT"))

  summary <- prep %>%
    log_group_by_cols(group_cols) %>%
    dplyr::summarise(
      n_current_page_id_events = sum(.data$log_type_upper == "CURRENT_PAGE_ID", na.rm = TRUE),
      n_current_page_nr_events = sum(.data$log_type_upper == "CURRENT_PAGE_NR", na.rm = TRUE),
      n_page_count_events = sum(.data$log_type_upper == "PAGE_COUNT", na.rm = TRUE),
      observed_page_ids = log_collapse_values(.data$current_page_id, max_values = 20L),
      observed_page_nrs = log_collapse_values(.data$current_page_nr, max_values = 20L),
      n_observed_page_nrs = log_n_distinct_non_missing(.data$current_page_nr),
      first_current_page_nr = suppressWarnings(as.numeric(
        log_first_by_time(as.character(.data$current_page_nr), .data$.ts_num, .data$.log_row)
      )),
      final_current_page_nr = suppressWarnings(as.numeric(
        log_last_by_time(as.character(.data$current_page_nr), .data$.ts_num, .data$.log_row)
      )),
      min_current_page_nr = log_min_or_na(.data$current_page_nr),
      max_current_page_nr = log_max_or_na(.data$current_page_nr),
      page_count = log_first_non_missing(.data$page_count),
      n_page_count_values = log_n_distinct_non_missing(.data$page_count),
      max_page_count = log_max_or_na(.data$page_count),
      first_page_ts = log_min_or_na(.data$.ts_num),
      last_page_ts = log_max_or_na(.data$.ts_num),
      .groups = "drop"
    )

  log_metric_join(base, summary, group_cols) %>%
    dplyr::mutate(
      n_current_page_id_events = log_metric_zero(.data$n_current_page_id_events),
      n_current_page_nr_events = log_metric_zero(.data$n_current_page_nr_events),
      n_page_count_events = log_metric_zero(.data$n_page_count_events),
      n_observed_page_nrs = log_metric_zero(.data$n_observed_page_nrs),
      n_page_count_values = log_metric_zero(.data$n_page_count_values),
      page_count_consistent = .data$n_page_count_values <= 1L,
      page_nr_exceeds_page_count = !is.na(.data$max_current_page_nr) &
        !is.na(.data$max_page_count) &
        .data$max_current_page_nr > .data$max_page_count,
      observed_pages_complete = !is.na(.data$max_current_page_nr) &
        !is.na(.data$max_page_count) &
        .data$max_current_page_nr >= .data$max_page_count
    )
}

#' Summarise response and presentation progress logs
#'
#' @param logs Tibble. Logs retrieved with [get_logs()] or read with [read_logs()].
#' @param session_cols Optional character vector with columns defining a test
#' session. By default, all available session columns are used.
#' @param unit_cols Optional character vector with columns defining units. By
#' default, available columns among `unit_key` and `unit_alias` are used.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Summarises raw `RESPONSE_PROGRESS` and `PRESENTATION_PROGRESS` events per
#' session/unit.
#'
#' @return A tibble with one row per session/unit.
#'
#' @export
summarise_log_progress <- function(logs, session_cols = NULL, unit_cols = NULL) {
  checkmate::assert_tibble(logs)
  assert_cols(logs, "log_entry", "logs")

  if (is.null(session_cols)) {
    session_cols <- log_session_cols(logs)
  }
  if (is.null(unit_cols)) {
    unit_cols <- log_unit_cols(logs)
  }
  checkmate::assert_character(session_cols, null.ok = FALSE)
  checkmate::assert_character(unit_cols, null.ok = TRUE)
  assert_cols(logs, session_cols, "logs")
  if (length(unit_cols) > 0) {
    assert_cols(logs, unit_cols, "logs")
  }

  group_cols <- unique(c(session_cols, unit_cols))
  base <- log_metric_groups(logs, session_cols, unit_cols)
  prep <- log_metric_event_data(logs) %>%
    dplyr::mutate(
      response_progress = stringr::str_to_lower(log_parse_event_value(.data$log_entry, "RESPONSE_PROGRESS")),
      presentation_progress = stringr::str_to_lower(log_parse_event_value(.data$log_entry, "PRESENTATION_PROGRESS"))
    ) %>%
    dplyr::filter(.data$log_type_upper %in% c("RESPONSE_PROGRESS", "PRESENTATION_PROGRESS"))

  summary <- prep %>%
    log_group_by_cols(group_cols) %>%
    dplyr::summarise(
      n_response_progress_events = sum(.data$log_type_upper == "RESPONSE_PROGRESS", na.rm = TRUE),
      n_presentation_progress_events = sum(.data$log_type_upper == "PRESENTATION_PROGRESS", na.rm = TRUE),
      response_progress_values = log_collapse_values(.data$response_progress, max_values = 10L),
      presentation_progress_values = log_collapse_values(.data$presentation_progress, max_values = 10L),
      first_response_progress = log_first_by_time(.data$response_progress, .data$.ts_num, .data$.log_row),
      final_response_progress = log_last_by_time(.data$response_progress, .data$.ts_num, .data$.log_row),
      first_presentation_progress = log_first_by_time(.data$presentation_progress, .data$.ts_num, .data$.log_row),
      final_presentation_progress = log_last_by_time(.data$presentation_progress, .data$.ts_num, .data$.log_row),
      first_response_progress_ts = log_min_or_na(.data$.ts_num[!is.na(.data$response_progress)]),
      final_response_progress_ts = log_max_or_na(.data$.ts_num[!is.na(.data$response_progress)]),
      first_presentation_progress_ts = log_min_or_na(.data$.ts_num[!is.na(.data$presentation_progress)]),
      final_presentation_progress_ts = log_max_or_na(.data$.ts_num[!is.na(.data$presentation_progress)]),
      response_complete_ts = log_min_or_na(.data$.ts_num[.data$response_progress == "complete"]),
      presentation_complete_ts = log_min_or_na(.data$.ts_num[.data$presentation_progress == "complete"]),
      .groups = "drop"
    )

  log_metric_join(base, summary, group_cols) %>%
    dplyr::mutate(
      n_response_progress_events = log_metric_zero(.data$n_response_progress_events),
      n_presentation_progress_events = log_metric_zero(.data$n_presentation_progress_events),
      response_reached_complete = !is.na(.data$response_complete_ts),
      presentation_reached_complete = !is.na(.data$presentation_complete_ts),
      response_final_complete = .data$final_response_progress == "complete",
      presentation_final_complete = .data$final_presentation_progress == "complete"
    )
}
