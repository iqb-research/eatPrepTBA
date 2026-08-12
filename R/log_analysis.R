# Shared helpers for log parsing and log quality summaries.

log_known_types <- function() {
  c(
    "LOADCOMPLETE",
    "CONNECTION",
    "CURRENT_UNIT_ID",
    "TESTLETS_TIMELEFT",
    "TESTLETS_CLEARED_CODE",
    "TESTLETS_LOCKED_AFTER_LEAVE",
    "BOOKLET_STATES",
    "UNITS_LOCKED_AFTER_LEAVE",
    "FOCUS",
    "CONTROLLER",
    "SHARED_PARAMETERS",
    "PLAYER",
    "CURRENT_PAGE_ID",
    "CURRENT_PAGE_NR",
    "PAGE_COUNT",
    "PRESENTATION_PROGRESS",
    "RESPONSE_PROGRESS",
    "RUNTIME ERROR"
  )
}

log_supported_parser_types <- function() {
  c(
    "LOADCOMPLETE",
    "CONNECTION",
    "CURRENT_UNIT_ID",
    "FOCUS",
    "CONTROLLER",
    "PLAYER",
    "CURRENT_PAGE_ID",
    "CURRENT_PAGE_NR",
    "PAGE_COUNT",
    "PRESENTATION_PROGRESS",
    "RESPONSE_PROGRESS"
  )
}

log_session_cols <- function(data) {
  intersect(
    c("group_id", "group", "login_name", "login", "login_code", "booklet_id"),
    names(data)
  )
}

log_person_cols <- function(data) {
  intersect(c("group_id", "group", "login_name", "login", "login_code"), names(data))
}

log_unit_cols <- function(data) {
  intersect(c("unit_key", "unit_alias"), names(data))
}

log_make_key <- function(data, cols) {
  if (length(cols) == 0) {
    return(rep(NA_character_, nrow(data)))
  }

  do.call(paste, c(data[cols], sep = "\r"))
}

log_first_non_missing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) {
    if (is.numeric(x) || is.integer(x)) {
      return(NA_real_)
    }
    if (is.logical(x)) {
      return(NA)
    }
    return(NA_character_)
  }
  x[[1]]
}

log_n_distinct_non_missing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) {
    return(0L)
  }
  dplyr::n_distinct(x)
}

log_min_or_na <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  min(x, na.rm = TRUE)
}

log_max_or_na <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

log_extract_type <- function(log_entry) {
  x <- trimws(as.character(log_entry))
  x <- stringr::str_replace(
    x,
    "^\"?([^:=\"]+)\"?\\s*[:=].*$",
    "\\1"
  )
  x <- stringr::str_remove_all(x, "^\"|\"$")
  x <- dplyr::case_when(
    stringr::str_detect(x, stringr::regex("^Runtime Error", ignore_case = TRUE)) ~ "Runtime Error",
    TRUE ~ x
  )
  trimws(x)
}

log_event_family <- function(log_type) {
  type_upper <- stringr::str_to_upper(log_type)

  dplyr::case_when(
    is.na(log_type) | log_type == "" ~ NA_character_,
    type_upper == "LOADCOMPLETE" ~ "environment",
    type_upper == "CONNECTION" ~ "connection",
    type_upper == "FOCUS" ~ "focus",
    type_upper == "CONTROLLER" ~ "controller",
    type_upper == "PLAYER" ~ "player",
    type_upper %in% c(
      "CURRENT_PAGE_ID",
      "CURRENT_PAGE_NR",
      "PAGE_COUNT",
      "PRESENTATION_PROGRESS",
      "RESPONSE_PROGRESS"
    ) ~ "unit_state",
    type_upper %in% c(
      "CURRENT_UNIT_ID",
      "TESTLETS_TIMELEFT",
      "TESTLETS_CLEARED_CODE",
      "TESTLETS_LOCKED_AFTER_LEAVE",
      "BOOKLET_STATES",
      "UNITS_LOCKED_AFTER_LEAVE",
      "SHARED_PARAMETERS"
    ) ~ "test_state",
    type_upper == "RUNTIME ERROR" ~ "runtime_error",
    log_type == "command executed" ~ "monitor",
    TRUE ~ "other"
  )
}

log_has_supported_parser <- function(log_type) {
  type_upper <- stringr::str_to_upper(log_type)
  type_upper %in% log_supported_parser_types()
}

log_is_known_type <- function(log_type) {
  type_upper <- stringr::str_to_upper(log_type)
  type_upper %in% log_known_types() | log_type == "command executed"
}

log_parse_connection <- function(log_entry) {
  is_connection <- log_extract_type(log_entry) == "CONNECTION"
  out <- rep(NA_character_, length(log_entry))

  if (!any(is_connection, na.rm = TRUE)) {
    return(out)
  }

  payload <- trimws(stringr::str_remove(
    log_entry[is_connection],
    "^\"?CONNECTION\"?\\s*:\\s*"
  ))
  payload <- stringr::str_remove_all(payload, "^\"|\"$")
  out[is_connection] <- payload
  out
}

log_parse_event_value <- function(log_entry, type) {
  type_pattern <- stringr::str_replace_all(type, "([^A-Za-z0-9_])", "\\\\\\1")
  is_type <- stringr::str_to_upper(log_extract_type(log_entry)) == stringr::str_to_upper(type)
  out <- rep(NA_character_, length(log_entry))

  if (!any(is_type, na.rm = TRUE)) {
    return(out)
  }

  payload <- trimws(stringr::str_remove(
    log_entry[is_type],
    stringr::regex(paste0("^\"?", type_pattern, "\"?\\s*[:=]\\s*"), ignore_case = TRUE)
  ))
  payload <- stringr::str_remove_all(payload, "^\"|\"$")
  out[is_type] <- payload
  out
}

log_parse_event_number <- function(log_entry, type) {
  suppressWarnings(as.numeric(log_parse_event_value(log_entry, type)))
}

log_group_by_cols <- function(data, cols) {
  if (length(cols) == 0) {
    return(data)
  }

  dplyr::group_by(data, dplyr::across(dplyr::all_of(cols)))
}

log_empty_anomalies <- function(context_cols = character()) {
  context <- tibble::as_tibble(stats::setNames(
    rep(list(character()), length(context_cols)),
    context_cols
  ))

  dplyr::bind_cols(
    context,
    tibble::tibble(
      anomaly_code = character(),
      severity = factor(
        character(),
        levels = c("info", "warning", "critical"),
        ordered = TRUE
      ),
      ts_start = numeric(),
      ts_end = numeric(),
      n_events = integer(),
      evidence = character(),
      message = character()
    )
  )
}

log_format_anomalies <- function(data, context_cols) {
  if (nrow(data) == 0) {
    return(log_empty_anomalies(context_cols))
  }

  missing_context <- setdiff(context_cols, names(data))
  if (length(missing_context) > 0) {
    data[missing_context] <- NA_character_
  }

  data %>%
    dplyr::mutate(
      severity = factor(
        .data$severity,
        levels = c("info", "warning", "critical"),
        ordered = TRUE
      ),
      ts_start = suppressWarnings(as.numeric(.data$ts_start)),
      ts_end = suppressWarnings(as.numeric(.data$ts_end)),
      n_events = as.integer(.data$n_events),
      evidence = as.character(.data$evidence),
      message = as.character(.data$message)
    ) %>%
    dplyr::select(
      dplyr::any_of(context_cols),
      "anomaly_code",
      "severity",
      "ts_start",
      "ts_end",
      "n_events",
      "evidence",
      "message"
    )
}

log_collapse_values <- function(x, max_values = 4L) {
  x <- unique(as.character(x[!is.na(x) & x != ""]))
  if (length(x) == 0) {
    return(NA_character_)
  }
  if (length(x) > max_values) {
    x <- c(x[seq_len(max_values)], "...")
  }
  paste(x, collapse = ", ")
}

log_empty_loadcomplete <- function(n = 0L) {
  tibble::tibble(
    browser_version = rep(NA_character_, n),
    browser_name = rep(NA_character_, n),
    os_name = rep(NA_character_, n),
    os_family = rep(NA_character_, n),
    os_version = rep(NA_character_, n),
    device = rep(NA_character_, n),
    device_class = rep(NA_character_, n),
    screen_size_width = rep(NA_real_, n),
    screen_size_height = rep(NA_real_, n),
    screen_orientation = rep(NA_character_, n),
    load_time = rep(NA_real_, n),
    loadcomplete_parse_ok = rep(FALSE, n),
    loadcomplete_parse_error = rep(NA_character_, n)
  )
}

log_clean_loadcomplete_payload <- function(log_entry) {
  payload <- trimws(stringr::str_remove(
    log_entry,
    "^LOADCOMPLETE\\s*[:=]\\s*"
  ))
  payload <- stringr::str_remove_all(payload, "^\"|\"$")
  payload <- stringr::str_replace_all(payload, "\\\\\"", "\"")
  payload <- stringr::str_replace_all(payload, "\"\"", "\"")
  payload
}

log_parse_json_deep <- function(payload, max_depth = 5L) {
  value <- payload

  for (i in seq_len(max_depth)) {
    if (!is.character(value) || length(value) != 1L) {
      break
    }

    value <- tryCatch(
      jsonlite::fromJSON(value, simplifyVector = FALSE),
      error = function(e) {
        structure(
          list(message = conditionMessage(e)),
          class = "eatPrepTBA_log_parse_error"
        )
      }
    )

    if (inherits(value, "eatPrepTBA_log_parse_error")) {
      break
    }
  }

  value
}

log_scalar_character <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(NA_character_)
  }
  as.character(x[[1]])
}

log_scalar_numeric <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(x[[1]]))
}

log_os_family <- function(os_name) {
  os <- stringr::str_to_lower(os_name)

  dplyr::case_when(
    is.na(os_name) | os_name == "" ~ NA_character_,
    stringr::str_detect(os, "ios") ~ "iOS",
    stringr::str_detect(os, "android") ~ "Android",
    stringr::str_detect(os, "windows") ~ "Windows",
    stringr::str_detect(os, "mac\\s*os|macos") ~ "macOS",
    stringr::str_detect(os, "chrome\\s*os|cros") ~ "Chrome OS",
    stringr::str_detect(os, "linux") ~ "Linux",
    TRUE ~ "other"
  )
}

log_device_class <- function(device) {
  device_lower <- stringr::str_to_lower(device)

  dplyr::case_when(
    is.na(device) | device == "" ~ NA_character_,
    stringr::str_detect(device_lower, "tablet|ipad") ~ "tablet",
    stringr::str_detect(device_lower, "mobile|phone|iphone|smartphone") ~ "smartphone",
    stringr::str_detect(device_lower, "desktop|laptop|notebook|pc") ~ "desktop",
    TRUE ~ "other"
  )
}

log_screen_orientation <- function(width, height) {
  dplyr::case_when(
    is.na(width) | is.na(height) ~ NA_character_,
    width > height ~ "landscape",
    width < height ~ "portrait",
    TRUE ~ "square"
  )
}

log_parse_loadcomplete_entry <- function(log_entry) {
  parsed_empty <- log_empty_loadcomplete(1L)

  if (is.na(log_entry) || log_extract_type(log_entry) != "LOADCOMPLETE") {
    return(parsed_empty)
  }

  payload <- log_clean_loadcomplete_payload(log_entry)
  parsed <- log_parse_json_deep(payload)

  if (inherits(parsed, "eatPrepTBA_log_parse_error")) {
    parsed_empty$loadcomplete_parse_error <- parsed$message
    return(parsed_empty)
  }

  if (!is.list(parsed)) {
    parsed_empty$loadcomplete_parse_error <- "Parsed LOADCOMPLETE payload is not a JSON object."
    return(parsed_empty)
  }

  environment <- parsed
  if (!is.null(parsed$environment) && is.list(parsed$environment)) {
    environment <- parsed$environment
  }

  browser_version <- log_scalar_character(environment$browserVersion)
  browser_name <- log_scalar_character(environment$browserName)
  os_name <- log_scalar_character(environment$osName)
  device <- log_scalar_character(environment$device)
  screen_size_width <- log_scalar_numeric(environment$screenSizeWidth)
  screen_size_height <- log_scalar_numeric(environment$screenSizeHeight)
  load_time <- log_scalar_numeric(environment$loadTime)

  tibble::tibble(
    browser_version = browser_version,
    browser_name = browser_name,
    os_name = os_name,
    os_family = log_os_family(os_name),
    os_version = stringr::str_extract(os_name, "\\d+(?:\\.\\d+)*"),
    device = device,
    device_class = log_device_class(device),
    screen_size_width = screen_size_width,
    screen_size_height = screen_size_height,
    screen_orientation = log_screen_orientation(screen_size_width, screen_size_height),
    load_time = load_time,
    loadcomplete_parse_ok = any(!is.na(c(
      browser_version,
      browser_name,
      os_name,
      device,
      screen_size_width,
      screen_size_height,
      load_time
    ))),
    loadcomplete_parse_error = NA_character_
  )
}

log_parse_loadcomplete <- function(log_entry) {
  if (length(log_entry) == 0) {
    return(log_empty_loadcomplete())
  }

  purrr::map_dfr(log_entry, log_parse_loadcomplete_entry)
}

log_loadcomplete_rows <- function(logs) {
  checkmate::assert_tibble(logs)
  assert_cols(logs, "log_entry", "logs")

  rows <- logs %>%
    dplyr::mutate(
      .log_row = dplyr::row_number(),
      log_type = log_extract_type(.data$log_entry)
    ) %>%
    dplyr::filter(.data$log_type == "LOADCOMPLETE")

  if (nrow(rows) == 0) {
    return(dplyr::bind_cols(rows, log_empty_loadcomplete()))
  }

  dplyr::bind_cols(rows, log_parse_loadcomplete(rows$log_entry))
}

#' Summarise log event types
#'
#' @param logs Tibble. Logs retrieved with [get_logs()] or read with [read_logs()].
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Counts log event types and classifies whether an event type is known from
#' Testcenter logging and whether eatPrepTBA currently has a parser for it.
#' This function is intended as a cheap first diagnostic before parsing large
#' log data in detail.
#'
#' @return A tibble with one row per log type.
#'
#' @export
summarise_log_inventory <- function(logs) {
  checkmate::assert_tibble(logs)
  assert_cols(logs, "log_entry", "logs")

  session_cols <- log_session_cols(logs)
  person_cols <- log_person_cols(logs)
  has_booklet <- "booklet_id" %in% names(logs)
  has_ts <- "ts" %in% names(logs)

  logs_prep <- logs %>%
    dplyr::mutate(
      log_type = log_extract_type(.data$log_entry),
      log_family = log_event_family(.data$log_type),
      known_log_type = log_is_known_type(.data$log_type),
      supported_parser = log_has_supported_parser(.data$log_type),
      .session_key = log_make_key(., session_cols),
      .person_key = log_make_key(., person_cols),
      .booklet_key = if (has_booklet) as.character(.data$booklet_id) else NA_character_,
      .ts_num = if (has_ts) suppressWarnings(as.numeric(.data$ts)) else NA_real_
    )

  logs_prep %>%
    dplyr::group_by(
      .data$log_type,
      .data$log_family,
      .data$known_log_type,
      .data$supported_parser
    ) %>%
    dplyr::summarise(
      n = dplyr::n(),
      n_sessions = if (length(session_cols) > 0) {
        dplyr::n_distinct(.data$.session_key, na.rm = TRUE)
      } else {
        NA_integer_
      },
      n_persons = if (length(person_cols) > 0) {
        dplyr::n_distinct(.data$.person_key, na.rm = TRUE)
      } else {
        NA_integer_
      },
      n_booklets = if (has_booklet) {
        dplyr::n_distinct(.data$.booklet_key, na.rm = TRUE)
      } else {
        NA_integer_
      },
      first_ts = log_min_or_na(.data$.ts_num),
      last_ts = log_max_or_na(.data$.ts_num),
      example_entry = log_first_non_missing(.data$log_entry),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$n), .data$log_type)
}

#' Summarise LOADCOMPLETE environment logs
#'
#' @param logs Tibble. Logs retrieved with [get_logs()] or read with [read_logs()].
#' @param session_cols Optional character vector with columns defining a test
#' session. By default, all available columns among `group_id`, `login_name`,
#' `login_code`, and `booklet_id` are used.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Extracts browser, operating system, device, screen size, orientation, and
#' initial `LOADCOMPLETE` load time. Multiple `LOADCOMPLETE` rows per session
#' are retained as diagnostics via count and conflict columns while the first
#' non-missing value per field is returned.
#'
#' @return A tibble with one row per session.
#'
#' @export
summarise_log_environment <- function(logs, session_cols = NULL) {
  checkmate::assert_tibble(logs)
  assert_cols(logs, "log_entry", "logs")

  if (is.null(session_cols)) {
    session_cols <- log_session_cols(logs)
  }
  checkmate::assert_character(session_cols, null.ok = FALSE)
  assert_cols(logs, session_cols, "logs")

  loadcomplete <- log_loadcomplete_rows(logs)
  has_ts <- "ts" %in% names(loadcomplete)

  if (nrow(loadcomplete) == 0) {
    out <- tibble::tibble(
      n_loadcomplete_events = integer(),
      n_loadcomplete_parsed = integer(),
      loadcomplete_parse_ok = logical(),
      loadcomplete_multiple = logical(),
      loadcomplete_conflicting = logical(),
      loadcomplete_first_ts = numeric(),
      loadcomplete_last_ts = numeric(),
      loadcomplete_parse_error = character(),
      browser_name = character(),
      browser_version = character(),
      os_name = character(),
      os_family = character(),
      os_version = character(),
      device = character(),
      device_class = character(),
      screen_size_width = numeric(),
      screen_size_height = numeric(),
      screen_orientation = character(),
      load_time = numeric()
    )

    if (length(session_cols) > 0) {
      out <- dplyr::bind_cols(logs[0, session_cols], out)
    }

    return(out)
  }

  loadcomplete <- loadcomplete %>%
    dplyr::mutate(
      .ts_num = if (has_ts) suppressWarnings(as.numeric(.data$ts)) else NA_real_
    ) %>%
    dplyr::arrange(
      dplyr::across(dplyr::any_of(session_cols)),
      .data$.ts_num,
      .data$.log_row
    )

  if (length(session_cols) > 0) {
    loadcomplete <- loadcomplete %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(session_cols)))
  }

  loadcomplete %>%
    dplyr::summarise(
      n_loadcomplete_events = dplyr::n(),
      n_loadcomplete_parsed = sum(.data$loadcomplete_parse_ok, na.rm = TRUE),
      loadcomplete_parse_ok = any(.data$loadcomplete_parse_ok %in% TRUE),
      loadcomplete_multiple = .data$n_loadcomplete_events > 1L,
      loadcomplete_conflicting = any(c(
        log_n_distinct_non_missing(.data$browser_name),
        log_n_distinct_non_missing(.data$browser_version),
        log_n_distinct_non_missing(.data$os_name),
        log_n_distinct_non_missing(.data$device),
        log_n_distinct_non_missing(.data$screen_size_width),
        log_n_distinct_non_missing(.data$screen_size_height),
        log_n_distinct_non_missing(.data$load_time)
      ) > 1L),
      loadcomplete_first_ts = log_min_or_na(.data$.ts_num),
      loadcomplete_last_ts = log_max_or_na(.data$.ts_num),
      loadcomplete_parse_error = log_first_non_missing(.data$loadcomplete_parse_error),
      browser_name = log_first_non_missing(.data$browser_name),
      browser_version = log_first_non_missing(.data$browser_version),
      os_name = log_first_non_missing(.data$os_name),
      os_family = log_first_non_missing(.data$os_family),
      os_version = log_first_non_missing(.data$os_version),
      device = log_first_non_missing(.data$device),
      device_class = log_first_non_missing(.data$device_class),
      screen_size_width = log_first_non_missing(.data$screen_size_width),
      screen_size_height = log_first_non_missing(.data$screen_size_height),
      screen_orientation = log_first_non_missing(.data$screen_orientation),
      load_time = log_first_non_missing(.data$load_time),
      .groups = "drop"
    )
}

#' Detect potentially unreliable log patterns
#'
#' @param logs Tibble. Logs retrieved with [get_logs()] or read with [read_logs()].
#' @param session_cols Optional character vector with columns defining a test
#' session. By default, all available columns among `group_id`, `group`,
#' `login_name`, `login`, `login_code`, and `booklet_id` are used.
#' @param unit_cols Optional character vector with columns defining units. By
#' default, available columns among `unit_key` and `unit_alias` are used.
#' @param focus_loss_threshold_ms Numeric. Focus losses longer than this
#' threshold are flagged.
#' @param connection_transition_threshold Integer. Sessions with more
#' connection state transitions than this threshold are flagged.
#' @param include_unknown_events Logical. Should event types unknown to
#' eatPrepTBA be returned as informational anomalies? Defaults to `FALSE`
#' because large log datasets may contain many player-specific entries.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Returns a long evidence table with one row per detected log anomaly. The
#' function is intentionally conservative: it focuses on structural reliability
#' signals that can be derived from ordinary Testcenter logs, such as malformed
#' `LOADCOMPLETE` rows, player loading/running inconsistencies, connection loss,
#' unresolved focus loss, runtime errors, timestamp problems, and inconsistent
#' page counters.
#'
#' @return A tibble with session identifiers, optional unit identifiers,
#' `anomaly_code`, `severity`, `ts_start`, `ts_end`, `n_events`, `evidence`, and
#' `message`.
#'
#' @export
detect_log_anomalies <- function(logs,
                                 session_cols = NULL,
                                 unit_cols = NULL,
                                 focus_loss_threshold_ms = 5 * 60 * 1000,
                                 connection_transition_threshold = 10L,
                                 include_unknown_events = FALSE) {
  checkmate::assert_tibble(logs)
  assert_cols(logs, "log_entry", "logs")
  checkmate::assert_number(focus_loss_threshold_ms, lower = 0)
  checkmate::assert_integerish(connection_transition_threshold, len = 1, lower = 0)
  checkmate::assert_logical(include_unknown_events, len = 1)

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

  context_cols <- unique(c(session_cols, unit_cols))
  has_ts <- "ts" %in% names(logs)

  logs_prep <- logs %>%
    dplyr::mutate(
      .log_row = dplyr::row_number(),
      .ts_num = if (has_ts) suppressWarnings(as.numeric(.data$ts)) else NA_real_,
      log_type = log_extract_type(.data$log_entry),
      log_type_upper = stringr::str_to_upper(.data$log_type),
      log_family = log_event_family(.data$log_type),
      known_log_type = log_is_known_type(.data$log_type),
      player_state = stringr::str_to_upper(log_parse_event_value(.data$log_entry, "PLAYER")),
      focus_state = stringr::str_to_upper(log_parse_event_value(.data$log_entry, "FOCUS")),
      connection_state = stringr::str_to_upper(log_parse_connection(.data$log_entry)),
      current_page_nr = log_parse_event_number(.data$log_entry, "CURRENT_PAGE_NR"),
      page_count = log_parse_event_number(.data$log_entry, "PAGE_COUNT")
    )

  env <- summarise_log_environment(logs, session_cols = session_cols)
  sessions <- if (length(session_cols) > 0) {
    dplyr::distinct(logs, dplyr::across(dplyr::all_of(session_cols)))
  } else {
    tibble::tibble(.rows = 1L)
  }

  env_joined <- dplyr::left_join(sessions, env, by = session_cols)

  missing_loadcomplete <- env_joined %>%
    dplyr::filter(is.na(.data$n_loadcomplete_events) | .data$n_loadcomplete_events == 0L) %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "missing_loadcomplete",
      severity = "warning",
      ts_start = NA_real_,
      ts_end = NA_real_,
      n_events = 1L,
      evidence = NA_character_,
      message = "No LOADCOMPLETE event was found for this session; environment and initial load-time metadata are unavailable."
    )

  malformed_loadcomplete <- env_joined %>%
    dplyr::filter(
      !is.na(.data$n_loadcomplete_events),
      .data$n_loadcomplete_events > 0L,
      .data$n_loadcomplete_parsed < .data$n_loadcomplete_events
    ) %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "malformed_loadcomplete",
      severity = "warning",
      ts_start = .data$loadcomplete_first_ts,
      ts_end = .data$loadcomplete_last_ts,
      n_events = .data$n_loadcomplete_events - .data$n_loadcomplete_parsed,
      evidence = .data$loadcomplete_parse_error,
      message = "At least one LOADCOMPLETE payload could not be parsed."
    )

  multiple_loadcomplete <- env_joined %>%
    dplyr::filter(.data$loadcomplete_multiple %in% TRUE) %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "multiple_loadcomplete_in_session",
      severity = "info",
      ts_start = .data$loadcomplete_first_ts,
      ts_end = .data$loadcomplete_last_ts,
      n_events = .data$n_loadcomplete_events,
      evidence = paste0("n_loadcomplete_events=", .data$n_loadcomplete_events),
      message = "More than one LOADCOMPLETE event was found for this session."
    )

  conflicting_loadcomplete <- env_joined %>%
    dplyr::filter(.data$loadcomplete_conflicting %in% TRUE) %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "conflicting_loadcomplete",
      severity = "warning",
      ts_start = .data$loadcomplete_first_ts,
      ts_end = .data$loadcomplete_last_ts,
      n_events = .data$n_loadcomplete_events,
      evidence = paste0(
        "browser=", .data$browser_name,
        "; os=", .data$os_name,
        "; device=", .data$device
      ),
      message = "Multiple LOADCOMPLETE events contain conflicting environment or load-time values."
    )

  first_player <- logs_prep %>%
    dplyr::filter(.data$log_type_upper == "PLAYER", !is.na(.data$.ts_num)) %>%
    log_group_by_cols(session_cols) %>%
    dplyr::summarise(first_player_ts = log_min_or_na(.data$.ts_num), .groups = "drop")

  loadcomplete_after_unit_start <- env_joined %>%
    dplyr::left_join(first_player, by = session_cols) %>%
    dplyr::filter(
      !is.na(.data$loadcomplete_first_ts),
      !is.na(.data$first_player_ts),
      .data$loadcomplete_first_ts > .data$first_player_ts
    ) %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "loadcomplete_after_unit_start",
      severity = "warning",
      ts_start = .data$first_player_ts,
      ts_end = .data$loadcomplete_first_ts,
      n_events = 1L,
      evidence = paste0(
        "first_player_ts=", .data$first_player_ts,
        "; loadcomplete_first_ts=", .data$loadcomplete_first_ts
      ),
      message = "LOADCOMPLETE occurred after the first PLAYER event; initial load timing may be unreliable."
    )

  player_group_cols <- unique(c(session_cols, unit_cols))
  player_logs <- logs_prep %>%
    dplyr::filter(.data$log_type_upper == "PLAYER", .data$player_state %in% c("LOADING", "RUNNING")) %>%
    dplyr::arrange(
      dplyr::across(dplyr::any_of(player_group_cols)),
      .data$.ts_num,
      .data$.log_row
    ) %>%
    log_group_by_cols(player_group_cols) %>%
    dplyr::mutate(
      prev_player_state = dplyr::lag(.data$player_state),
      next_player_state = dplyr::lead(.data$player_state)
    ) %>%
    dplyr::ungroup()

  running_without_loading <- player_logs %>%
    dplyr::filter(
      .data$player_state == "RUNNING",
      is.na(.data$prev_player_state) | .data$prev_player_state != "LOADING"
    ) %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "player_running_without_loading",
      severity = "warning",
      ts_start = .data$.ts_num,
      ts_end = .data$.ts_num,
      n_events = 1L,
      evidence = .data$log_entry,
      message = "PLAYER was RUNNING without a preceding LOADING event for the same unit."
    )

  loading_without_running <- player_logs %>%
    dplyr::filter(
      .data$player_state == "LOADING",
      is.na(.data$next_player_state) | .data$next_player_state != "RUNNING"
    ) %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "loading_without_running",
      severity = "warning",
      ts_start = .data$.ts_num,
      ts_end = .data$.ts_num,
      n_events = 1L,
      evidence = .data$log_entry,
      message = "PLAYER was LOADING and was not followed by RUNNING for the same unit."
    )

  repeated_loading <- player_logs %>%
    dplyr::filter(.data$player_state == "LOADING", .data$next_player_state == "LOADING") %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "repeated_loading",
      severity = "warning",
      ts_start = .data$.ts_num,
      ts_end = .data$.ts_num,
      n_events = 1L,
      evidence = .data$log_entry,
      message = "Consecutive PLAYER = LOADING events were logged for the same unit."
    )

  significant_logs <- logs_prep %>%
    dplyr::filter(
      !.data$log_type_upper %in% c("TESTLETS_TIMELEFT"),
      !is.na(.data$.ts_num)
    ) %>%
    dplyr::arrange(
      dplyr::across(dplyr::any_of(session_cols)),
      .data$.ts_num,
      .data$.log_row
    ) %>%
    log_group_by_cols(session_cols) %>%
    dplyr::slice_tail(n = 1L) %>%
    dplyr::ungroup()

  last_player_event <- significant_logs %>%
    dplyr::filter(.data$log_type_upper == "PLAYER", .data$player_state %in% c("LOADING", "RUNNING")) %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = dplyr::case_when(
        .data$player_state == "LOADING" ~ "last_event_is_loading",
        .data$player_state == "RUNNING" ~ "last_event_is_running",
        TRUE ~ "last_event_is_player"
      ),
      severity = "warning",
      ts_start = .data$.ts_num,
      ts_end = .data$.ts_num,
      n_events = 1L,
      evidence = .data$log_entry,
      message = "The last significant session log is a PLAYER state; end timing may be ambiguous."
    )

  connection_logs <- logs_prep %>%
    dplyr::filter(.data$log_type_upper == "CONNECTION") %>%
    dplyr::arrange(
      dplyr::across(dplyr::any_of(session_cols)),
      .data$.ts_num,
      .data$.log_row
    ) %>%
    log_group_by_cols(session_cols) %>%
    dplyr::mutate(connection_transition = .data$connection_state != dplyr::lag(.data$connection_state)) %>%
    dplyr::ungroup()

  connection_lost <- connection_logs %>%
    dplyr::filter(.data$connection_state == "LOST") %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "connection_lost",
      severity = "warning",
      ts_start = .data$.ts_num,
      ts_end = .data$.ts_num,
      n_events = 1L,
      evidence = .data$log_entry,
      message = "The Testcenter connection state was LOST."
    )

  many_connection_transitions <- connection_logs %>%
    log_group_by_cols(session_cols) %>%
    dplyr::summarise(
      n_events = sum(.data$connection_transition %in% TRUE, na.rm = TRUE),
      ts_start = log_min_or_na(.data$.ts_num),
      ts_end = log_max_or_na(.data$.ts_num),
      evidence = log_collapse_values(.data$connection_state),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$n_events > connection_transition_threshold) %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "many_connection_transitions",
      severity = "warning",
      ts_start = .data$ts_start,
      ts_end = .data$ts_end,
      n_events = .data$n_events,
      evidence = .data$evidence,
      message = "The session has unusually many connection state transitions."
    )

  last_connection_lost <- connection_logs %>%
    log_group_by_cols(session_cols) %>%
    dplyr::slice_tail(n = 1L) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$connection_state == "LOST") %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "last_connection_lost",
      severity = "critical",
      ts_start = .data$.ts_num,
      ts_end = .data$.ts_num,
      n_events = 1L,
      evidence = .data$log_entry,
      message = "The last logged Testcenter connection state was LOST."
    )

  focus_logs <- logs_prep %>%
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
      has_focus_at_or_after = rev(cumsum(rev(.data$focus_state == "HAS")) > 0L),
      has_later_focus_regain = dplyr::lead(.data$has_focus_at_or_after, default = FALSE)
    ) %>%
    dplyr::ungroup()

  focus_lost_never_regained <- focus_logs %>%
    dplyr::filter(.data$focus_state == "HAS_NOT", !.data$has_later_focus_regain) %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "focus_lost_never_regained",
      severity = "warning",
      ts_start = .data$.ts_num,
      ts_end = .data$.ts_num,
      n_events = 1L,
      evidence = .data$log_entry,
      message = "Focus was lost and no later FOCUS = HAS event was logged in the same session."
    )

  repeated_focus_lost <- focus_logs %>%
    dplyr::filter(.data$focus_state == "HAS_NOT", .data$next_focus_state == "HAS_NOT") %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "repeated_focus_lost_before_regain",
      severity = "info",
      ts_start = .data$.ts_num,
      ts_end = .data$next_focus_ts,
      n_events = 2L,
      evidence = .data$log_entry,
      message = "A second focus-lost event was logged before focus was regained."
    )

  very_long_focus_loss <- focus_logs %>%
    dplyr::filter(
      .data$focus_state == "HAS_NOT",
      .data$next_focus_state == "HAS",
      !is.na(.data$next_focus_ts),
      .data$next_focus_ts - .data$.ts_num > focus_loss_threshold_ms
    ) %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "very_long_focus_loss",
      severity = "warning",
      ts_start = .data$.ts_num,
      ts_end = .data$next_focus_ts,
      n_events = 1L,
      evidence = paste0("duration_ms=", .data$next_focus_ts - .data$.ts_num),
      message = "Focus was lost for longer than focus_loss_threshold_ms."
    )

  runtime_error <- logs_prep %>%
    dplyr::filter(.data$log_type_upper == "RUNTIME ERROR") %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "runtime_error",
      severity = "critical",
      ts_start = .data$.ts_num,
      ts_end = .data$.ts_num,
      n_events = 1L,
      evidence = .data$log_entry,
      message = "A Verona player runtime error was logged."
    )

  timestamp_decreases <- logs_prep %>%
    dplyr::filter(!is.na(.data$.ts_num)) %>%
    dplyr::arrange(dplyr::across(dplyr::any_of(session_cols)), .data$.log_row) %>%
    log_group_by_cols(session_cols) %>%
    dplyr::mutate(previous_ts = dplyr::lag(.data$.ts_num)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$previous_ts), .data$.ts_num < .data$previous_ts) %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "timestamp_decreases_in_input",
      severity = "warning",
      ts_start = .data$previous_ts,
      ts_end = .data$.ts_num,
      n_events = 1L,
      evidence = .data$log_entry,
      message = "Timestamps decrease in the input order within the same session."
    )

  zero_timestamp <- logs_prep %>%
    dplyr::filter(.data$.ts_num == 0) %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "zero_timestamp",
      severity = "warning",
      ts_start = .data$.ts_num,
      ts_end = .data$.ts_num,
      n_events = 1L,
      evidence = .data$log_entry,
      message = "A log entry has timestamp 0."
    )

  page_group_cols <- unique(c(session_cols, unit_cols))
  page_counts <- logs_prep %>%
    dplyr::filter(!is.na(.data$page_count)) %>%
    log_group_by_cols(page_group_cols) %>%
    dplyr::summarise(
      n_page_count_values = dplyr::n_distinct(.data$page_count, na.rm = TRUE),
      ts_start = log_min_or_na(.data$.ts_num),
      ts_end = log_max_or_na(.data$.ts_num),
      evidence = log_collapse_values(.data$page_count),
      .groups = "drop"
    )

  page_count_inconsistent <- page_counts %>%
    dplyr::filter(.data$n_page_count_values > 1L) %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "page_count_inconsistent",
      severity = "warning",
      ts_start = .data$ts_start,
      ts_end = .data$ts_end,
      n_events = .data$n_page_count_values,
      evidence = .data$evidence,
      message = "Different PAGE_COUNT values were logged for the same session/unit."
    )

  page_max <- logs_prep %>%
    dplyr::filter(!is.na(.data$current_page_nr) | !is.na(.data$page_count)) %>%
    log_group_by_cols(page_group_cols) %>%
    dplyr::summarise(
      max_current_page_nr = suppressWarnings(max(.data$current_page_nr, na.rm = TRUE)),
      max_page_count = suppressWarnings(max(.data$page_count, na.rm = TRUE)),
      ts_start = log_min_or_na(.data$.ts_num),
      ts_end = log_max_or_na(.data$.ts_num),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      max_current_page_nr = dplyr::if_else(is.infinite(.data$max_current_page_nr), NA_real_, .data$max_current_page_nr),
      max_page_count = dplyr::if_else(is.infinite(.data$max_page_count), NA_real_, .data$max_page_count)
    )

  page_nr_exceeds_page_count <- page_max %>%
    dplyr::filter(
      !is.na(.data$max_current_page_nr),
      !is.na(.data$max_page_count),
      .data$max_current_page_nr > .data$max_page_count
    ) %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(context_cols)),
      anomaly_code = "page_nr_exceeds_page_count",
      severity = "warning",
      ts_start = .data$ts_start,
      ts_end = .data$ts_end,
      n_events = 1L,
      evidence = paste0(
        "max_current_page_nr=", .data$max_current_page_nr,
        "; max_page_count=", .data$max_page_count
      ),
      message = "CURRENT_PAGE_NR exceeds PAGE_COUNT for the same session/unit."
    )

  unknown_events <- if (include_unknown_events) {
    logs_prep %>%
      dplyr::filter(!.data$known_log_type) %>%
      log_group_by_cols(c(session_cols, "log_type")) %>%
      dplyr::summarise(
        ts_start = log_min_or_na(.data$.ts_num),
        ts_end = log_max_or_na(.data$.ts_num),
        n_events = dplyr::n(),
        evidence = log_first_non_missing(.data$log_entry),
        .groups = "drop"
      ) %>%
      dplyr::transmute(
        dplyr::across(dplyr::any_of(context_cols)),
        anomaly_code = "unknown_log_type",
        severity = "info",
        ts_start = .data$ts_start,
        ts_end = .data$ts_end,
        n_events = .data$n_events,
        evidence = .data$evidence,
        message = paste0("Unknown or player-specific log type: ", .data$log_type)
      )
  } else {
    log_empty_anomalies(context_cols)
  }

  dplyr::bind_rows(
    missing_loadcomplete,
    malformed_loadcomplete,
    multiple_loadcomplete,
    conflicting_loadcomplete,
    loadcomplete_after_unit_start,
    running_without_loading,
    loading_without_running,
    repeated_loading,
    last_player_event,
    connection_lost,
    many_connection_transitions,
    last_connection_lost,
    focus_lost_never_regained,
    repeated_focus_lost,
    very_long_focus_loss,
    runtime_error,
    timestamp_decreases,
    zero_timestamp,
    page_count_inconsistent,
    page_nr_exceeds_page_count,
    unknown_events
  ) %>%
    log_format_anomalies(context_cols) %>%
    dplyr::arrange(
      dplyr::across(dplyr::any_of(context_cols)),
      .data$ts_start,
      .data$anomaly_code
    )
}

#' Summarise log quality anomalies by session
#'
#' @param logs Tibble. Logs retrieved with [get_logs()] or read with [read_logs()].
#' @param anomalies Optional tibble returned by [detect_log_anomalies()]. When
#' omitted, anomalies are detected from `logs` using default settings.
#' @param session_cols Optional character vector with columns defining a test
#' session. By default, all available columns among `group_id`, `group`,
#' `login_name`, `login`, `login_code`, and `booklet_id` are used.
#' @param ... Additional arguments passed to [detect_log_anomalies()] when
#' `anomalies` is omitted.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Condenses the long anomaly table into one row per session, retaining counts
#' by severity and the set of anomaly codes observed. Sessions with no detected
#' anomalies are included.
#'
#' @return A tibble with one row per session.
#'
#' @export
summarise_log_qc <- function(logs, anomalies = NULL, session_cols = NULL, ...) {
  checkmate::assert_tibble(logs)
  assert_cols(logs, "log_entry", "logs")

  if (is.null(session_cols)) {
    session_cols <- log_session_cols(logs)
  }
  checkmate::assert_character(session_cols, null.ok = FALSE)
  assert_cols(logs, session_cols, "logs")

  if (is.null(anomalies)) {
    anomalies <- detect_log_anomalies(logs, session_cols = session_cols, ...)
  }
  checkmate::assert_tibble(anomalies)
  assert_cols(anomalies, c("anomaly_code", "severity"), "anomalies")
  assert_cols(anomalies, session_cols, "anomalies")

  sessions <- if (length(session_cols) > 0) {
    dplyr::distinct(logs, dplyr::across(dplyr::all_of(session_cols)))
  } else {
    tibble::tibble(.rows = 1L)
  }

  counts <- anomalies %>%
    dplyr::mutate(severity = as.character(.data$severity)) %>%
    log_group_by_cols(session_cols) %>%
    dplyr::summarise(
      n_anomalies = dplyr::n(),
      n_critical = sum(.data$severity == "critical", na.rm = TRUE),
      n_warning = sum(.data$severity == "warning", na.rm = TRUE),
      n_info = sum(.data$severity == "info", na.rm = TRUE),
      anomaly_codes = log_collapse_values(.data$anomaly_code, max_values = 20L),
      .groups = "drop"
    )

  sessions %>%
    dplyr::left_join(counts, by = session_cols) %>%
    dplyr::mutate(
      n_anomalies = dplyr::coalesce(.data$n_anomalies, 0L),
      n_critical = dplyr::coalesce(.data$n_critical, 0L),
      n_warning = dplyr::coalesce(.data$n_warning, 0L),
      n_info = dplyr::coalesce(.data$n_info, 0L),
      has_critical_anomaly = .data$n_critical > 0L,
      has_warning_anomaly = .data$n_warning > 0L,
      has_info_anomaly = .data$n_info > 0L,
      log_qc_flag = dplyr::case_when(
        .data$has_critical_anomaly ~ "critical",
        .data$has_warning_anomaly ~ "warning",
        .data$has_info_anomaly ~ "info",
        TRUE ~ "ok"
      ),
      anomaly_codes = dplyr::coalesce(.data$anomaly_codes, NA_character_)
    ) %>%
    dplyr::select(
      dplyr::all_of(session_cols),
      "log_qc_flag",
      "has_critical_anomaly",
      "has_warning_anomaly",
      "has_info_anomaly",
      "n_anomalies",
      "n_critical",
      "n_warning",
      "n_info",
      "anomaly_codes"
    )
}
