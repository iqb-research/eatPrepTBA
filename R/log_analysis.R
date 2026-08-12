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
  intersect(c("group_id", "login_name", "login_code", "booklet_id"), names(data))
}

log_person_cols <- function(data) {
  intersect(c("group_id", "login_name", "login_code"), names(data))
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
