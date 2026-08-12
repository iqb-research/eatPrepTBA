#' Prepares logs
#'
#' @param logs Tibble. Responses retrieved from the IQB Testcenter via [get_logs()] or from an extracted csv and read via [read_logs()].
#' @param log_events Character vector. Names of events to be filtered for.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function returns the logs in a format where the values are still list columns that need to be unpacked.
#'
#' @return A tibble.
#'
#' @export
prepare_logs <- function(logs, log_events = NULL) {
  checkmate::assert_tibble(logs)
  assert_cols(logs, "log_entry", "logs")
  checkmate::assert_character(log_events, null.ok = TRUE)

  all_events <-
    c("current_unit_id",
      "current_page_id",
      "current_page_nr",
      "page_count",
      "controller",
      "focus",
      "player",
      "presentation_progress",
      "response_progress",
      "loadcomplete",
      "connection")

  if (is.null(log_events)) {
    log_events <- all_events
  } else {
    log_events <- stringr::str_to_lower(log_events)
  }

  log_filter <-
    intersect(log_events, all_events) %>%
    stringr::str_to_upper()

  if (length(log_filter) == 0) {
    return(logs[0, ])
  }

  logs %>%
    dplyr::filter(stringr::str_to_upper(log_extract_type(log_entry)) %in% log_filter) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      current_unit_id = parse_log_entry(log_entry, log_events, type = "CURRENT_UNIT_ID", parse = TRUE),
      current_page_id = parse_log_entry(log_entry, log_events, type = "CURRENT_PAGE_ID", sep = "="),
      current_page_no = parse_log_entry(log_entry, log_events, type = "CURRENT_PAGE_NR", sep = "="),
      page_count = parse_log_entry(log_entry, log_events, type = "PAGE_COUNT", sep = "="),
      controller = parse_log_entry(log_entry, log_events, type = "CONTROLLER", parse = TRUE),
      focus = parse_log_entry(log_entry, log_events, type = "FOCUS", parse = TRUE),
      player = parse_log_entry(log_entry, log_events, type = "PLAYER", sep = "="),
      presentation_progress = parse_log_entry(log_entry, log_events, type = "PRESENTATION_PROGRESS", sep = "="),
      response_progress = parse_log_entry(log_entry, log_events, type = "RESPONSE_PROGRESS", sep = "="),
      loadcomplete = if ("loadcomplete" %in% log_events) {
        list(log_parse_loadcomplete_entry(log_entry))
      },
      connection = if ("connection" %in% log_events) {
        log_parse_connection(log_entry)
      }
    ) %>%
    tidyr::unnest(dplyr::any_of(c("loadcomplete")), keep_empty = TRUE) %>%
    # Move log_entry to last position
    dplyr::relocate(log_entry, .after = ncol(.))
}

parse_log_entry <- function(log_entry, log_events, type = "", sep = ":", parse = FALSE) {
  if (stringr::str_to_lower(type) %in% log_events) {
    if (parse) {
      ifelse(test = stringr::str_detect(log_entry, stringr::str_glue("^{type}")),
             yes = stringr::str_remove(log_entry, stringr::str_glue("^{type} {sep} ")) %>% jsonlite::parse_json(),
             no = NA_character_)
    } else {
      # These strings are usually in capital letters.
      ifelse(test = stringr::str_detect(log_entry, stringr::str_glue("^{type}")),
             yes = stringr::str_remove(log_entry, stringr::str_glue("^{type} {sep} ")) %>% stringr::str_to_lower(),
             no = NA_character_)

    }
  } else {
    NULL
  }
}
