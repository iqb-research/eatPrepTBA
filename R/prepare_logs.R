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
  }

  log_filter <-
    # Make sure that only plausible events are added (might not be necessary, however)
    intersect(log_events, all_events) %>%
    stringr::str_to_upper() %>%
    stringr::str_c(collapse = "|")

  logs %>%
    dplyr::filter(stringr::str_detect(log_entry, stringr::str_glue("^({log_filter})"))) %>%
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
      loadcomplete = if ("loadcomplete" %in% log_events) purrr::map(log_entry, function(x) {
        if (stringr::str_detect(log_entry, "^LOADCOMPLETE")) {
          # TODO: This routine works for both, csv and json (as these are different...)
          log_prep <- log_entry %>%
            stringr::str_remove("^LOADCOMPLETE : ") %>%  # Prefix entfernen
            stringr::str_remove_all("^\"|\"$")           # Äußere Quotes entfernen

          # **Erkennung und Korrektur von zwei Escape-Problemen:**
          if (stringr::str_detect(log_prep, "\\\\")) {
            # Fall 1: JSON mit Backslashes (`\\\"`)
            log_prep <- stringr::str_replace_all(log_prep, "\\\\\"", "\"")
          } else if (stringr::str_detect(log_prep, "\"\"")) {
            # Fall 2: JSON mit doppelten Quotes (`""`)
            log_prep <- stringr::str_replace_all(log_prep, "\"\"", "\"")
          }

          # **Erster Parsing-Versuch**
          parsed <- tryCatch(jsonlite::parse_json(log_prep), error = function(e) log_prep)

          # **Falls das Ergebnis noch ein JSON-String ist, nochmal parsen**
          if (is.character(parsed)) {
            parsed <- jsonlite::parse_json(parsed)
          }


          # Convert to tibble
          tibble::as_tibble(parsed)
        } else {
          tibble::tibble()
        }
      }, .progress = "Splitting user information"),

      connection = if ("connection" %in% log_events) purrr::map_chr(log_entry, function(x) {
        if (stringr::str_detect(log_entry, "^CONNECTION")) {
          log_entry %>%
            stringr::str_remove("^CONNECTION : ") %>%
            jsonlite::parse_json()
        } else if (stringr::str_detect(log_entry, "^\"CONNECTION\"")) {
          # Fix for CONNNECTION : LOST (somehow printed differently...)
          log_entry %>%
            stringr::str_remove("\"?CONNECTION\" : ")
        } else {
          NA
        }
      }, .progress = "Reading connection information"),
    ) %>%
    tidyr::unnest(dplyr::any_of(c("loadcomplete")), keep_empty = TRUE) %>%
    dplyr::rename(
      dplyr::any_of(c(
        "browser_version" = "browserVersion",
        "browser_name" = "browserName",
        "os_name" = "osName",
        "device" = "device",
        "screen_size_width" = "screenSizeWidth",
        "screen_size_height" = "screenSizeHeight",
        "load_time" = "loadTime"
      ))
    ) %>%
    # Move log_entry to last position
    dplyr::relocate(log_entry, .after = ncol(.))

  # logs_prep %>%
  #   dplyr::filter(!stringr::str_detect(log_entry, "^LOADCOMPLETE")) %>%
  #   dplyr::filter(!stringr::str_detect(log_entry, "^CURRENT_UNIT_ID")) %>%
  #   dplyr::filter(!stringr::str_detect(log_entry, "^CONNECTION")) %>%
  #   dplyr::filter(!stringr::str_detect(log_entry, "^\"CONNECTION\"")) %>%
  #   dplyr::filter(!stringr::str_detect(log_entry, "^FOCUS")) %>%
  #   dplyr::filter(!stringr::str_detect(log_entry, "^RESPONSE_PROGRESS")) %>%
  #   dplyr::filter(!stringr::str_detect(log_entry, "^PRESENTATION_PROGRESS")) %>%
  #   dplyr::filter(!stringr::str_detect(log_entry, "^CONTROLLER")) %>%
  #   dplyr::filter(!stringr::str_detect(log_entry, "^PLAYER")) %>%
  #   dplyr::filter(!stringr::str_detect(log_entry, "^CURRENT_PAGE_ID")) %>%
  #   dplyr::filter(!stringr::str_detect(log_entry, "^CURRENT_PAGE_NR")) %>%
  #   dplyr::filter(!stringr::str_detect(log_entry, "^PAGE_COUNT")) %>%
  # TODO: Currently, these events are not supported (might produce overfull data)
  # dplyr::filter(!stringr::str_detect(log_entry, "^TESTLETS_CLEARED_CODE")) %>%
  # dplyr::filter(!stringr::str_detect(log_entry, "^TESTLETS_TIMELEFT")) %>%
  # dplyr::count(log_entry) %>%
  # print(width = Inf)
}

parse_log_entry <- function(log_entry, log_events, type = "", sep = ":", parse = FALSE) {
  if (stringr::str_to_lower(type) %in% log_events) {
    if (parse) {
      ifelse(test = stringr::str_detect(log_entry, stringr::str_glue("^{type}")),
             yes = stringr::str_remove(log_entry, stringr::str_glue("^{type} {sep} ")) %>% jsonlite::parse_json(),
             no = NA_character_)
    } else {
      # These strings are usually in capital letters (whyever)
      ifelse(test = stringr::str_detect(log_entry, stringr::str_glue("^{type}")),
             yes = stringr::str_remove(log_entry, stringr::str_glue("^{type} {sep} ")) %>% stringr::str_to_lower(),
             no = NA_character_)

    }
  } else {
    NULL
  }
}
