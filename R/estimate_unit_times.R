#' Estimates loading and stay times for units, unit plays and pages from log data
#'
#' @param logs Tibble. Must be a logs tibble retrieved with `get_logs()` or `read_logs()`.
#' @param use_unit_alias Boolean value. Determines whether to use unit_alias as unit identifier. If
#'        FALSE, use unit_key instead, which is the default.
#'        By default, unit_alias == unit_key (mapping to the Studio unit). In special cases -
#'        particularly for unit start pages and units that appear identically in multiple test
#'        booklets but are to be evaluated separately (which is rare; this has so far only
#'        applied to instruction pages or clarification questions) - a different unit_alias
#'        is intentionally assigned to logically identical units. In these cases, setting this
#'        parameter to TRUE can be advisable.
#' @param full_design Tibble. Design tibble containing block information,
#'        and all columns to be joined with logs. Relevant for finding lost focus events.
#' @param block_self_switch Boolean value. Were subjects able to switch to the next block themselves,
#'        or did they have to wait for the time to run out / the test conductor to switch blocks?
#'        This is relevant for finding lost focus events.
#'
#' @return Tibble containing various times and timestamps per participant unit and page
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Calculates estimated processing and loading times for units and pages. Excludes units that never
#' actually played. Duration units are in milliseconds.
#' New columns and nested columns:
#' - unit_start_time: Timestamp of first "PLAYER = RUNNING" log in this unit & booklet
#' - unit_n_play: Number of playbacks of the unit in this session, incomplete playbacks included
#' - n_run_no_load: Number of playbacks without previous loading log
#' - unit_time: Time interval at each playback of the unit between the player's "RUNNING" state and
#'   the next timestamp (usually the LOADING of the next unit, re-loading of the same unit,
#'   sometimes the end of the booklet), summed over playbacks
#' - unit_loadtime: Time interval between the player's "LOADING" and "RUNNING" states at each playback of the unit,
#'   including the duration of unsuccessful loading attempts, summed over playbacks
#' - unit_playbacks: Tibble containing one row with information for each playback of the unit
#'   - unit_start_i: Running number of unit playbacks
#'   - unit_time_i: Time interval at each playback of the unit between the player's "RUNNING" state and
#'   the next significant entry (usually the LOADING of the next unit, re-loading of the same unit,
#'   sometimes the end of the booklet)
#'   - unit_end_time_i: Timestamp of the next significant log entry (usually the LOADING of the next unit, re-loading of the same unit,
#'   sometimes the end of the booklet, i. e. the final timestamp within the current booklet playback) after start of current unit playback
#'   - unit_start_time_i: Timestamp of start of current unit playback
#'   - unit_loadtime_i: Time interval between the player's "LOADING" and "RUNNING" states at each playback of the unit,
#'   including the duration of unsuccessful loading attempts before playback
#'   - unit_loadstart_i: Timestamp of first "PLAYER = LOADING" for current playback of unit
#'   - run_no_load_i: Player was logged as RUNNING, but not previously as LOADING.
#'     In this case, load times were not calculated.
#' - n_failed_loadings: Number of failed loading attempts for the unit
#' - focus_events: Tibble containing all focus lost and regained events within each unit, based on log entries (FOCUS HAS or HAS NOT),
#'   as well as unit and page switches (which are considered as marking regained focus).
#'   NA for units with no focus lost events.
#'   - focus_event_ts: Timestamp of the focus event
#'   - focus_event_type: Type of event ("LOST" or "REGAINED", but REGAINED events are not returned)
#'   - focus_event_unfollowed: Boolean flag for lost focus events = TRUE when NOT followed by a regained event
#'     before another lost event appears
#'   - focus_lost_duration: For focus lost events, time until focus regained
#' - unit_page_logs: Tibble containing one row with information for each page of the unit.
#'   NA when a unit only has one page.
#'   - page_id: Digit(s) extracted from the log entry for this page
#'   - page_start_time: Timestamp at which page is logged in log entry
#'   - page_n_play: Number of playbacks of current page
#'   - page_time: Time interval between CURRENT_PAGE_ID = [...] (page load completion) and
#'     the next page load completion, the loading of a new unit, or until the end of the booklet,
#'     summed over playbacks of current page
#'   - page_logs_i: Tibble containing one row with information for each playback of the page
#'     - page_start_i: Running number of playbacks of the page
#'     - page_time_i: Time interval between CURRENT_PAGE_ID = [...] (page load completion) and
#'       the next page load completion, the loading of a new unit, or until the end of the booklet
#'     - page_end_time_i: Timestamp of the next page load completion, the loading of a new unit,
#'       or the end of the booklet (i. e. the final timestamp within the current booklet playback)
#'     - page_start_time_i: Timestamp of CURRENT_PAGE_ID = [...] (page load completion)
#' - unit_has_pages: Boolean, marks whether there is a unit_page_logs tibble
#' - unit_ident: Either a copy of unit_alias or unit_key, depending on the value of
#'   use_unit_alias
#' - device: Testing device, drawn from LOADCOMPLETE log entries
#' - osName: Operating system, drawn from LOADCOMPLETE log entries
#' - browserName: Browser used for testing, drawn from LOADCOMPLETE log entries
#' - browserVersion: Browser version, drawn from LOADCOMPLETE log entries
#' - testlet_no: Block (testlet) number
#'
#' Data grouped by group, login, booklet, and a unit identifier which depends on use_unit_alias.
#'
#' @export
#'
estimate_unit_times <- function(logs, use_unit_alias=FALSE,
                                full_design=NULL, block_self_switch=FALSE) {
  cli_setting()
  devicecolumns <- c("device", "osName", "browserName", "browserVersion")

  logs_cols <- c("group_id", "login_name", "login_code", "unit_alias", "unit_key", "ts", "log_entry", "booklet_id")
  checkmate::assert_tibble(logs)
  assert_cols(logs, logs_cols, "logs")
  
  checkmate::assert_tibble(full_design, null.ok = TRUE)
  checkmate::assert_logical(use_unit_alias, len = 1)
  checkmate::assert_logical(block_self_switch, len = 1)


  if (use_unit_alias) {
    logs$unit_ident <- logs$unit_alias
  } else {
    logs$unit_ident <- logs$unit_key
  }

  groups_booklet <- c("group_id", "login_name", "login_code", "booklet_id")
  groups_unit <- c("group_id", "login_name", "login_code", "booklet_id", "unit_ident")

  logs <- logs[!duplicated(logs), ]

  all_logs <-
    logs %>%
    dplyr::filter(
      # Delete duplicate page identifiers as these would contaminate page time estimation
      !.data$log_entry %>% stringr::str_detect("(CURRENT_PAGE_NR|PAGE_COUNT)"),
      # This is only a constant message stream that is not interaction-based
      !.data$log_entry %>% stringr::str_detect("TESTLETS_TIMELEFT"),
      !is.na(.data$booklet_id)
    ) %>%
    dplyr::mutate(ts = as.numeric(.data$ts))

  # Parse LOADCOMPLETE rows and extract browser, version and device
  dev_logs <- all_logs %>%
    dplyr::filter(stringr::str_detect(.data$log_entry, "LOADCOMPLETE"))

  if (nrow(dev_logs)==0) {
    warning("No LOADCOMPLETE logs (which contain device and browser information) found in the data.")
  } else {
    dev_logs <- dev_logs %>%
      dplyr::mutate(
        load_payload = stringr::str_remove(.data$log_entry, "^LOADCOMPLETE\\s*[:\\-]?\\s*")
      ) %>%
      dplyr::mutate(
        load_payload = stringr::str_replace_all(.data$load_payload, '""', '\\\\"')
      ) %>%
      dplyr::mutate(
        parsed_col = purrr::map(.data$load_payload, function(cell) {
        safe_limit <- 0
        # Apply fromJSON up to 5 times
        while (is.character(cell) && safe_limit < 5) {
          cell <- jsonlite::fromJSON(cell)
          safe_limit <- safe_limit + 1
        }
        return(cell)
      })) %>%
      tidyr::unnest_wider(.data$parsed_col) %>%
      dplyr::select(-.data$load_payload, -.data$screenSizeWidth, -.data$screenSizeHeight, -.data$loadTime)

    if (sum(is.na(dev_logs$browserName)) > 0 || sum(dev_logs$browserName == "NULL", na.rm = TRUE) > 0) {
      warning("Not all LOADCOMPLETE logs successfully parsed from json.")}
    }

  if (!is.null(full_design)) {
    if (!"testlet_no" %in% names(full_design)) {
      full_design$testlet_no <- NA_integer_
    }
    if (use_unit_alias) {
      full_design$unit_ident <- full_design$unit_alias
    } else {
      full_design$unit_ident <- full_design$unit_key
    }
    design_cols <- c("group_id", "login_name", "login_code", "unit_ident", "booklet_id", "testlet_no")
    checkmate::assert_tibble(full_design)
    assert_cols(full_design, design_cols, "design")

    all_logs <- all_logs %>%
    dplyr::left_join(
      full_design %>% dplyr::select(dplyr::all_of(c(intersect(names(all_logs), names(full_design)),
                                                    "testlet_no"))),
      by = intersect(names(.), intersect(names(all_logs), names(full_design))),
      multiple = "any"
    )

    if (!block_self_switch && all(is.na(all_logs$testlet_no))) {
      warning("No testlet_no values could be joined from full_design; automatic block switches cannot be identified.",
              call. = FALSE)
    } else if (!block_self_switch && any(is.na(all_logs$testlet_no))) {
      warning("Some log rows have no joined testlet_no; automatic block switch detection may be incomplete.",
              call. = FALSE)
    }
  } else {
    print("Design tibble with block info not provided; ignoring blocks for focus event computation.
          Any unit loading start will be treated as a focus regained event where focus was lost before.")
    block_self_switch <- TRUE
  }

  all_logs <- all_logs %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(groups_booklet, "ts")))) %>%
    # Unusable timestamps
    dplyr::filter(.data$ts != 0) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet)))) %>%
    dplyr::mutate(
      unit_ident = dplyr::case_when(
        # For legacy reasons
        stringr::str_detect(.data$log_entry, "CURRENT_UNIT_ID") ~
          stringr::str_extract(.data$log_entry, "\"(.+)\"", group = TRUE),
        .default = .data$unit_ident
      ),
      is_max_ts = .data$ts == max(.data$ts)
    ) %>%
    tidyr::fill(.data$unit_ident, .direction = "downup") %>%
    dplyr::filter((!is.na(.data$unit_ident) & .data$unit_ident != "") | .data$is_max_ts) %>%
    dplyr::ungroup()

  all_ts <-
    all_logs %>%
    dplyr::mutate(
      ts_name = dplyr::case_when(
        # For the previous unit
        stringr::str_detect(.data$log_entry, "CURRENT_UNIT_ID") ~ "unit_current_ts",
        stringr::str_detect(.data$log_entry, "PLAYER = LOADING") ~ "unit_load_ts",
        stringr::str_detect(.data$log_entry, "PLAYER = RUNNING") ~ "unit_start_ts",
        stringr::str_detect(.data$log_entry, "CURRENT_PAGE_ID") ~ "page_start_ts",
        .data$log_entry == "PLAYER = PAUSED" ~ "n_paused",
        .data$log_entry == "FOCUS : \"HAS_NOT\"" ~ "focus_lost_ts",
        .data$log_entry == "FOCUS : \"HAS\"" ~ "focus_regained_ts",
        .default = NA_character_
      ),
      page_id = dplyr::case_when(
        # For legacy reasons
        .data$log_entry == "CURRENT_PAGE_ID" ~ 0L,
        .data$ts_name == "page_start_ts" ~ .data$log_entry %>% stringr::str_extract("\\d+") %>% as.integer(),
        .default = NA_integer_
      )
    ) %>%
    dplyr::mutate(
      ts_name = dplyr::case_when(
        .data$is_max_ts ~ "booklet_end_ts",
        .default = .data$ts_name
      )
    )

  all_ts <- all_ts %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$ts_name))

  unit_logs_prep <-
    all_ts %>%
    dplyr::filter(
      .data$ts_name == "unit_start_ts" | .data$ts_name == "unit_load_ts" | .data$ts_name == "booklet_end_ts"
    )  %>%
    dplyr::mutate(playercode = dplyr::case_when(.data$log_entry == "PLAYER = LOADING" ~ 0,
                                                .data$log_entry == "PLAYER = RUNNING" ~ 1,
                                                .default = 999),
                  lag_unit_equal = dplyr::case_when(.data$unit_ident == dplyr::lag(.data$unit_ident) ~ 1,
                                                    .data$unit_ident != dplyr::lag(.data$unit_ident) ~ 0,
                                                    .default = 999)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_booklet))) %>%
    dplyr::arrange("ts", by_group=TRUE)

  # Nach wiederholten Ladeversuchen, und Unit-Starts ohne vorheriges Laden suchen
  unit_logs_prep <- unit_logs_prep %>%
    dplyr::mutate(
      failed_loading = dplyr::case_when(.data$playercode == 0 & (dplyr::lead(.data$playercode) != 1 |
                                                              .data$unit_ident != dplyr::lead(.data$unit_ident)
                                                              | is.na(dplyr::lead(.data$playercode))) ~ TRUE,
                                            .default = FALSE),
      run_no_load = dplyr::case_when(.data$playercode == 1 & (dplyr::lag(.data$playercode) != 0 |
                                                          .data$lag_unit_equal == 0 | is.na(dplyr::lag(.data$playercode))) ~ TRUE,
                                     .default = FALSE),
      duplicate_loading = dplyr::case_when(.data$playercode == 0 & dplyr::lag(.data$playercode) == 0 &
                                              .data$lag_unit_equal == 1 ~ TRUE,
                                            .default = FALSE)
    )

  mult_loadings <-
    unit_logs_prep %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_unit))) %>%
    dplyr::summarise( # Adds up all loading attempts from various unit plays
      n_failed_loadings = sum(.data$failed_loading),
      .groups = "drop"
    )

  print("Berechne Unit-Bearbeitungs- und Ladezeiten")
  unit_logs_prep <- unit_logs_prep %>%
    dplyr::filter(.data$duplicate_loading == FALSE) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_booklet))) %>%
    dplyr::arrange("ts", by_group=TRUE) %>%
    dplyr::mutate(
      ts_next = dplyr::lead(.data$ts),
      unit_time = .data$ts_next - .data$ts, # Unit time hier definiert als Zeitspanne von Unit RUNNING
      # bis zur naechsten Aktion innerhalb des Booklets
      ts_prev = dplyr::lag(.data$ts),
      unit_loadtime = .data$ts - .data$ts_prev # Unit Loadtime hier definiert als Zeitspanne von
      # PLAYER=LOADING bis zu PLAYER=RUNNING (erfolglose Ladeversuche inklusive)
    ) %>%
    dplyr::mutate(
      unit_loadtime = dplyr::case_when(
        .data$run_no_load == TRUE ~ NA, .default = .data$unit_loadtime
      ), # Ladezeiten loeschen, wenn vor RUNNING kein LOADING kam
      ts_prev = dplyr::case_when(
        .data$run_no_load == TRUE ~ NA, .default = .data$ts_prev # Dasselbe fuer ts_prev
      )) %>%
    dplyr::filter(.data$ts_name =="unit_start_ts") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_unit))) %>%
    dplyr::arrange("ts", by_group=TRUE) %>%
    dplyr::mutate(
      unit_start_i = seq_along(.data$unit_time)
    )

  # Multiple Unit plays
  unit_logs_starts <-
    unit_logs_prep %>%
    dplyr::select(dplyr::all_of(c(groups_unit,
                                  "unit_start_i",
                                  "unit_time_i" = "unit_time",
                                  "unit_start_time_i" = "ts",
                                  "unit_end_time_i" = "ts_next",
                                  "unit_loadtime_i" = "unit_loadtime",
                                  "unit_loadstart_i" = "ts_prev",
                                  "run_no_load_i" = "run_no_load"))) %>%
    tidyr::nest(
      unit_playbacks = c("unit_start_i", "unit_time_i", "unit_end_time_i", "unit_start_time_i",
                      "unit_loadtime_i", "unit_loadstart_i", "run_no_load_i"))

  # Bring stats together
  unit_logs <-
    unit_logs_prep %>%
    dplyr::summarise(
      unit_start_time = min(.data$ts),
      unit_n_play = length(.data$unit_time),
      n_run_no_load = sum(.data$run_no_load, na.rm = TRUE),
      unit_time = sum(.data$unit_time, na.rm = TRUE),
      unit_loadtime =  sum(.data$unit_loadtime, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      unit_logs_starts,
      by = dplyr::join_by(!!! groups_unit)
    ) %>%
    dplyr::left_join(
      mult_loadings,
      by = dplyr::join_by(!!! groups_unit)
    ) %>%
    dplyr::mutate(
      unit_loadtime = dplyr::case_when(.data$n_run_no_load == .data$unit_n_play ~ NA,
                .default = .data$unit_loadtime)
    )

  # Extract and combine focus lost and regained events
  print("Berechne Focus-Events")

  focus_events_combined <-
    all_ts %>%
    dplyr::filter(.data$ts_name == "focus_lost_ts" | .data$ts_name == "focus_regained_ts" |
                    .data$ts_name == "unit_load_ts" | .data$ts_name == "page_start_ts" |
                    .data$ts_name == "booklet_end_ts") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet)))) %>%
    dplyr::arrange("ts", by_group=TRUE)

  if (!block_self_switch) { # if blocks could not be switched actively by subjects
  focus_events_combined <-
    focus_events_combined %>%
    dplyr::mutate(
      auto_block_switch = dplyr::case_when(
        dplyr::lag(.data$testlet_no) != .data$testlet_no ~ TRUE,
        .default = FALSE
      )) %>%
    dplyr::mutate(
      auto_block_switch = dplyr::case_when(
        (.data$ts_name == "page_start_ts" &
           stringr::str_detect(dplyr::lag(.data$log_entry), "PLAYER = LOADING") &
           dplyr::lag(.data$auto_block_switch)) ~ TRUE,
        .default = .data$auto_block_switch
      ))
  } else { # dummy variable in case subjects could switch blocks themselves
  focus_events_combined <-
    focus_events_combined %>%
    dplyr::mutate(
      auto_block_switch = FALSE
      )
  }

  # Process each unit's focus events
  focus_events_nested <-
    focus_events_combined %>%
    dplyr::mutate(
      event_type = dplyr::case_when(
        .data$ts_name == "focus_lost_ts" ~ "LOST",
        .data$ts_name == "focus_regained_ts" ~ "REGAINED",
        (.data$ts_name == "unit_load_ts" & !.data$auto_block_switch) ~ "REGAINED",
        (.data$ts_name == "page_start_ts" & !.data$auto_block_switch) ~ "REGAINED",
        .default = NA_character_
      ),
      focus_event_ts = .data$ts
    ) %>%
    dplyr::filter(!is.na(.data$event_type) | .data$ts_name == "booklet_end_ts") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_booklet))) %>%
    dplyr::arrange(.data$focus_event_ts, by_group=TRUE) %>%
    dplyr::mutate(
      focus_next_ts = dplyr::lead(.data$focus_event_ts),
      next_event_type = dplyr::lead(.data$event_type),
      prev_event_type = dplyr::lag(.data$event_type),
      # Flag for lost focus events not followed by regain before another loss
      focus_event_unfollowed = dplyr::case_when(
        .data$event_type == "LOST" & .data$next_event_type == "LOST" ~ TRUE,
        .default = FALSE
      ),
      # Compute time during which focus was lost
      focus_lost_duration = dplyr::case_when(
        !.data$focus_event_unfollowed ~ .data$focus_next_ts - .data$focus_event_ts,
        .default = NA
      ),
      # Flag for regained focus events not preceded by loss (or preceded by another regain) LEGACY
      focus_event_unpreceded = dplyr::case_when(
        .data$event_type == "REGAINED" & (is.na(.data$prev_event_type) | .data$prev_event_type == "REGAINED") ~ TRUE,
        .default = FALSE
      )
    ) %>%
    dplyr::filter(.data$ts_name == "focus_lost_ts") %>%
    dplyr::select(dplyr::all_of(c(groups_unit, "focus_event_ts", "focus_lost_duration",
                                  "event_type", "focus_event_unfollowed"))) %>%
    dplyr::rename(focus_event_type = "event_type") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_unit))) %>%
    tidyr::nest(focus_events = c("focus_event_ts", "focus_event_type",
                                 "focus_event_unfollowed", "focus_lost_duration")) %>%
    dplyr::ungroup()

  # Add combined focus events if available
  if (nrow(focus_events_nested) > 0) {
    unit_logs <- unit_logs %>%
      dplyr::left_join(
        focus_events_nested,
        by = dplyr::join_by(!!! groups_unit)
      )
  } else {
    unit_logs$focus_events <- NA
  }

  # Page times
  if (any(!is.na(all_ts$page_id))) {
    print("Berechne Seiten-Bearbeitungszeiten")
    unit_page_logs_prep <-
      all_ts %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet, "unit_ident")))) %>%
      dplyr::mutate(
        unit_max_ts = .data$ts == max(.data$ts)
      ) %>%
      dplyr::filter(
        .data$ts_name %>% stringr::str_detect("^page_") | .data$ts_name == "unit_load_ts" | .data$ts_name == "booklet_end_ts"
      ) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet)))) %>%
      dplyr::arrange("ts", by_group=TRUE) %>%
      dplyr::mutate(
        ts_next = dplyr::lead(.data$ts),
        page_time = .data$ts_next - .data$ts
      ) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_unit)))) %>%
      dplyr::filter(
        .data$ts_name != "unit_current_ts" & .data$ts_name != "unit_load_ts" # These are only
        # used as endpoint of last page
      ) %>%
      # The first page is not logged before completion...
      tidyr::fill(.data$page_id, .direction = "up") %>%
      dplyr::filter(!is.na(.data$page_id)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_unit, "page_id")))) %>%
      dplyr::mutate(
        page_start_i = seq_along(.data$page_time)
      )

    # Separate Unit start and stay times
    unit_page_logs_start <-
      unit_page_logs_prep %>%
      dplyr::select(dplyr::all_of(c(groups_unit,
                                    "page_id",
                                    "page_start_i",
                                    "page_time_i" = "page_time",
                                    "page_start_time_i" = "ts",
                                    "page_end_time_i" = "ts_next"))) %>%
      tidyr::nest(
        page_logs_i = c("page_start_i", "page_time_i", "page_end_time_i", "page_start_time_i")
      )

    unit_page_logs <-
      unit_page_logs_prep %>%
      dplyr::summarise(
        page_start_time = min(.data$ts),
        page_n_play = length(.data$page_time),
        page_time = sum(.data$page_time, na.rm=TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(
        unit_page_logs_start,
        by = dplyr::join_by(!!! c(groups_unit, "page_id"))
      ) %>%
      tidyr::nest(unit_page_logs = dplyr::any_of(c("page_id", "page_start_time",
                                                   "page_n_play", "page_time", "page_logs_i")))


    unit_logs <- unit_logs %>%
      dplyr::left_join(
        unit_page_logs,
        by = groups_unit
      ) %>%
      dplyr::mutate(
        unit_has_pages = purrr::map_lgl(
          .data$unit_page_logs,
          function(x) !is.null(x) && nrow(x) > 0)
      )

  } else {
    print("Keine Seiten-IDs; Seiten-Bearbeitungszeiten werden nicht berechnet")
    unit_logs$unit_page_logs <- NA
  }

  if (nrow(dev_logs) > 0) {
    unit_logs <- unit_logs %>%
      dplyr::left_join(
        dev_logs %>% dplyr::select(dplyr::all_of(c(groups_booklet, devicecolumns))),
        by = groups_booklet,
        multiple = "any")}
  
  # Re-insert all unit IDs
  unit_logs <- unit_logs %>%
    dplyr::left_join(
      all_ts %>% dplyr::select(dplyr::all_of(c(groups_unit, "unit_alias", "unit_key", "unit_ident"))),
      by = groups_unit,
      multiple = "any")
  
  # Re-insert testlet_no (block number) if available
  if (!is.null(full_design)) {
    unit_logs <- unit_logs %>%
      dplyr::left_join(
        full_design %>% dplyr::select(dplyr::all_of(c(groups_unit, "testlet_no"))),
        by = groups_unit,
        multiple = "any")
  } else {
    unit_logs$testlet_no <- NA
  }

  unit_logs$unit_page_logs[unit_logs$unit_page_logs=="NULL"] <- NA
  unit_logs$focus_events[unit_logs$focus_events=="NULL"] <- NA
  unit_logs$unit_playbacks[unit_logs$unit_playbacks=="NULL"] <- NA

  return(unit_logs)
}


#' Mines information on audio and video clips and their play rates from the response data
#'
#' @param response_df Tibble. Pulled from test center using get_responses().
#'
#' @return Tibble containing number of playbacks per participant unit, clip and page.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Extracts the audio and video clip IDs and playback numbers from the "response" JSON strings in the 
#' input tibble, and unnests them for each participant.
#' New columns:
#' - id: Audio or video clip ID
#' - status: Unclear
#' - n_plays: Number of playbacks per participant and booklet, usually integer, sometimes float (?)
#' - media_ids: Concatenated unit_key and id. Contains identifiers for both the unit and the clip.
#'
#' @export
#'
estimate_audio_video_plays <- function(response_df) {
  audiomask_resp <- stringr::str_detect(response_df$responses, "audio")
  audio_resp_rows <- response_df[audiomask_resp, ]
  parsed_audios <- audio_resp_rows %>%
    dplyr::filter(!is.na(.data$responses)) %>%
    dplyr::mutate(
      parsed_col = purrr::map(.data$responses, function(cell) {
        safe_limit <- 0
        # Apply fromJSON up to 5 times
        while (is.character(cell) && safe_limit < 5) {
          cell <- jsonlite::fromJSON(cell)
          safe_limit <- safe_limit + 1
        }
        return(cell)
      })) %>%
    dplyr::select(.data$group_id, .data$login_name, .data$login_code, .data$booklet_id, .data$unit_key, .data$page_no, .data$parsed_col)
  
  if (nrow(parsed_audios) > 0) {
    parsed_audios <- parsed_audios %>%
      tidyr::unnest(.data$parsed_col) %>%
      dplyr::filter(stringr::str_detect(.data$id, "audio"))
  }

  videomask_resp <- stringr::str_detect(response_df$responses, "video")
  video_resp_rows <- response_df[videomask_resp, ]
  parsed_videos <- video_resp_rows %>%
    dplyr::filter(!is.na(.data$responses)) %>%
    dplyr::mutate(
      parsed_col = purrr::map(.data$responses, function(cell) {
        safe_limit <- 0
        # Apply fromJSON up to 5 times
        while (is.character(cell) && safe_limit < 5) {
          cell <- jsonlite::fromJSON(cell)
          safe_limit <- safe_limit + 1
        }
        return(cell)
      })) %>%
    dplyr::select(.data$group_id, .data$login_name, .data$login_code, .data$booklet_id, .data$unit_key, .data$page_no, .data$parsed_col)
    
  if (nrow(parsed_videos) > 0) {
    parsed_videos <- parsed_videos %>%
    tidyr::unnest(.data$parsed_col) %>%
    dplyr::filter(stringr::str_detect(.data$id, "video"))
  }

  all_parsed <- dplyr::bind_rows(list(parsed_videos, parsed_audios))
  all_parsed$media_unit_ids <- paste(all_parsed$unit_key, all_parsed$id)
  all_parsed <- dplyr::rename(all_parsed, n_plays = .data$value)

  return(all_parsed)
}
