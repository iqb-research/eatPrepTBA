#' Estimates loading and stay times for units, unit plays and pages from log data
#'
#' @param logs Tibble. Must be a logs tibble retrieved with `get_logs()` or `read_logs()`.
#' @param use_unit_alias Boolean value. Determines whether to use unit_alias as unit identifier. If
#'        FALSE, use unit_key instead, which is the default.
#'        By default, unit_alias == unit_key (mapping to the Studio unit). In special cases —
#'        particularly for unit start pages and units that appear identically in multiple test
#'        booklets but are to be evaluated separately (which is rare; this has so far only
#'        applied to instruction pages or clarification questions) — a different unit_alias
#'        is intentionally assigned to logically identical units. In these cases, setting this
#'        parameter to TRUE can be advisable.
#' @param full_design Tibble. Design tibble containing block information,
#'        and all columns to be joined with logs. Relevant for finding lost focus events.
#' @param block_self_switch Boolean value. Were subjects able to switch to the next block themselves,
#'        or did they have to wait for the time to run out / the test conductor to switch blocks?
#'        This is relevant for finding lost focus events.
#' @param include_environment Boolean value. If TRUE, session-level browser,
#'        operating system, device, screen, and initial load-time metadata from
#'        `LOADCOMPLETE` logs are joined to the unit-time output via
#'        [summarise_log_environment()]. Sessions without `LOADCOMPLETE` are kept
#'        without warnings and receive missing environment fields plus diagnostic
#'        count flags.
#'
#' @return Tibble containing various times and timestamps per unit and page
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Calculates estimated processing and loading times for units and pages. Excludes units that never
#' actually played.
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
#' - n_invalid_current_page_id_events: Number of raw negative-integer `CURRENT_PAGE_ID`
#'   events such as `CURRENT_PAGE_ID = -1` within the unit.
#' - delay_first_valid_page_id_ms: Time from first `PLAYER = RUNNING` to the first
#'   valid page ID; `0` if a valid page ID was already observed before the first
#'   `PLAYER = RUNNING`, and `NA` if no valid page ID is observed.
#' - valid_page_id_before_running: Boolean flag for units where a valid page ID
#'   was observed before the first `PLAYER = RUNNING`.
#' - unmapped_page_time_ms: Raw indicator for intervals after unmapped negative
#'   page IDs until the next valid page ID, unit load, or booklet end. This is not
#'   the total unassigned unit time.
#' - focus_events: Tibble containing all focus lost and regained events within each unit, based on log entries (FOCUS HAS or HAS NOT),
#'   as well as unit and page switches (which are considered as marking regained focus)
#'   - focus_event_ts: Timestamp of the focus event
#'   - focus_event_type: Type of event ("LOST" or "REGAINED", but REGAINED events are not returned)
#'   - focus_event_unfollowed: Boolean flag for lost focus events = TRUE when NOT followed by a regained event
#'     before another lost event appears
#'   - focus_lost_duration: For focus lost events, time until focus regained
#' - unit_page_logs: Tibble containing one row with information for each page of the unit.
#'   NULL when a unit only has one page.
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
#' - Environment columns from [summarise_log_environment()] if
#'   `include_environment = TRUE`, including `browser_name`, `browser_version`,
#'   `os_name`, `device`, screen-size fields, `load_time`, and LOADCOMPLETE
#'   diagnostic columns.
#'
#' Data grouped by group, login, booklet, and a unit identifier which depends on use_unit_alias.
#'
#' @export
#'
estimate_unit_times <- function(logs, use_unit_alias=FALSE,
                                full_design=NULL, block_self_switch=FALSE,
                                include_environment=TRUE) {
  cli_setting()

  logs_cols <- c("unit_alias", "unit_key", "ts", "log_entry", "booklet_id")
  checkmate::assert_tibble(logs)
  assert_cols(logs, logs_cols, "logs")
  checkmate::assert_tibble(full_design, null.ok = TRUE)

  checkmate::assert_logical(use_unit_alias, len = 1)
  checkmate::assert_logical(block_self_switch, len = 1)
  checkmate::assert_logical(include_environment, len = 1)


  if (use_unit_alias) {
    logs$unit_ident <- logs$unit_alias
  } else {
    logs$unit_ident <- logs$unit_key
  }

  groups_booklet <- setdiff(names(logs), c("unit_key", "unit_alias", "unit_ident", "ts", "log_entry"))
  groups_unit <- setdiff(names(logs), c("ts", "log_entry", "unit_key", "unit_alias"))

  logs <- logs[!duplicated(logs), ]

  all_logs <-
    logs %>%
    dplyr::filter(
      # Delete duplicate page identifiers as these would contaminate page time estimation
      !log_entry %>% stringr::str_detect("(CURRENT_PAGE_NR|PAGE_COUNT)"),
      # This is only a constant message stream that is not interaction-based
      !log_entry %>% stringr::str_detect("TESTLETS_TIMELEFT"),
      !is.na(booklet_id)
    ) %>%
    dplyr::mutate(ts = as.numeric(ts))

  if (!is.null(full_design)) {
    if (!"testlet_no" %in% names(full_design)) {
      full_design$testlet_no <- NA_integer_
    }

    all_logs <- all_logs %>%
    dplyr::left_join(
      full_design %>% dplyr::select(dplyr::all_of(c(intersect(names(all_logs), names(full_design)),
                                                    "testlet_no"))),
      by = intersect(names(.), intersect(names(all_logs), names(full_design))),
      multiple = "first"
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
    dplyr::filter(ts != 0) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet)))) %>%
    dplyr::mutate(
      unit_ident = dplyr::case_when(
        # For legacy reasons
        stringr::str_detect(log_entry, "CURRENT_UNIT_ID") ~
          stringr::str_extract(log_entry, "\"(.+)\"", group = TRUE),
        .default = unit_ident
      ),
      is_max_ts = ts == max(ts)
    ) %>%
    tidyr::fill(unit_ident, .direction = "downup") %>%
    dplyr::filter((!is.na(unit_ident) & unit_ident != "") | is_max_ts) %>%
    dplyr::ungroup()

  all_ts <-
    all_logs %>%
    dplyr::mutate(
      current_page_id_number = log_parse_current_page_id_number(log_entry),
      invalid_current_page_id = log_current_page_id_is_negative_integer(log_entry),
      ts_name = dplyr::case_when(
        # For the previous unit
        stringr::str_detect(log_entry, "CURRENT_UNIT_ID") ~ "unit_current_ts",
        stringr::str_detect(log_entry, "PLAYER = LOADING") ~ "unit_load_ts",
        stringr::str_detect(log_entry, "PLAYER = RUNNING") ~ "unit_start_ts",
        invalid_current_page_id ~ "unmapped_page_ts",
        stringr::str_detect(log_entry, "CURRENT_PAGE_ID") ~ "page_start_ts",
        log_entry == "PLAYER = PAUSED" ~ "n_paused",
        log_entry == "FOCUS : \"HAS_NOT\"" ~ "focus_lost_ts",
        log_entry == "FOCUS : \"HAS\"" ~ "focus_regained_ts",
        .default = NA_character_
      ),
      page_id = dplyr::case_when(
        # For legacy reasons
        log_entry == "CURRENT_PAGE_ID" ~ 0L,
        ts_name == "page_start_ts" & !is.na(current_page_id_number) ~ current_page_id_number,
        .default = NA_integer_
      )
    ) %>%
    dplyr::mutate(
      ts_name = dplyr::case_when(
        .data$is_max_ts & .data$ts_name %in% c("page_start_ts", "unmapped_page_ts") ~ .data$ts_name,
        .data$is_max_ts ~ "booklet_end_ts",
        .default = ts_name
      ),
      is_booklet_end = .data$is_max_ts
    )

  all_ts <- all_ts %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(ts_name))

  first_unit_starts <- all_ts %>%
    dplyr::filter(.data$ts_name == "unit_start_ts") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_unit))) %>%
    dplyr::summarise(
      first_unit_start_time = min(.data$ts, na.rm = TRUE),
      .groups = "drop"
    )

  first_valid_page_ids <- all_ts %>%
    dplyr::filter(.data$ts_name == "page_start_ts") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_unit))) %>%
    dplyr::slice_min(.data$ts, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      dplyr::across(dplyr::all_of(groups_unit)),
      first_valid_page_id_ts = .data$ts
    )

  unmapped_page_times <- all_ts %>%
    dplyr::inner_join(first_unit_starts, by = groups_unit) %>%
    dplyr::filter(
      is.na(.data$first_unit_start_time) |
        .data$ts >= .data$first_unit_start_time |
        .data$is_booklet_end
    ) %>%
    dplyr::filter(
      .data$ts_name %in% c("page_start_ts", "unit_load_ts", "booklet_end_ts") |
        .data$is_booklet_end |
        .data$invalid_current_page_id
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_booklet))) %>%
    dplyr::arrange(.data$ts, .by_group = TRUE) %>%
    dplyr::mutate(
      ts_next = dplyr::lead(.data$ts),
      unmapped_page_interval = dplyr::case_when(
        .data$invalid_current_page_id & !is.na(.data$ts_next) &
          .data$ts_next >= .data$ts ~ .data$ts_next - .data$ts,
        .default = NA_real_
      )
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_unit))) %>%
    dplyr::summarise(
      unmapped_page_time_ms = sum(.data$unmapped_page_interval, na.rm = TRUE),
      .groups = "drop"
    )

  page_id_diagnostics <- all_ts %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_unit))) %>%
    dplyr::summarise(
      n_invalid_current_page_id_events = sum(.data$invalid_current_page_id, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(first_unit_starts, by = groups_unit) %>%
    dplyr::left_join(first_valid_page_ids, by = groups_unit) %>%
    dplyr::left_join(unmapped_page_times, by = groups_unit) %>%
    dplyr::mutate(
      n_invalid_current_page_id_events = as.integer(dplyr::coalesce(
        .data$n_invalid_current_page_id_events,
        0L
      )),
      delay_first_valid_page_id_ms = dplyr::case_when(
        !is.na(.data$first_unit_start_time) & !is.na(.data$first_valid_page_id_ts) ~
          pmax(.data$first_valid_page_id_ts - .data$first_unit_start_time, 0),
        .default = NA_real_
      ),
      valid_page_id_before_running = !is.na(.data$first_unit_start_time) &
        !is.na(.data$first_valid_page_id_ts) &
        .data$first_valid_page_id_ts < .data$first_unit_start_time,
      unmapped_page_time_ms = dplyr::coalesce(.data$unmapped_page_time_ms, 0)
    ) %>%
    dplyr::select(
      -dplyr::any_of(c("first_unit_start_time", "first_valid_page_id_ts"))
    )

  unit_logs_prep <-
    all_ts %>%
    dplyr::filter(
      ts_name == "unit_start_ts" | ts_name == "unit_load_ts" |
        ts_name == "booklet_end_ts" | is_booklet_end
    )  %>%
    dplyr::mutate(playercode = dplyr::case_when(log_entry == "PLAYER = LOADING" ~ 0,
                                                log_entry == "PLAYER = RUNNING" ~ 1,
                                                .default = 999),
                  lag_unit_equal = dplyr::case_when(unit_ident == dplyr::lag(unit_ident) ~ 1,
                                                    unit_ident != dplyr::lag(unit_ident) ~ 0,
                                                    .default = 999)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_booklet))) %>%
    dplyr::arrange("ts", by_group=TRUE)

  # Nach wiederholten Ladeversuchen, und Unit-Starts ohne vorheriges Laden suchen
  unit_logs_prep <- unit_logs_prep %>%
    dplyr::mutate(
      failed_loading = dplyr::case_when(playercode == 0 & (dplyr::lead(playercode) != 1 |
                                                              unit_ident != dplyr::lead(unit_ident)
                                                              | is.na(dplyr::lead(playercode))) ~ TRUE,
                                            .default = FALSE),
      run_no_load = dplyr::case_when(playercode == 1 & (dplyr::lag(playercode) != 0 |
                                                          lag_unit_equal == 0 | is.na(dplyr::lag(playercode))) ~ TRUE,
                                     .default = FALSE),
      duplicate_loading = dplyr::case_when(playercode == 0 & dplyr::lag(playercode) == 0 &
                                              lag_unit_equal == 1 ~ TRUE,
                                            .default = FALSE)
    )

  mult_loadings <-
    unit_logs_prep %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_unit))) %>%
    dplyr::summarise( # Adds up all loading attempts from various unit plays
      n_failed_loadings = sum(failed_loading),
      .groups = "drop"
    )

  print("Berechne Unit-Bearbeitungs- und Ladezeiten")
  unit_logs_prep <- unit_logs_prep %>%
    dplyr::filter(duplicate_loading == FALSE) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_booklet))) %>%
    dplyr::arrange("ts", by_group=TRUE) %>%
    dplyr::mutate(
      ts_next = dplyr::lead(ts),
      unit_time = ts_next - ts, # Unit time hier definiert als Zeitspanne von Unit RUNNING
      # bis zur nächsten Aktion innerhalb des Booklets
      ts_prev = dplyr::lag(ts),
      unit_loadtime = ts - ts_prev # Unit Loadtime hier definiert als Zeitspanne von
      # PLAYER=LOADING bis zu PLAYER=RUNNING (erfolglose Ladeversuche inklusive)
    ) %>%
    dplyr::mutate(
      unit_loadtime = dplyr::case_when(
        run_no_load == TRUE ~ NA, .default = unit_loadtime
      ), # Ladezeiten löschen, wenn vor RUNNING kein LOADING kam
      ts_prev = dplyr::case_when(
        run_no_load == TRUE ~ NA, .default = ts_prev # Dasselbe für ts_prev
      )) %>%
    dplyr::filter(ts_name =="unit_start_ts") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_unit))) %>%
    dplyr::arrange("ts", by_group=TRUE) %>%
    dplyr::mutate(
      unit_start_i = seq_along(unit_time)
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
      unit_start_time = min(ts),
      unit_n_play = length(unit_time),
      n_run_no_load = sum(run_no_load, na.rm = TRUE),
      unit_time = sum(unit_time, na.rm = TRUE),
      unit_loadtime =  sum(unit_loadtime, na.rm = TRUE),
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
      unit_loadtime = dplyr::case_when(n_run_no_load==unit_n_play ~ NA,
                .default = unit_loadtime)
    )

  # Extract and combine focus lost and regained events
  print("Berechne Focus-Events")

  focus_events_combined <-
    all_ts %>%
    dplyr::filter(ts_name == "focus_lost_ts" | ts_name == "focus_regained_ts" |
                    ts_name == "unit_load_ts" | ts_name == "page_start_ts" |
                    ts_name == "booklet_end_ts" | is_booklet_end) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet)))) %>%
    dplyr::arrange("ts", by_group=TRUE)

  if (!block_self_switch) { # if blocks could not be switched actively by subjects
  focus_events_combined <-
    focus_events_combined %>%
    dplyr::mutate(
      auto_block_switch = dplyr::case_when(
        dplyr::lag(testlet_no) != testlet_no ~ TRUE,
        .default = FALSE
      )) %>%
    dplyr::mutate(
      auto_block_switch = dplyr::case_when(
        (ts_name == "page_start_ts" &
           stringr::str_detect(dplyr::lag(log_entry), "PLAYER = LOADING") &
           dplyr::lag(auto_block_switch)) ~ TRUE,
        .default = auto_block_switch
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
        ts_name == "focus_lost_ts" ~ "LOST",
        ts_name == "focus_regained_ts" ~ "REGAINED",
        (ts_name == "unit_load_ts" & !auto_block_switch) ~ "REGAINED",
        (ts_name == "page_start_ts" & !auto_block_switch) ~ "REGAINED",
        .default = NA_character_
      ),
      focus_event_ts = ts
    ) %>%
    dplyr::filter(!is.na(event_type) | ts_name == "booklet_end_ts" | is_booklet_end) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_booklet))) %>%
    dplyr::arrange("focus_event_ts", by_group=TRUE) %>%
    dplyr::mutate(
      focus_next_ts = dplyr::lead(focus_event_ts),
      next_event_type = dplyr::lead(event_type),
      prev_event_type = dplyr::lag(event_type),
      # Flag for lost focus events not followed by regain before another loss
      focus_event_unfollowed = dplyr::case_when(
        event_type == "LOST" & next_event_type == "LOST" ~ TRUE,
        .default = FALSE
      ),
      # Compute time during which focus was lost
      focus_lost_duration = dplyr::case_when(
        !focus_event_unfollowed ~ focus_next_ts - focus_event_ts,
        .default = NA
      ),
      # Flag for regained focus events not preceded by loss (or preceded by another regain) LEGACY
      focus_event_unpreceded = dplyr::case_when(
        event_type == "REGAINED" & (is.na(prev_event_type) | prev_event_type == "REGAINED") ~ TRUE,
        .default = FALSE
      )
    ) %>%
    dplyr::filter(ts_name == "focus_lost_ts") %>%
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
  if (any(all_ts$ts_name == "page_start_ts", na.rm = TRUE)) {
    print("Berechne Seiten-Bearbeitungszeiten")
    unit_page_logs_prep <-
      all_ts %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet, "unit_ident")))) %>%
      dplyr::mutate(
        unit_max_ts = ts == max(ts)
      ) %>%
      dplyr::filter(
        .data$ts_name == "page_start_ts" |
          .data$invalid_current_page_id |
          .data$ts_name == "unit_load_ts" |
          .data$ts_name == "booklet_end_ts" |
          .data$is_booklet_end
      ) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet)))) %>%
      dplyr::arrange("ts", by_group=TRUE) %>%
      dplyr::mutate(
        ts_next = dplyr::case_when(
          .data$is_booklet_end ~ .data$ts,
          .default = dplyr::lead(.data$ts)
        ),
        page_time = ts_next - ts
      ) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_unit)))) %>%
      dplyr::filter(
        ts_name != "unit_current_ts" & ts_name != "unit_load_ts" & !invalid_current_page_id # These are only
        # used as endpoint of last page
      ) %>%
      # The first page is not logged before completion...
      tidyr::fill(page_id, .direction = "up") %>%
      dplyr::filter(!is.na(page_id)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_unit, "page_id")))) %>%
      dplyr::mutate(
        page_start_i = seq_along(page_time)
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
        page_start_time = min(ts),
        page_n_play = length(page_time),
        page_time = sum(page_time, na.rm=TRUE),
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
        unit_has_pages = purrr::map_lgl(unit_page_logs, function(x) !is.null(x))
      )

  } else {
    print("Keine Seiten-IDs; Seiten-Bearbeitungszeiten werden nicht berechnet")
    unit_logs$unit_page_logs <- NA
    unit_logs$unit_has_pages <- FALSE
  }

  unit_logs <- unit_logs %>%
    dplyr::left_join(
      page_id_diagnostics,
      by = groups_unit
    ) %>%
    dplyr::mutate(
      n_invalid_current_page_id_events = dplyr::coalesce(
        .data$n_invalid_current_page_id_events,
        0L
      ),
      unmapped_page_time_ms = dplyr::coalesce(.data$unmapped_page_time_ms, 0)
    )

  unit_logs <- unit_logs %>%
    dplyr::left_join(
      all_ts %>% dplyr::select(dplyr::all_of(c(groups_unit, "unit_alias", "unit_key", "unit_ident"))),
      by = groups_unit,
      multiple = "any")

  if (include_environment) {
    unit_logs <- add_log_environment_to_unit_times(
      unit_logs = unit_logs,
      logs = logs,
      session_cols = groups_booklet
    )
  }

  return(unit_logs)
}

add_log_environment_to_unit_times <- function(unit_logs, logs, session_cols) {
  environment <- summarise_log_environment(logs, session_cols = session_cols)
  environment_cols <- setdiff(names(environment), session_cols)
  environment_cols <- setdiff(environment_cols, names(unit_logs))

  if (length(environment_cols) == 0L) {
    return(unit_logs)
  }

  dplyr::left_join(
    unit_logs,
    environment[, c(session_cols, environment_cols), drop = FALSE],
    by = session_cols
  )
}
