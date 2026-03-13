#' Berechnet Bearbeitungs- und Ladezeiten anhand der Logdaten
#'
#' @param logs Tibble. Must be a logs tibble retrieved with `get_logs()` or `read_logs()`.
#'
#' @return Data frame mit diversen Zeiten und Zeitstempeln pro Unit bzw. Seite
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Berechnet geschätzte Bearbeitungs- und Ladezeiten für Units und Seiten.
#' - loading_time: Zeitspanne zwischen LOADING und RUNNING des Players pro Abspielung des Units
#' - unit_time: Zeitspanne zwischen RUNNING des Players und dem nächsten Zeitstempel 
#'            (meist LOADING des nächsten Units, manchmal Sitzungsende), pro Abspielung des Units
#' - unit_n_play: Anzahl der Abspielungen des Units in dieser Session
#' - n_loadings: Anzahl der Ladeversuche des Units (summiert über die Abspielungen,
#'            und über erfolgreiche und erfolgslose Ladeversuche)
#' - page_time: Zeitspanne zwischen CURRENT_PAGE_ID = [...] (Ladeabschluss der Seite) und
#'            Ladeabschluss der nächsten Seite bzw. bis Sitzungsende
#' - run_no_load_i: Player wurde als RUNNING, aber vorher nicht als LOADING geloggt.
#'                  In diesem Fall wurden Ladezeiten nicht berechnet.
#'                  
#' Daten gruppiert nach Gruppe, Login, Booklet, Unit_key.
#' Achtung: unit_alias wird hier nicht beachtet und es wird nicht danach gruppiert
#' oder sortiert, es wird nur am Ende als Information wieder eingefügt.
#'
#' @export

#TODO: Ermittlung von doppelten Ladeversuchen etc. schneller machen

estimate_unit_times <- function(logs) {
  cli_setting()
  groups_booklet <- setdiff(names(logs), c("unit_key", "unit_alias", "ts", "log_entry"))
  groups_unit <- setdiff(names(logs), c("ts", "log_entry", "unit_alias"))
  
  all_logs <-
    logs %>%
    dplyr::filter(
      # Delete duplicate page identifiers as these would contaminate page time estimation
      !log_entry %>% stringr::str_detect("(CURRENT_PAGE_NR|PAGE_COUNT)"),
      # This is only a constant message stream that is not interaction-based
      !log_entry %>% stringr::str_detect("TESTLETS_TIMELEFT")
    ) %>%
    dplyr::mutate(ts = as.numeric(ts)) 
  
  all_logs <- all_logs %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(groups_booklet, "ts")))) %>%
    # Unusable timestamps
    dplyr::filter(ts != 0) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet)))) %>%
    dplyr::mutate(
      unit_key = dplyr::case_when(
        # For legacy reasons
        stringr::str_detect(log_entry, "CURRENT_UNIT_ID") ~
          stringr::str_extract(log_entry, "\"(.+)\"", group = TRUE),
        .default = unit_key
      ),
      is_max_ts = ts == max(ts)
    ) %>%
    dplyr::filter((!is.na(unit_key) & unit_key != "") | is_max_ts) %>%
    tidyr::fill(unit_key, .direction = "downup") %>%
    dplyr::ungroup()
  
  all_ts <-
    all_logs %>%
    dplyr::mutate(
      ts_name = dplyr::case_when(
        # For the previous unit
        stringr::str_detect(log_entry, "CURRENT_UNIT_ID") ~ "unit_current_ts",
        stringr::str_detect(log_entry, "PLAYER = LOADING") ~ "unit_load_ts",
        stringr::str_detect(log_entry, "PLAYER = RUNNING") ~ "unit_start_ts",
        stringr::str_detect(log_entry, "CURRENT_PAGE_ID") ~ "page_start_ts",
        is_max_ts ~ "session_end_ts",
        log_entry == "PLAYER = PAUSED" ~ "n_paused",
        log_entry == "FOCUS : \"HAS_NOT\"" ~ "n_lost_focus",
        .default = NA_character_
      ),
      page_id = dplyr::case_when(
        # For legacy reasons
        log_entry == "CURRENT_PAGE_ID" ~ 0L,
        ts_name == "page_start_ts" ~ log_entry %>% stringr::str_extract("\\d+") %>% as.integer(),
        .default = NA_integer_
      )
    ) %>%
    dplyr::mutate(
      ts_name = dplyr::case_when(
        is_max_ts ~ "session_end_ts",
        .default = ts_name
      )
    )
  
  all_ts <- all_ts %>%
    # dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_unit, "ts_name")))) %>%
    # dplyr::mutate(
    #   n_play = ifelse(ts_name == "unit_start_ts", seq_along(ts_name), NA_integer_)
    # ) %>%
    # dplyr::group_by(dplyr::across(dplyr::all_of(groups_unit))) %>%
    # dplyr::arrange("ts", by_group=TRUE) %>%
    # tidyr::fill(n_play) %>%
    # dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_unit, "n_play")))) %>% # n_play m. E. 
    # fehlerhaft, 
    # weil tw. PLAYER=LOADING als Ende des letzten Units gewertet wird
    # dplyr::mutate(
    #   n_ts = seq_along(ts_name),
    #   ts_name = ifelse((n_ts == max(n_ts) & n_ts != 1), "unitplay_last_ts", ts_name), # Lea: 
    #   # Nicht nutzbar für Berechnung der Spielzeiten
    #   # einzelner Unit-Plays, weil dazwischen tw. andere Units eingespielt wurden.
    #   # Ferner wird tw. Der Start-ts auch als End-ts (letzter ts im Play) markiert.
    # ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(ts_name))
  
  unit_logs_prep <-
    all_ts %>%
    dplyr::filter(
      ts_name == "unit_start_ts" | ts_name == "unit_load_ts" | ts_name == "session_end_ts"
    )  %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_booklet))) %>%
    dplyr::arrange("ts", by_group=TRUE)

  # Nach wiederholten Ladeversuchen, und Unit-Starts ohne vorheriges Laden suchen
  print("Finde wiederholte Ladeversuche, sowie Unit-Starts ohne vorheriges Laden")
  unit_logs_prep$duplicate_loadings <- FALSE
  unit_logs_prep$run_no_load <- FALSE
  pb = utils::txtProgressBar(min = 0, max = nrow(unit_logs_prep), initial = 2) 
  for (row in 2:nrow(unit_logs_prep)) {
    unit_logs_prep$duplicate_loadings[[row]] <- (unit_logs_prep[row, "log_entry"] == "PLAYER = LOADING" &
                                            unit_logs_prep[row-1, "log_entry"] == "PLAYER = LOADING" &
                                            unit_logs_prep[row, "unit_key"] == unit_logs_prep[row-1, "unit_key"])
    unit_logs_prep$run_no_load[[row]] <- (unit_logs_prep[row, "log_entry"] == "PLAYER = RUNNING" &
                                            (unit_logs_prep[row-1, "log_entry"] != "PLAYER = LOADING" |
                                               unit_logs_prep[row, "unit_key"] != unit_logs_prep[row-1, "unit_key"]))
    utils::setTxtProgressBar(pb,row)
  }

  mult_loadings <-
    unit_logs_prep %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_unit))) %>%
    dplyr::summarise( # Adds up all loading attempts from various unit plays
      n_loadings = sum(duplicate_loadings==TRUE),
      .groups = "drop"
    )

  print("Berechne Unit-Bearbeitungs- und Ladezeiten")
  unit_logs_prep <- unit_logs_prep %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_booklet))) %>%
    dplyr::arrange("ts", by_group=TRUE) %>%  
    dplyr::mutate(
      ts_next = dplyr::lead(ts),
      unit_time = ts_next - ts, # Unit time hier definiert als Zeitspanne von Unit RUNNING 
      # bis zur nächsten Aktion innerhalb des Booklets
      ts_prev = dplyr::lag(ts),
      unit_loadtime = ts - ts_prev # Unit Loadtime hier definiert als Zeitspanne von 
      # letztem PLAYER=LOADING bis zu PLAYER=RUNNING
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
      unit_logs_i = c("unit_start_i", "unit_time_i", "unit_end_time_i", "unit_start_time_i", 
                      "unit_loadtime_i", "unit_loadstart_i", "run_no_load_i"))
  
  # Bring stats together
  unit_logs <-
    unit_logs_prep %>%
    dplyr::summarise(
      unit_start_time = min(ts),
      unit_n_play = length(unit_time),
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
    )
  
  unit_logs$n_loadings <- unit_logs$n_loadings + unit_logs$unit_n_play
  
  # Page times
  if (any(!is.na(all_ts$page_id))) {
    print("Berechne Seiten-Bearbeitungszeiten")
    unit_page_logs_prep <-
      all_ts %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet, "unit_key")))) %>%
      dplyr::mutate(
        is_max_ts = ts == max(ts)
      ) %>%
      dplyr::filter(
        ts_name %>% stringr::str_detect("^page_") | ts_name == "unit_load_ts" | ts_name == "session_end_ts"
      ) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet)))) %>%
      dplyr::arrange("ts", by_group=TRUE) %>%  
      dplyr::mutate(
        ts_next = dplyr::lead(ts),
        page_time = ts_next - ts
        # ts = ifelse(ts_name == "page_start_ts", ts, ts_next)
      ) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_unit)))) %>%
      dplyr::filter(
        ts_name != "unit_current_ts" & ts_name != "unit_load_ts" # These are only
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
        page_n_start = length(page_time),
        page_time = sum(page_time, na.rm=TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(
        unit_page_logs_start,
        by = dplyr::join_by(!!! c(groups_unit, "page_id"))
      ) %>%
      tidyr::nest(unit_page_logs = dplyr::any_of(c("page_id", "page_start_time", 
                                                   "page_n_start", "page_time", "page_logs_i")))
    
    # unit_page_logs$unit_page_logs[[4]]$page_logs_i
    
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
  }
  unit_logs <- unit_logs %>%
    dplyr::left_join(
      all_ts %>% dplyr::select(c(groups_unit, "unit_alias")),
      by = groups_unit,
      multiple = "any")
  return(unit_logs)
}