#' Computes quantile tables of stay times
#'
#' Author: Philipp Franikowski, restructured by Lea Musiolek
#'
#' @param fach String. Letter denoting the school subject in question.
#' @param log_times Data frame. Log data with stay times. Result of pulling
#' log data with eatPrepTBA::get_logs() and then using eatPrepTBA::estimate_unit_times().
#' Can contain irrelevant units/subjects too, as these get sorted out before use in the function.
#' @param unit_domains Data frame. Three string variables: subject (should equal fach),
#' domain ('<fach><Kompetenz>'), unit_key. Should contain each relevant unit_key once, and no others.
#' Important for assigning the right subject and domain to each unit key down the line, and selecting 
#' only the units that are relevant for the current table. 
#' Can be generated from the blocks.xlsx used for generating the test booklets, using the 
#' generate_unit_domains() function in this file.
#' @param final_responses Data frame. Contains the item-wise and respondent-wise coded responses,
#' ideally corrected for switches etc. Relevant variables: 
#' booklet_id, IDSTUD, group_id, login_name, login_code,
#' unit_key, variable_page
#' Can contain irrelevant units/subjects too, as these get sorted out before use in the function.
#' @param unit_cs Data frame. Unit-wise coding schemes, exported from IQB Studio and enriched via 
#' add_coding_scheme().
#' Relevant variables: unit_key, unit_codes, variable_label, variable_page, variable_id
#' Can contain irrelevant units/subjects too, as these get sorted out before use in the function.
#' @param unit_meta Data frame. Unit-wise metadata, exported from IQB Studio and enriched via 
#' add_metadata().
#' Relevant variables: ws_id, unit_id, unit_key, unit_label, unit_metadata, item_metadata, 
#' Aufgabenzeit
#' Can contain irrelevant units/subjects too, as these get sorted out before use in the function.
#' @param students_select Vector of strings. If necessary, contains the IDSTUDs of students to
#' include in the analysis. Otherwise, NULL
#' @param FS_marker String. If this string appears in final_responses$booklet_id, then the respective
#' booklet is treated as containing a special needs school study design.
#' @param output_path String. Directory to store prepared tables in.
#'
#' @return A tibble. Also saves large tibble with name `tab_\\{domain\\}.RData` under output_path, 
#' including quantile dot plots.
#' This can then be incorporated into a quarto document as described in the example below.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Takes various data sources with unit, page and participant stay times and metadata,
#' and computes quantiles for use in reports. Uses the layout_staytime_tables function
#' to layout them for quarto reports.
#' IMPORTANT: 
#' - Although item IDs are listed in the final outputs, these functions cannot compute 
#' actual item stay times, as the finest-grained stay times in the log data are page 
#' stay times. One page often contains several items.
#' - The function assumes that there is one study design for regular schools (RS)
#' and one for special needs schools (FS), and that the booklets with FS design can be identified
#' with certainty by a string which appears as a substring of final_responses$booklet_id entries.
#' This string is passed to the function as the argument FS_marker.
#' - For the function to work properly, the working directory needs to be set to the location of
#' the script you are running it from before running. See example.
#'
#' @details
#' The resulting saved tibble can be incorporated into a Quarto document using
#' \code{load("output_path/tab_\\{domain\\}.RData")}
#'
#' \preformatted{
#' ::: panel-tabset
#'
#' ### Units
#'
#' \verb{```{r}}
#' #| column: screen
#' tab
#' \verb{```}
#'
#' ### Items
#'
#' \verb{```{r}}
#' #| column: screen
#' tab_item
#' \verb{```}
#'
#' :::
#' }
#'
#' @examples
#' \dontrun{
#' fach <- c("F")
#' FS_marker <- "S"
#'
#' data_path <- "..."
#' prep_path <- "..."
#' db_path <- "..."
#' output_path <- "..."
#'
#' unit_meta <- readRDS(paste(c(db_path, "units_md.rds"), collapse="")) # unit meta data
#' unit_cs <- readRDS(paste(c(db_path, "units_cs.rds"), collapse="")) # unit coding scheme
#' final_responses <- readRDS(paste(c(data_path, "responses_xyz.rds"), 
#' collapse="")) #final results file
#'
#' log_times <- readRDS(paste(c(prep_path, "log_times_", fach, ".rds"), collapse="")) # output
#' # of estimate_unit_times()
#' unit_domains <- readRDS(paste(c(data_path, "unit_domains_", fach, ".rds"), collapse="")) # table
#' # with one row per unit, columns for unit_key, school subject as letter, and testing domain
#' # (subject and competence type) as 2-letter combination
#'
#' setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set wd to current directory
#' eatPrepTBA::compute_staytime_tables(fach,
#'                                     log_times,
#'                                     unit_domains,
#'                                     final_responses,
#'                                     unit_cs,
#'                                     unit_meta,
#'                                     students_select,
#'                                     FS_marker,
#'                                     output_path)
#' }
#'
#' # Requires packages eatPrepTBA, tidyverse, reactable, and htmltools there.
#'
#' @export


# TODO: Remove extra rows in final table
# TODO: Update column checks in the beginning
# TODO: Make sure variable_id isn't confusing the computation of page staytimes

compute_staytime_tables <- function(fach,
                                    log_times,
                                    unit_domains,
                                    final_responses,
                                    unit_cs,
                                    unit_meta,
                                    students_select,
                                    FS_marker,
                                    output_path) {
  
  min_plays = 2 # Minimal necessary plays of the page for an item to be included
  
  # input validation
  checkmate::assert_character(fach, len=1)
  checkmate::assert_character(FS_marker, len=1)
  checkmate::assert_character(output_path, len=1)
  checkmate::assert_character(students_select, null.ok = TRUE)
  checkmate::assert_data_frame(log_times)
  
  unit_domains_cols <- c("unit_key", "subject", "domain")
  checkmate::assert_data_frame(unit_domains)
  assert_cols(unit_domains, unit_domains_cols, "unit_domains")
  # 
  # final_responses_cols <- c("booklet_id", "IDSTUD", "group_id", "login_name",
  #                           "login_code", "unit_key", "variable_page")
  # checkmate::assert_data_frame(final_responses)
  # assert_cols(final_responses, final_responses_cols, "final_responses")
  
  
  unit_page_logtimes <-
    log_times %>%
    tidyr::unnest(unit_page_logs, keep_empty = TRUE) %>%
    dplyr::mutate(
      page_time2 = dplyr::case_when(
        .data$unit_has_pages ~ .data$page_time,
        .default = .data$unit_time
      ),
      page_id = dplyr::coalesce(.data$page_id, 0)
    )
  # rm(log_times)
  
  # unit_cs umformen: irrelevante Units rausschmeißen
  unit_cs <-
    unit_cs[which(unit_cs$unit_key %in% unit_domains$unit_key), ]
  
  # Unit-Details auspacken
  unit_cs_big <-
    unit_cs %>%
    tidyr::unnest(unit_codes, keep_empty = TRUE)
  # rm(unit_cs)
  
  # unit_cs_cols <- c("unit_key", "variable_label", "variable_page", "variable_id")
  # checkmate::assert_data_frame(unit_cs_big)
  # assert_cols(unit_cs_big, unit_cs_cols, "unit_cs unnested")
  
  # Korrektur fuer die Markieritems
  unit_cs_corrected <-
    unit_cs_big %>%
    dplyr::mutate(
      # variable_temp = dplyr::case_when(
      #   grepl("^[0-9]+$", variable_label) ~ as.integer(variable_label),
      #   .default = NA),
      variable_temp = tryCatch({readr::parse_integer(.data$variable_label)},
                               warning = function(w) {
                                 NA
                               }),
      page_id = dplyr::case_when(
        !is.na(.data$variable_temp) ~ .data$variable_temp,
        .default = as.integer(.data$variable_page)
      )
    )
  # rm(unit_cs_big)
  
  # Umformen: irrelevante Units rausschmeißen
  final_responses <-
    final_responses[which(final_responses$unit_key %in% unit_domains$unit_key), ] %>%
    dplyr::left_join(unit_domains, 
                     by="unit_key")
  
  final_resp <- final_responses %>%
    dplyr::mutate(
      design = dplyr::case_when(
        stringr::str_detect(.data$booklet_id, FS_marker) ~ "FS",
        .default = "RS"
      ),
      booklet_id = stringr::str_to_upper(.data$booklet_id)
    )
  # rm(final_responses)
  
  resp_pages <-
    final_resp %>%
    dplyr::mutate(page_id = as.integer(.data$page_id)) %>%
    dplyr::left_join(unit_cs_corrected %>% 
                       dplyr::select(unit_key, variable_id, page_id),
                     by = c("unit_key", "page_id"),
                     multiple="all",
                     relationship="many-to-many") 
  # rm(unit_cs_corrected, final_resp)
  
  resp_page_logtimes <-
    resp_pages %>%
    dplyr::left_join(unit_page_logtimes,
                     by = c("group_id", "login_name", "login_code", "booklet_id", "unit_key",
                            "unit_alias", "page_id"),
                     multiple="all")
  # rm(resp_pages)
  
  if (!is.null(students_select)) {
    resp_page_logtimes <- resp_page_logtimes[resp_page_logtimes$IDSTUD %in% students_select, ]
  }
  
  stim_logs_quant <-
    unit_page_logtimes %>%
    dplyr::anti_join(resp_page_logtimes,
                     by = c("group_id", "login_name", "login_code", "booklet_id", "unit_key", 
                            "unit_alias", "unit_start_time", "unit_n_play", "unit_time", 
                            "page_id", "page_start_time",
                            "page_logs_i", "unit_has_pages", 
                            "page_time2")) %>% # find leftover page logtimes not in 
    # resp_page_logtimes
    dplyr::semi_join(resp_page_logtimes %>% dplyr::distinct(.data$group_id, .data$login_name, 
                                                            .data$login_code,
                                                            .data$booklet_id, .data$unit_key),
                     by = c("group_id", "login_name", "login_code", "booklet_id", "unit_key")) %>% 
    # select only those unit plays which appear in resp_page_logtimes
    dplyr::mutate(variable_id = ifelse(.data$page_id == 0, "Stimulus", NA_character_)) %>%
    # dplyr::filter(.data$page_id == 0 & !is.na(.data$page_time2)) %>%
    dplyr::filter(!is.na(.data$page_time2)) %>%
    dplyr::group_by(.data$unit_key, .data$variable_id, .data$page_id) %>% 
    # item_id = .data$variable_id, .data$variable_page) %>%
    dplyr::summarise(
      page_n_valid = length(na.omit(.data$page_time2)),
      page_median = median(.data$page_time2, na.rm = TRUE) / 1000,
      page_q90 = quantile(.data$page_time2, .90, na.rm = TRUE) / 1000,
      page_q95 = quantile(.data$page_time2, .95, na.rm = TRUE) / 1000,
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      # Nur Seiten, die mindestens min_plays mal bearbeitet wurden
      .data$page_n_valid >= min_plays
    ) %>%
    dplyr::left_join(unit_domains, 
                     by="unit_key")
  
  stim_logs_quant_design <-
    unit_page_logtimes %>%
    dplyr::anti_join(resp_page_logtimes,
                     by = c("group_id", "login_name", "login_code", "booklet_id", "unit_key",
                            "unit_alias", "page_id", "unit_start_time", "unit_n_play",
                            "unit_time", "page_start_time",
                            "page_logs_i", "unit_has_pages",
                            "page_time2")) %>%
    dplyr::semi_join(resp_page_logtimes %>% 
                       dplyr::distinct(.data$group_id, .data$login_name, .data$login_code, 
                                       .data$booklet_id, .data$unit_key),
                     by = c("group_id", "login_name", "login_code", "booklet_id", "unit_key")) %>%
    dplyr::left_join(resp_page_logtimes %>% 
                       dplyr::distinct(.data$group_id, .data$login_name, .data$login_code,
                                       .data$booklet_id, .data$unit_key, .data$design),
                     by = c("group_id", "login_name", "login_code", "booklet_id", "unit_key")) %>%
    dplyr::mutate(variable_id = ifelse(.data$page_id == 0, "Stimulus", NA_character_)) %>%
    # dplyr::filter(.data$page_id == 0 & !is.na(.data$page_time2))
    dplyr::filter(!is.na(.data$page_time2))
  
  if (sum(stim_logs_quant_design$design == "FS", na.rm=TRUE) == 0) {
    for (i in 1:min_plays) {
      stim_logs_quant_design[nrow(stim_logs_quant_design) + 1,] = NA
      stim_logs_quant_design$unit_key[nrow(stim_logs_quant_design)] = "Platzhalter"
      stim_logs_quant_design$design[nrow(stim_logs_quant_design)] = "FS"
      stim_logs_quant_design$page_time2[nrow(stim_logs_quant_design)] = 0
    }}
  if (sum(stim_logs_quant_design$design == "RS", na.rm=TRUE) == 0) {
    for (i in 1:min_plays) {
      stim_logs_quant_design[nrow(stim_logs_quant_design) + 1,] = NA
      stim_logs_quant_design$unit_key[nrow(stim_logs_quant_design)] = "Platzhalter"
      stim_logs_quant_design$design[nrow(stim_logs_quant_design)] = "RS"
      stim_logs_quant_design$page_time2[nrow(stim_logs_quant_design)] = 0
    }
  }
  
  stim_logs_quant_design <-
    stim_logs_quant_design %>%
    dplyr::group_by(.data$design, .data$unit_key, .data$variable_id, .data$page_id) %>% 
    # item_id = .data$variable_id, .data$variable_page) %>%
    dplyr::summarise(
      page_n_valid = length(na.omit(.data$page_time2)),
      page_median = median(.data$page_time2, na.rm = TRUE) / 1000,
      page_q90 = quantile(.data$page_time2, .90, na.rm = TRUE) / 1000,
      page_q95 = quantile(.data$page_time2, .95, na.rm = TRUE) / 1000,
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      .data$page_n_valid >= min_plays
    ) %>%
    tidyr::pivot_wider(names_from = .data$design,
                       values_from = c(page_n_valid,
                                       page_median,
                                       page_q90,
                                       page_q95))
  
  resp_page_logtimes_page_quant <-
    resp_page_logtimes %>%
    dplyr::distinct(.data$login_code, .data$unit_key, .data$variable_id, .data$page_id, 
                    .data$page_time2) %>%
    dplyr::group_by(.data$unit_key, .data$variable_id, .data$page_id) %>%
    dplyr::summarise(
      page_n_valid = length(na.omit(.data$page_time2)),
      page_median = median(.data$page_time2, na.rm = TRUE) / 1000,
      page_q90 = quantile(.data$page_time2, .90, na.rm = TRUE) / 1000,
      page_q95 = quantile(.data$page_time2, .95, na.rm = TRUE) / 1000,
    ) %>%
    dplyr::ungroup()
  
  if (sum(resp_page_logtimes$design == "FS", na.rm=TRUE) == 0) {
    for (i in 1:min_plays) {
      resp_page_logtimes[nrow(resp_page_logtimes) + 1,] = NA
      resp_page_logtimes$unit_key[nrow(resp_page_logtimes)] = "Platzhalter"
      resp_page_logtimes$design[nrow(resp_page_logtimes)] = "FS"
      resp_page_logtimes$page_time2[nrow(resp_page_logtimes)] = 0
      resp_page_logtimes$unit_time[nrow(resp_page_logtimes)] = 0
    }}
  if (sum(resp_page_logtimes$design == "RS", na.rm=TRUE) == 0) {
    for (i in 1:min_plays) {
      resp_page_logtimes[nrow(resp_page_logtimes) + 1,] = NA
      resp_page_logtimes$unit_key[nrow(resp_page_logtimes)] = "Platzhalter"
      resp_page_logtimes$design[nrow(resp_page_logtimes)] = "RS"
      resp_page_logtimes$page_time2[nrow(resp_page_logtimes)] = 0
      resp_page_logtimes$unit_time[nrow(resp_page_logtimes)] = 0
    }
  }
  
  resp_page_logtimes_page_quant_design <-
    resp_page_logtimes %>%
    dplyr::distinct(.data$login_code, .data$design, .data$unit_key, .data$variable_id, 
                    .data$page_id, .data$page_time2) %>%
    dplyr::group_by(.data$design, .data$unit_key, .data$variable_id, .data$page_id) %>%
    dplyr::summarise(
      page_n_valid = length(na.omit(.data$page_time2)),
      page_median = median(.data$page_time2, na.rm = TRUE) / 1000,
      page_q90 = quantile(.data$page_time2, .90, na.rm = TRUE) / 1000,
      page_q95 = quantile(.data$page_time2, .95, na.rm = TRUE) / 1000,
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = .data$design,
                       values_from = c(page_n_valid,
                                       page_median, page_q90,
                                       page_q95))
  
  resp_page_logtimes_unit_quant <-
    resp_page_logtimes %>%
    dplyr::distinct(.data$login_code, .data$unit_key, .data$unit_time) %>%
    dplyr::group_by(.data$unit_key) %>%
    dplyr::summarise(
      unit_n_valid = length(na.omit(.data$unit_time)),
      unit_median = median(.data$unit_time, na.rm = TRUE) / 1000,
      unit_q90 = quantile(.data$unit_time, .90, na.rm = TRUE) / 1000,
      unit_q95 = quantile(.data$unit_time, .95, na.rm = TRUE) / 1000,
    )
  
  resp_page_logtimes_unit_quant_design <-
    resp_page_logtimes %>%
    dplyr::distinct(.data$login_code, .data$design, .data$unit_key, .data$unit_time) %>%
    dplyr::group_by(.data$design, .data$unit_key) %>%
    dplyr::summarise(
      unit_n_valid = length(na.omit(.data$unit_time)),
      unit_median = median(.data$unit_time, na.rm = TRUE) / 1000,
      unit_q90 = quantile(.data$unit_time, .90, na.rm = TRUE) / 1000,
      unit_q95 = quantile(.data$unit_time, .95, na.rm = TRUE) / 1000,
    ) %>%
    tidyr::pivot_wider(names_from = .data$design,
                       values_from = c(unit_n_valid,
                                       unit_median, unit_q90,
                                       unit_q95))
  
  # Irrelevante Units rausschmeißen:
  unit_meta <- unit_meta[which(unit_meta$unit_key %in% unit_domains$unit_key), ]
  
  unit_meta_big_cols <- c("ws_id", "unit_id", "unit_key", "unit_label",
                          "item_id", "variable_id", "Aufgabenzeit")
  
  unit_meta_big <-
    unit_meta %>%
    dplyr::select(.data$ws_id, .data$unit_id, .data$unit_key, .data$unit_label, 
                  .data$unit_metadata, .data$item_metadata) %>%
    tidyr::unnest(.data$unit_metadata) %>%
    tidyr::unnest(.data$item_metadata) %>%
    dplyr::select(dplyr::matches(stringr::str_c(unit_meta_big_cols, collapse = "|"))) %>%
    dplyr::mutate(
      # Achtung: Dieser Link sollte der kuenftige Link zum UeA-Bereich werden
      link = stringr::str_glue(
        "https://www.iqb-studio.de/#/a/{.data$ws_id}/{.data$unit_id}/preview"),
      link_legacy = stringr::str_glue(
        "https://www.iqb-studio.de/#/a/{.data$ws_id}/{.data$unit_id}/preview")
    )
  # rm(unit_meta)
  
  checkmate::assert_data_frame(unit_meta_big)
  assert_cols(unit_meta_big, unit_meta_big_cols, "unit_meta unnested")
  
  meta_logs <-
    unit_meta_big %>%
    dplyr::distinct(.data$unit_key, .data$unit_label, .data$Aufgabenzeit, .data$link) %>%
    dplyr::mutate(
      unit_estimated = lubridate::ms(.data$Aufgabenzeit) %>% lubridate::period_to_seconds()) %>%
    dplyr::select(-.data$Aufgabenzeit)
  # rm(unit_meta)
  
  resp_page_logtimes_unit_quant_meta <-
    resp_page_logtimes_unit_quant %>%
    dplyr::left_join(meta_logs,
                     by = "unit_key") %>%
    dplyr::mutate(
      unit_diff = .data$unit_q90 - .data$unit_estimated,
      unit_diff95 = .data$unit_q95 - .data$unit_estimated)
  
  resp_page_logtimes_unit_quant_meta_design <-
    resp_page_logtimes_unit_quant_design %>%
    dplyr::left_join(meta_logs,
                     by = "unit_key") %>%
    dplyr::mutate(
      unit_diff_RS = .data$unit_q90_RS - .data$unit_estimated,
      unit_diff95_RS = .data$unit_q95_RS - .data$unit_estimated,
      unit_diff_FS = .data$unit_q90_FS - .data$unit_estimated,
      unit_diff95_FS = .data$unit_q95_FS - .data$unit_estimated
    )
  
  all_quant_design <-
    dplyr::bind_rows(
      resp_page_logtimes_page_quant_design,
      stim_logs_quant_design
    ) %>%
    # dplyr::arrange(.data$unit_key, .data$variable_page, .data$item_id, .by_group=TRUE) %>%
    dplyr::left_join(resp_page_logtimes_unit_quant_meta_design,
                     by = "unit_key"
    )
  # rm(resp_page_logtimes_page_quant_design, stim_logs_quant_design, 
  # resp_page_logtimes_unit_quant_meta_design)
  
  resp_page_logtimes_page_quant <- resp_page_logtimes_page_quant %>% 
    dplyr::left_join(unit_domains, 
                     by="unit_key")
  
  all_quant <-
    dplyr::bind_rows(
      resp_page_logtimes_page_quant,
      stim_logs_quant %>% mutate(variable_id = as.character(variable_id))) %>%
    # dplyr::arrange(.data$unit_key, .data$variable_page, .data$item_id, .by_group=TRUE) %>%
    dplyr::left_join(resp_page_logtimes_unit_quant_meta,
                     by = "unit_key") %>%
    dplyr::left_join(all_quant_design,
                     by = c("unit_key", "variable_id", "page_id", "unit_label", "link", 
                            "unit_estimated")) %>%
    dplyr::left_join(unit_meta_big %>% dplyr::select(.data$unit_key, 
                                                     .data$item_id, .data$variable_id),
                     by = c("unit_key", "variable_id")) %>%
    dplyr::mutate(item_id = dplyr::case_when(
      .data$variable_id == "Stimulus" ~ "Stimulus",
      .default = .data$item_id
    )) %>%
    dplyr::arrange(.data$unit_key, .data$page_id, .data$item_id, .by_group=TRUE)
  # rm(resp_page_logtimes_page_quant, stim_logs_quant, resp_page_logtimes_unit_quant_meta, 
  # all_quant_design)
  
  dat_table <-
    all_quant %>%
    dplyr::select(
      .data$domain,
      .data$link,
      .data$unit_key,
      .data$unit_label,
      .data$unit_estimated,
      .data$unit_median,
      .data$unit_q90,
      .data$unit_diff,
      .data$unit_q95,
      .data$unit_diff95,
      .data$unit_median_RS,
      .data$unit_q90_RS,
      .data$unit_diff_RS,
      .data$unit_q95_RS,
      .data$unit_diff95_RS,
      .data$unit_median_FS,
      .data$unit_q90_FS,
      .data$unit_diff_FS,
      .data$unit_q95_FS,
      .data$unit_diff95_FS,
      .data$item_id,
      .data$page_id,
      .data$page_median,
      .data$page_q90,
      .data$page_q95,
      .data$page_median_RS,
      .data$page_q90_RS,
      .data$page_q95_RS,
      .data$page_median_FS,
      .data$page_q90_FS,
      .data$page_q95_FS
    ) %>%
    dplyr::mutate(
      item_id = ifelse(.data$item_id != "Stimulus", 
                       stringr::str_glue("{.data$unit_key}_{.data$item_id}"), .data$item_id),
      SPF = ifelse(!is.na(.data$unit_median_FS), "ja", "nein")
    )
  
  # Unit-Tabelle
  dat_table %>%
    tidyr::nest(data = -.data$domain) %>% #.$data %>% .[[1]] -> data
    dplyr::mutate(
      save = purrr::walk2(.data$data, .data$domain, function(data, domain) {
        tab <-
          data %>%
          dplyr::distinct(.data$link, dplyr::across(dplyr::starts_with("unit")), .data$SPF) %>%
          layout_staytime_tables(id = "unit-table")
        
        # Items
        tab_item <-
          data %>%
          dplyr::arrange(.data$unit_key, .data$page_id, .data$item_id, .by_group=TRUE) %>%
          dplyr::mutate(page_id = .data$page_id + 1) %>%
          # dplyr::distinct(.data$link, dplyr::across(dplyr::starts_with("unit")), .data$SPF, .data$item_id) %>%
          layout_staytime_tables(id = "item-table")
        
        save(tab, tab_item, file = stringr::str_glue(
          paste(c(output_path, "tab_{domain}.RData"), collapse="")))
        
      }, .progress = TRUE)
    )
}




#' Sets and layouts quantile tables of stay times
#'
#' Author: Philipp Franikowski, restructuring by Lea Musiolek
#'
#' @param data Pre-computed quantile tables of stay times
#' @param id String. Either "unit-table" (default) or "item-table",
#'            changes some details about the layout.
#' @param subject String. Default is "dep". Legacy parameter.
#' @param filterable Boolean. Default is TRUE. Necessary for reactable function.
#' @param searchable Boolean. Default is TRUE. Necessary for reactable function.
#' @param sortable Boolean. Default is TRUE. Necessary for reactable function.
#' @param views Boolean. Default is TRUE. Legacy parameter.
#' @param download String. Default is NULL. Download file name (legacy).
#'
#' @return Tables, including quantile dot plots, ready for use in quarto document
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Function for rendering pre-existing quantile tables of unit, page and item
#' stay times into a shape and layout suitable for a quarto document.
#' Attention! Dataset needs to be called "data" (Philipp).

layout_staytime_tables <- function(data,
                                   id = "unit-table",
                                   subject = "dep",
                                   filterable = TRUE,
                                   searchable = TRUE,
                                   sortable = TRUE,
                                   views = TRUE,
                                   download = NULL) {
  # input validation
  checkmate::assert_character(id, len = 1)
  checkmate::assert_character(subject, len = 1)
  checkmate::assert_logical(filterable, len = 1)
  checkmate::assert_logical(searchable, len = 1)
  checkmate::assert_logical(sortable, len = 1)
  checkmate::assert_logical(views, len = 1)
  checkmate::assert_character(download, len = 1, null.ok = TRUE)
  
  unit_cols <- colUnit(data)
  
  columns <- c(
    unit_cols,
    if (id == "item-table") colPage,
    colNoShow
  )
  
  # unit_cols <- colUnit(data_table)
  
  columns_filter <-
    columns %>% purrr::keep(purrr::imap_lgl(., function(x, i) i %in% names(data)))
  
  # if (subject == "dep") {
  #   diff_group <- c("itemP", "itemP_RS", "itemP_FS", "est", "se", "Geschätzte_Schwierigkeit")
  #
  #   download_columns <- c("link_legacy", "item", "Nvalid", "Nvalid_RS", "Nvalid_FS",
  #                         "itemP", "itemP_RS", "itemP_FS", "itemDiscrim", "est", "se",
  #                         "infit", "outfit", "Itemformat", "Geschätzte_Schwierigkeit", 
  #                         "Anforderungsbereich", "Bildungsstandard", "SPF", "q3_n",
  #                         "flag")
  # } else {
  #   diff_group <- c("itemP", "itemP_RS", "itemP_FS", "est__g", "est", "se__g", "se", 
  #                   "Geschätzte_Schwierigkeit")
  #
  #   download_columns <- c("link_legacy", "item", "Nvalid", "Nvalid_RS", "Nvalid_FS",
  #                         "itemP", "itemP_RS", "itemP_FS", "itemDiscrim",
  #                         "est", "se", "est__g", "se__g",
  #                         "infit", "outfit", "infit__g", "outfit__g",
  #                         "Itemformat", "Geschätzte_Schwierigkeit", "Anforderungsbereich",
  #                         "Bildungsstandard", "SPF", "q3_n",
  #                         "flag")
  # }
  
  # diff_group <- dplyr::intersect(
  #   names(data),
  #   diff_group
  # )
  
  group_item <- NULL
  if (id == "item-table") {
    group_item <-
      list(
        reactable::colGroup(name = "Item (Global)", columns = c("page_median", "page_q90", 
                                                                "page_q95")),
        reactable::colGroup(name = "Item (Regel)", columns = c("page_median_RS", "page_q90_RS", 
                                                               "page_q95_RS")),
        reactable::colGroup(name = "Item (SPF)", columns = c("page_median_FS", "page_q90_FS", 
                                                             "page_q95_FS"))
      )
  }
  
  table <-
    reactable::reactable(
      data,
      columnGroups = c(
        list(
          reactable::colGroup(name = "Unit (Global)", columns = c("unit_median", "unit_q90",
                                                                  "unit_q95", "unit_diff", 
                                                                  "unit_diff95")),
          reactable::colGroup(name = "Unit (Regel)", columns = c("unit_median_RS", "unit_q90_RS",
                                                                 "unit_q95_RS", "unit_diff_RS", 
                                                                 "unit_diff95_RS")),
          reactable::colGroup(name = "Unit (SPF)", columns = c("unit_median_FS", "unit_q90_FS",
                                                               "unit_q95_FS", "unit_diff_FS", 
                                                               "unit_diff95_FS"))
        ),
        group_item
      ),
      columns = columns_filter,
      elementId = id,
      searchable = searchable,
      filterable = filterable,
      sortable = sortable,
      showPageSizeOptions = TRUE,
      defaultPageSize = 10,
      pageSizeOptions = c(10, 50, 100, 900),
      style = list("fontFamily" = "Open Sans", width = "100%")#,
      #      language = reactable_language_settings
    )
  
  # if (views) {
  #   filter_parameters <- c("outfit",
  #                          "se",
  #                          "outfit__g",
  #                          "se__g")
  #
  #   show_parameters <- dplyr::intersect(filter_parameters, names(data))
  #
  #   filter_meta <- c("Geschätzte_Schwierigkeit",
  #                    "Anforderungsbereich",
  #                    "Bildungsstandard",
  #                    "SPF",
  #                    "innovation",
  #                    "unit_label")
  #
  #   if (subject == "dep") {
  #     filter_meta <- c(filter_meta, "innovation_unit")
  #   }
  #
  #   show_meta <- dplyr::intersect(filter_meta, names(data))
  
  filter_design <- c("unit_median_RS",
                     "unit_q90_RS",
                     "unit_q95_RS",
                     "unit_diff_RS",
                     "unit_median_FS",
                     "unit_q90_FS",
                     "unit_q95_FS",
                     "unit_diff_FS"
  )
  
  if (id == "item-table") {
    filter_design <- c(
      filter_design,
      "page_median_RS",
      "page_q90_RS",
      "page_q95_RS",
      "page_median_FS",
      "page_q90_FS",
      "page_q95_FS"
    )
  }
  
  show_design <- dplyr::intersect(filter_design, names(data))
  
  # Darstellung mit Checkboxen
  htmltools::browsable(
    htmltools::div(
      # htmltools::div(
      #   style = "display: inline-block; margin-right: 10px;",
      #   download_button(download = download,
      #                   id = id,
      #                   columns = download_columns),
      # ),
      # htmltools::div(
      #   style = "display: inline-block; margin-right: 10px;",
      #   generate_checkbox(label = "Zeige Item-Metadaten",
      #                     checked = FALSE,
      #                     id = id,
      #                     columns = show_meta),
      # ),
      # htmltools::div(
      #   style = "display: inline-block; margin-right: 10px;",
      #   generate_checkbox(label = "Zeige alle Kennwerte",
      #                     checked = FALSE,
      #                     id = id,
      #                     columns = show_parameters),
      # ),
      htmltools::div(
        style = "display: inline-block; margin-right: 10px;",
        generate_checkbox(label = "Zeige Teildesign (SPF)",
                          checked = FALSE,
                          id = id,
                          columns = show_design,
                          filter_column = list(list(id = "SPF", value = "ja"))
        ),
      ),
      table
    )
  )
}

# download_button <- function(id, columns, download) {
#   if (is.null(download)) {
#     return(NULL)
#   }
#   columns_json <- jsonlite::toJSON(columns)
#   callback <- glue::glue("Reactable.downloadDataCSV('{id}', '{download}.csv', 
#                            {{columnIds: {columns_json}, sep: ';', dec: ','}})")
#   htmltools::browsable(htmltools::tags$button(shiny::icon("download"), "Herunterladen", 
#                           onclick = callback))
# }

# # Filterfunktionen (allgemein)
# filter_multiple <- htmlwidgets::JS("function(rows, columnId, filterValue) {
#   if (typeof filterValue === 'string') {
#     // Split comma-separated values, trim spaces, and convert to lowercase
#     filterValue = filterValue.split(',').map(value => value.trim().toLowerCase());
#   }
#
#   // Proceed with filtering rows based on case-insensitive partial matches
#   return rows.filter(row => {
#     const cellValue = String(row.values[columnId]).toLowerCase();
#     return filterValue.some(filterText => cellValue.includes(filterText));
#   });
# }")

# filter_min <- htmlwidgets::JS("function(rows, columnId, filterValue) {
#         return rows.filter(function(row) {
#           return isNaN(filterValue) || row.values[columnId] >= Number(filterValue)
#         })
#       }")

# filter_input_slider <- function(id, min = NULL, max = NULL, step = .001) {
#   function(values, name) {
#     oninput <- stringr::str_glue("Reactable.setFilter('{id}', '{name}', this.value)")
#
#     min_val <- ifelse(is.null(min), min(values, na.rm = TRUE), min)
#     max_val <- ifelse(is.null(max), max(values, na.rm = TRUE), max)
#
#     htmltools::div(
#       style = htmltools::css(
#         display = "flex",
#         alignItems = "center",
#         justifyContent = "center",
#         height = "100%"
#       ),
#       htmltools::tags$input(
#         style = htmltools::css(
#           width = "90%"
#         ),
#         type = "range",
#         min = min_val,
#         max = max_val,
#         step = step,
#         value = ifelse(length(values) == 0, min, min_val),
#         oninput = oninput,
#         onchange = oninput, # For IE11 support
#         "aria-label" = stringr::str_glue("Filter by minimum {name}")
#       ),
#
#     )
#   }
# }

# # Links
# display_linkset <- function(value, index) {
#   if (!is.na(value)) {
#     link_icon_old <- shiny::icon("box-archive", lib = "font-awesome")
#     #
#     # a(
#     #   link_icon_old, target = "_blank", href = value,
#     #   style = "color: #a8a29e;",
#     #   onmouseover = "this.style.color='#d6d3d1'",
#     #   onmouseout = "this.style.color='#a8a29e'"
#     # )
#   }
# }

# display_badge <- function(data, digits = 2, na = "-") {
#   function(value, index, name) {
#     if (is.na(value)) {
#       return(na)
#     }
#
#     print_value <- printnum(value, digits = digits, gt1 = TRUE)
#
#     badge <- status_badge(color = data[[index, stringr::str_glue("color_{name}")]])
#     badge_tool <- with_tooltip(badge, data[[index, stringr::str_glue("tooltip_{name}")]])
#
#     shiny::tagList(badge_tool, print_value)
#   }
# }

# display_q3 <- function(data, id) {
#   function(value, index, name) {
#     if (length(has) > 0) {
#     } else {
#       htmltools::div(value)
#     }
#   }
# }

to_stamp <- function(x) {
  if (is.na(x)) {
    return("-")
  }
  
  sign_char <- ifelse(x < 0, "-", "")
  abs_x <- abs(x)
  
  # Convert absolute value to period
  p <- abs_x %>%
    round() %>%
    as.integer() %>%
    lubridate::seconds_to_period()
  
  # Format as MM:SS
  time_str <- sprintf("%02d:%02d", lubridate::minute(p),
                      lubridate::second(p))
  
  # Combine sign and time
  paste0(sign_char, time_str)
}

linkbox <- function(x) {
  shiny::icon("box-archive", lib = "font-awesome")
}

colUnit <- function(data) {
  list(
    link = reactable::colDef(
      name = "Links",
      width = 100,
      filterable = FALSE,
      sortable = FALSE,
      cell = linkbox
    ),
    unit_key = reactable::colDef(
      name = "Kurzname",
      style = sort_function,
      cell = function(value) htmltools::tags$code(value)
    ),
    unit_label = reactable::colDef(
      name = "Aufgabenbezeichnung",
      width = 350,
      style = sort_function
    ),
    unit_estimated = reactable::colDef(
      name = "a-priori",
      cell = to_stamp,
      style = sort_function
    ),
    unit_diff = reactable::colDef(
      name = "Differenz Q90",
      cell = to_stamp,
      style = sort_function
    ),
    unit_diff95 = reactable::colDef(
      name = "Differenz Q95",
      cell = to_stamp,
      style = sort_function
    ),
    
    # Globale Werte
    unit_median = reactable::colDef(
      name = "Median",
      cell = display_dotplot(data),
      style = sort_function,
      width = 400
    ),
    unit_q90 = reactable::colDef(
      name = "Q90",
      cell = to_stamp,
      style = sort_function
    ),
    unit_q95 = reactable::colDef(
      name = "Q95",
      cell = to_stamp,
      style = sort_function
    ),
    
    # Regelschulwerte
    unit_diff_RS = reactable::colDef(
      name = "Differenz Q90",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    ),
    unit_diff95_RS = reactable::colDef(
      name = "Differenz Q95",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    ),
    unit_median_RS = reactable::colDef(
      name = "Median",
      cell = display_dotplot(data, design = "RS"),
      style = sort_function,
      show = FALSE,
      width = 400
    ),
    unit_q90_RS = reactable::colDef(
      name = "Q90",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    ),
    unit_q95_RS = reactable::colDef(
      name = "Q95",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    ),
    
    # Förderschulwerte
    unit_diff_FS = reactable::colDef(
      name = "Differenz Q90",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    ),
    unit_diff95_FS = reactable::colDef(
      name = "Differenz Q95",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    ),
    unit_median_FS = reactable::colDef(
      name = "Median",
      cell = display_dotplot(data, design = "FS"),
      style = sort_function,
      show = FALSE,
      width = 400
    ),
    unit_q90_FS = reactable::colDef(
      name = "Q90",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    ),
    unit_q95_FS = reactable::colDef(
      name = "Q95",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    )
  )
}

display_dotplot <- function(data, design = NULL) {
  function(value, index) {
    if (is.na(value)) {
      return(value)
    }
    print_value <- to_stamp(value)
    prior <- data[[index, "unit_estimated"]]
    
    if (is.null(design)) {
      q90 <- data[[index, "unit_q90"]]
      q95 <- data[[index, "unit_q95"]]
      # tdiff <- data[[index, "unit_diff"]]
    } else {
      q90 <- data[[index, stringr::str_glue("unit_q90_{design}")]]
      q95 <- data[[index, stringr::str_glue("unit_q95_{design}")]]
      # tdiff <- data[[index, stringr::str_glue("unit_diff_{design}")]]
    }
    
    # color <- dplyr::case_when(
    #   tdiff > 0 ~ "#fb7185",
    #   tdiff < -60 ~ "#0ea5e9",
    #   .default = "#34d399")
    
    htmltools::div(
      style = list(display = "flex"),
      
      htmltools::div(print_value, style = list(flex = "0 0 40px")),
      
      eatWidget::range_chart(
        est = prior,
        est_min = q90,
        est_max = q95,
        global_est = value,
        global_est_min = value,
        global_est_max = q90,
        width = 350,
        height = 20,
        min = 0,
        max = 20 * 60,
        color_line = "#bae6fd",
        global_color_line = "#0ea5e9",
        global_color = "#0ea5e9",
        global_fill = "#0ea5e9",
        fill = "#e2e8f0"
      )
    )
  }
}

generate_checkbox <- function(label, checked = NULL, id = "item-table", columns, 
                              filter_column = NULL) {
  filter_code <- ""
  if (!is.null(filter_column)) {
    filter_code <- glue::glue("
    if (!show) {{
      Reactable.setAllFilters(id, {jsonlite::toJSON(filter_column, auto_unbox = TRUE)});
    }}else {{
      Reactable.setAllFilters(id, []);
    }}")
  }
  
  js_code <- glue::glue("((e) => {{
    const show = !e.target.checked;
    const id = '{id}';
    console.log(Reactable.getState(id));
    const cols = {jsonlite::toJSON(columns, auto_unbox = TRUE)};
    cols.map(col => Reactable.toggleHideColumn(id, col, show));
    {filter_code}
    }})(event)")
  
  htmltools::tags$input(
    label,
    type = "checkbox",
    checked = if (is.null(checked) || !checked) NULL else checked,
    onChange = htmltools::HTML(js_code)
  )
}

sort_function <- htmlwidgets::JS("function(rowInfo, column, state) {
  const {id} = column;
  const firstSorted = state.sorted[0]
  const validIds = ['domain', 'unit_key', 'unit_median', 'unit_label'];
  if (!firstSorted || validIds.includes(firstSorted.id)) {
    const prevRow = state.pageRows[rowInfo.viewIndex - 1]
    if (prevRow && rowInfo.values[id] === prevRow[id]) {
      return { visibility: 'hidden' }
    }
  }
}")

colPage <- list(
  page_id = reactable::colDef(name = "Seite"),
  page_median = reactable::colDef(name = "Median", cell = to_stamp),
  page_q90 = reactable::colDef(name = "Q90", cell = to_stamp),
  page_q95 = reactable::colDef(name = "Q95", cell = to_stamp),
  page_median_RS = reactable::colDef(name = "Median", cell = to_stamp, show = FALSE),
  page_q90_RS = reactable::colDef(name = "Q90", cell = to_stamp, show = FALSE),
  page_q95_RS = reactable::colDef(name = "Q95", cell = to_stamp, show = FALSE),
  page_median_FS = reactable::colDef(name = "Median", cell = to_stamp, show = FALSE),
  page_q90_FS = reactable::colDef(name = "Q90", cell = to_stamp, show = FALSE),
  page_q95_FS = reactable::colDef(name = "Q95", cell = to_stamp, show = FALSE),
  item_id = reactable::colDef(name = "Item", style = sort_function,
                              cell = function(value) htmltools::tags$code(value),
                              width = 120))

no_show_list <- c("SPF")

colNoShow <-
  no_show_list %>%
  purrr::map(function(x) reactable::colDef(show = FALSE)) %>%
  purrr::set_names(no_show_list)


#' Generates and saves simple unit_domain dfs
#'
#' Lea Musiolek
#'
#' @param block_file_path String. Path to "blocks.xslx" file used in generating the
#' testing booklets. Has to contain "subject", "domain" and "unit_key" columns.
#' @param out_path String. Path to directory to save unit_domain files in.
#'
#' @return Data frames, one for each subject contained in the block file.
#' Three string variables per df: subject, domain ('<fach><Kompetenz>'), unit_key. 
#' Contains each unit_key once.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
generate_unit_domains <- function(block_file_path,
                                  out_path) {
  block_df <- readxl::read_excel(block_file_path, sheet="units")
  block_df <- block_df %>%
    dplyr::select("subject", "domain", "unit_key")
  subjects <- unique(block_df$subject)
  for (subj in subjects) {
    subj_df <- block_df[which(block_df$subject==subj),]
    subj_df %>% 
      readr::write_rds(paste(c(out_path, "unit_domains_", subj, ".rds"), collapse=""))
  }
}
