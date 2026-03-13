#' Computes quantile tables of stay times
#' 
#' Author: Philipp Franikowski, restructured by Lea Musiolek
#' 
#' @param fach String. Letter denoting the school subject in question.
#' @param log_times Data frame. Log data with stay times for one subject. Result of pulling 
#' log data with eatPrepTBA::get_logs() and then using eatPrepTBA::estimate_unit_times(). 
#' If necessary, data for the subject in question needs to be selected.
#' @param unit_domains Data frame. Three string variables: subject (should equal fach), 
#' domain ('<fach><Kompetenz>'), unit_key. Should contain each relevant unit_key once.
#' Important for assigning subject and domain to each unit key down the line. Can be generated 
#' from the blocks.xlsx used for generating the tests.
#' @param final_responses Data frame. Contains the item-wise and respondent-wise responses,
#' ideally corrected for switches etc. Relevant variables: id_used, code_type, 
#' code_id, variable_source_type, booklet_id, item_id, IDSTUD, group_id, login_name, login_code,
#' unit_key, variable_page
#' @param units_cs Data frame. Unit-wise coding schemes, exported directly from IQB Studio.
#' Relevant variables: unit_key, unit_codes, variable_label, variable_page, variable_id
#' @param unit_meta Data frame. Unit-wise metadata, exported directly from IQB Studio. 
#' Relevant variables: ws_id, unit_id, unit_key, unit_label, unit_metadata, item_metadata
#' @param students_select Vector of strings. If necessary, contains the IDSTUDs of students to
#' include in the analysis. Otherwise, NULL
#'
#' @return None; saves tables, including quantile dot plots, ready for using in quarto document
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' 
#' Takes various data sources with unit, item and participant stay times and metadata,
#' and computes quantiles for use in reports. Uses the layout_staytime_tables function
#' to layout them for quarto reports.
#'
#' @export

compute_staytime_tables <- function(fach,
                                    log_times,
                                    unit_domains,
                                    final_responses,
                                    units_cs,
                                    unit_meta,
                                    students_select) {
  unit_page_logtimes <-
    log_times %>%
    unnest(unit_page_logs, keep_empty = TRUE) %>%
    dplyr::mutate(
      item_time = case_when(
        unit_has_pages ~ page_time,
        .default = unit_time
      ),
      page_id = coalesce(page_id, 0)
    )
  rm(log_times)
  
  # units_cs umformen: irrelevante Units rausschmeißen
  units_cs <- 
    units_cs[which(units_cs$unit_key %in% unit_domains$unit_key), ]
  
  # Unit-Details auspacken
  units_cs <-
    units_cs %>%
    unnest(unit_codes, keep_empty = TRUE)
  
  # Korrektur fuer die Markieritems
  units_cs_corrected <-
    units_cs %>%
    dplyr::mutate(
      variable_label = as.integer(variable_label),
      variable_page = case_when(
        !is.na(variable_label) ~ variable_label,
        .default = as.integer(variable_page)
      )
    )
  rm(units_cs)
  
  # Umformen: irrelevante Units rausschmeißen
  final_responses <- 
    final_responses[which(final_responses$unit_key %in% unit_domains$unit_key), ] %>%
    dplyr::left_join(unit_domains, by="unit_key")
  
  final_resp <-
    final_responses[
      complete.cases(final_responses[, c("id_used", "code_type", 
                                         "code_id", "variable_source_type")]) &
        final_responses$id_used == TRUE &
        final_responses$code_type != "EXAMPLE" &
        final_responses$code_type != "NO_CODING", ] %>%
    dplyr::mutate(
      design = case_when(
        str_detect(booklet_id, "S") ~ "FS",
        .default = "RS"
      ),
      booklet_id = str_to_upper(booklet_id)
    )
  rm(final_responses)
  
  resp_pages <-
    final_resp %>%
    filter(!is.na(item_id)) %>%
    dplyr::left_join(units_cs_corrected %>% dplyr::select(unit_key, variable_id, variable_page))
  rm(units_cs_corrected, final_resp)
  
  resp_page_logtimes <-
    resp_pages %>%
    dplyr::left_join(unit_page_logtimes %>% rename(variable_page = page_id))
  rm(resp_pages)
  if (!is.null(students_select)) {
    resp_page_logtimes <- resp_page_logtimes[resp_page_logtimes$IDSTUD %in% students_select, ]
  }
  
  stim_logs_quant <-
    unit_page_logtimes %>%
    rename(variable_page = page_id) %>%
    anti_join(resp_page_logtimes) %>% # select leftover page logtimes not in resp_page_logtimes, 
    # i. e. pages without item IDs
    semi_join(resp_page_logtimes %>% dplyr::distinct(group_id, login_name, login_code, 
                                              booklet_id, unit_key)) %>% # select only those use
    # combinations which appear in resp_page_logtimes
    dplyr::mutate(variable_id = ifelse(variable_page == 0, "Stimulus", NA_character_)) %>%
    filter(variable_page == 0 & !is.na(page_time)) %>%
    dplyr::group_by(unit_key, item_id = variable_id, variable_page) %>%
    dplyr::summarise(
      page_n_valid = length(na.omit(page_time)),
      page_median = median(page_time, na.rm = TRUE) / 1000,
      page_q90 = quantile(page_time, .90, na.rm = TRUE) / 1000,
      page_q95 = quantile(page_time, .95, na.rm = TRUE) / 1000,
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      # Nur Seiten, die mindestens 11 mal bearbeitet wurden
      page_n_valid > 10
    ) %>%
    dplyr::left_join(unit_domains, by="unit_key")
  
  stim_logs_quant_design <-
    unit_page_logtimes %>%
    rename(variable_page = page_id) %>%
    anti_join(resp_page_logtimes) %>%
    semi_join(resp_page_logtimes %>% dplyr::distinct(group_id, login_name, login_code, 
                                              booklet_id, unit_key)) %>%
    dplyr::left_join(resp_page_logtimes %>% dplyr::distinct(group_id, login_name, login_code,
                                              booklet_id, unit_key, design)) %>%
    dplyr::mutate(variable_id = ifelse(variable_page == 0, "Stimulus", NA_character_)) %>%
    filter(variable_page == 0 & !is.na(page_time))
  
  if (sum(stim_logs_quant_design$design == "FS", na.rm=TRUE) == 0) {
    for (i in 1:11) {
      stim_logs_quant_design[nrow(stim_logs_quant_design) + 1,] = NA
      stim_logs_quant_design$unit_key[nrow(stim_logs_quant_design)] = "0Platzhalter"
      stim_logs_quant_design$design[nrow(stim_logs_quant_design)] = "FS"
      stim_logs_quant_design$page_time[nrow(stim_logs_quant_design)] = 0
    }}
  if (sum(stim_logs_quant_design$design == "RS", na.rm=TRUE) == 0) {
    for (i in 1:11) {
      stim_logs_quant_design[nrow(stim_logs_quant_design) + 1,] = NA
      stim_logs_quant_design$unit_key[nrow(stim_logs_quant_design)] = "0Platzhalter"
      stim_logs_quant_design$design[nrow(stim_logs_quant_design)] = "RS"
      stim_logs_quant_design$page_time[nrow(stim_logs_quant_design)] = 0
    }
  }
  
  stim_logs_quant_design <- 
    stim_logs_quant_design %>%
    dplyr::group_by(design, unit_key, item_id = variable_id, variable_page) %>%
    dplyr::summarise(
      page_n_valid = length(na.omit(page_time)),
      page_median = median(page_time, na.rm = TRUE) / 1000,
      page_q90 = quantile(page_time, .90, na.rm = TRUE) / 1000,
      page_q95 = quantile(page_time, .95, na.rm = TRUE) / 1000,
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      page_n_valid > 10
    ) %>%
    tidyr::pivot_wider(names_from = design,
                       values_from = c(page_n_valid,
                                       page_median, 
                                       page_q90,
                                       page_q95))
  
  resp_page_logtimes_page_quant <-
    resp_page_logtimes %>%
    dplyr::distinct(login_code, unit_key, item_id, variable_page, page_time) %>%
    dplyr::group_by(unit_key, item_id, variable_page) %>%
    dplyr::summarise(
      page_n_valid = length(na.omit(page_time)),
      page_median = median(page_time, na.rm = TRUE) / 1000,
      page_q90 = quantile(page_time, .90, na.rm = TRUE) / 1000,
      page_q95 = quantile(page_time, .95, na.rm = TRUE) / 1000,
    ) %>%
    dplyr::ungroup()
  
  if (sum(resp_page_logtimes$design == "FS", na.rm=TRUE) == 0) {
    for (i in 1:11) {
      resp_page_logtimes[nrow(resp_page_logtimes) + 1,] = NA
      resp_page_logtimes$unit_key[nrow(resp_page_logtimes)] = "0Platzhalter"
      resp_page_logtimes$design[nrow(resp_page_logtimes)] = "FS"
      resp_page_logtimes$page_time[nrow(resp_page_logtimes)] = 0
      resp_page_logtimes$unit_time[nrow(resp_page_logtimes)] = 0
    }}
  if (sum(resp_page_logtimes$design == "RS", na.rm=TRUE) == 0) {
    for (i in 1:11) {
      resp_page_logtimes[nrow(resp_page_logtimes) + 1,] = NA
      resp_page_logtimes$unit_key[nrow(resp_page_logtimes)] = "0Platzhalter"
      resp_page_logtimes$design[nrow(resp_page_logtimes)] = "RS"
      resp_page_logtimes$page_time[nrow(resp_page_logtimes)] = 0
      resp_page_logtimes$unit_time[nrow(resp_page_logtimes)] = 0
    }
  }
  
  resp_page_logtimes_page_quant_design <-
    resp_page_logtimes %>%
    dplyr::distinct(login_code, design, unit_key, item_id, variable_page, page_time) %>%
    dplyr::group_by(design, unit_key, item_id, variable_page) %>%
    dplyr::summarise(
      page_n_valid = length(na.omit(page_time)),
      page_median = median(page_time, na.rm = TRUE) / 1000,
      page_q90 = quantile(page_time, .90, na.rm = TRUE) / 1000,
      page_q95 = quantile(page_time, .95, na.rm = TRUE) / 1000,
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = design,
                       values_from = c(page_n_valid,
                                       page_median, page_q90,
                                       page_q95))
  
  resp_page_logtimes_unit_quant <-
    resp_page_logtimes %>%
    dplyr::distinct(login_code, unit_key, unit_time) %>%
    dplyr::group_by(unit_key) %>%
    dplyr::summarise(
      unit_n_valid = length(na.omit(unit_time)),
      unit_median = median(unit_time, na.rm = TRUE) / 1000,
      unit_q90 = quantile(unit_time, .90, na.rm = TRUE) / 1000,
      unit_q95 = quantile(unit_time, .95, na.rm = TRUE) / 1000,
    )
  
  resp_page_logtimes_unit_quant_design <-
    resp_page_logtimes %>%
    dplyr::distinct(login_code, design, unit_key, unit_time) %>%
    dplyr::group_by(design, unit_key) %>%
    dplyr::summarise(
      unit_n_valid = length(na.omit(unit_time)),
      unit_median = median(unit_time, na.rm = TRUE) / 1000,
      unit_q90 = quantile(unit_time, .90, na.rm = TRUE) / 1000,
      unit_q95 = quantile(unit_time, .95, na.rm = TRUE) / 1000,
    ) %>%
    tidyr::pivot_wider(names_from = design,
                       values_from = c(unit_n_valid,
                                       unit_median, unit_q90,
                                       unit_q95))
  
  # Irrelevante Units rausschmeißen:
  unit_meta <- unit_meta[which(unit_meta$unit_key %in% unit_domains$unit_key), ]
  
  select_cols <- c("ws_id", "unit_id", "unit_key", "unit_label", 
  "item_id", "variable_id",
  "Aufgabenzeit", "Textsorte", "Wortanzahl", "Entwickler_in",
  "Itemformat", "Gesch\u00E4tzte_GeR_Niveaustufe_a_priori", "Lese_H\u00F6rstil")
  
  unit_meta <- 
    unit_meta %>% 
    dplyr::select(ws_id, unit_id, unit_key, unit_label, unit_metadata, item_metadata) %>% 
    unnest(unit_metadata) %>%
    unnest(item_metadata) %>% 
    dplyr::select(matches(str_c(select_cols, collapse = "|"))) %>% 
    dplyr::mutate(
      # Achtung: Dieser Link sollte der kuenftige Link zum UeA-Bereich werden
      link = stringr::str_glue("https://www.iqb-studio.de/#/a/{ws_id}/{unit_id}/preview"),
      link_legacy = stringr::str_glue("https://www.iqb-studio.de/#/a/{ws_id}/{unit_id}/preview")
    )
  
  meta_logs <-
    unit_meta %>%
    dplyr::distinct(unit_key, unit_label, Aufgabenzeit, link) %>%
    dplyr::mutate(
      unit_estimated = lubridate::ms(Aufgabenzeit) %>% lubridate::period_to_seconds(),
    ) %>%
    dplyr::select(-Aufgabenzeit)
  rm(unit_meta)
  
  resp_page_logtimes_unit_quant_meta <-
    resp_page_logtimes_unit_quant %>%
    dplyr::left_join(meta_logs) %>%
    dplyr::mutate(
      unit_diff = unit_q90 - unit_estimated,
      unit_diff95 = unit_q95 - unit_estimated,
    )
  
  resp_page_logtimes_unit_quant_meta_design <-
    resp_page_logtimes_unit_quant_design %>%
    dplyr::left_join(meta_logs) %>%
    dplyr::mutate(
      unit_diff_RS = unit_q90_RS - unit_estimated,
      unit_diff95_RS = unit_q95_RS - unit_estimated,
      unit_diff_FS = unit_q90_FS - unit_estimated,
      unit_diff95_FS = unit_q95_FS - unit_estimated
    )
  
  p25_all_quant_design <-
    dplyr::bind_rows(
      resp_page_logtimes_page_quant_design,
      stim_logs_quant_design
    ) %>%
    dplyr::arrange(unit_key, variable_page, item_id) %>%
    dplyr::left_join(
      resp_page_logtimes_unit_quant_meta_design
    )
  rm(resp_page_logtimes_page_quant_design, stim_logs_quant_design, resp_page_logtimes_unit_quant_meta_design)
  
  resp_page_logtimes_page_quant <- dplyr::left_join(resp_page_logtimes_page_quant, unit_domains, by="unit_key")
  
  p25_all_quant <-
    dplyr::bind_rows(
      resp_page_logtimes_page_quant,
      stim_logs_quant
    ) %>%
    dplyr::arrange(unit_key, variable_page, item_id) %>%
    dplyr::left_join(resp_page_logtimes_unit_quant_meta) %>%
    dplyr::left_join(
      p25_all_quant_design
    )
  rm(resp_page_logtimes_page_quant, stim_logs_quant, resp_page_logtimes_unit_quant_meta, p25_all_quant_design)
  
  dat_table <-
    p25_all_quant %>%
    dplyr::select(
      domain,
      link,
      unit_key,
      unit_label,
      unit_estimated,
      unit_median,
      unit_q90,
      unit_diff,
      unit_q95,
      unit_diff95,
      unit_median_RS,
      unit_q90_RS,
      unit_diff_RS,
      unit_q95_RS,
      unit_diff95_RS,
      unit_median_FS,
      unit_q90_FS,
      unit_diff_FS,
      unit_q95_FS,
      unit_diff95_FS,
      item_id,
      variable_page,
      page_median,
      page_q90,
      page_q95,
      page_median_RS,
      page_q90_RS,
      page_q95_RS,
      page_median_FS,
      page_q90_FS,
      page_q95_FS,
    ) %>%
    dplyr::mutate(
      item_id = ifelse(item_id != "Stimulus", stringr::str_glue("{unit_key}_{item_id}"), item_id),
      SPF = ifelse(!is.na(unit_median_FS), "ja", "nein")
    )
  
  # Unit-Tabelle
  dat_table %>%
    tidyr::nest(data = -domain) %>% #.$data %>% .[[1]] -> data
    dplyr::mutate(
      save = purrr::walk2(data, domain, function(data, domain) {
        tab <-
          data %>%
          dplyr::distinct(link, dplyr::across(starts_with("unit")), SPF) %>%
          layout_staytime_tables(id = "unit-table")
        
        # Items
        tab_item <-
          data %>%
          dplyr::arrange(unit_key, variable_page, item_id) %>%
          dplyr::mutate(variable_page = variable_page + 1) %>%
          # dplyr::distinct(link, dplyr::across(starts_with("unit")), SPF) %>%
          layout_staytime_tables(id = "item-table")
        
        save(tab, tab_item, file = stringr::str_glue("output/tab_{domain}.RData"))
        
      }, .progress = TRUE)
    )
  
  # dat_table %>%
  #   filter(domain == "D5") %>%
  #   # filter(unit_key %in% (1:17 %>% str_pad(width = 2, pad = "0") %>% str_c("D2_BT", .))) %>%
  #   ggplot2::ggplot(ggplot2::aes(y = as.factor(item_id) %>% fct_rev(), x = page_median)) +
  #   ggplot2::geom_linerange(ggplot2::aes(xmin = page_median, xmax = page_q90),
  #                           alpha = .9, linewidth = 1.5,
  #                           color = "deepskyblue",) +
  #   ggplot2::geom_linerange(ggplot2::aes(xmin = page_q90, xmax = page_q95),
  #                           alpha = .9, linewidth = 1.5,
  #                           color = "#bae6fd",) +
  #   ggplot2::geom_point(size = 2, shape = 21,
  #                       stroke = 1,
  #                       color = "deepskyblue",
  #                       fill = "white") +
  #   ggplot2::labs(x = "Median (IQR) Verweildauer (s)", y = "Seite") +
  #   ggplot2::scale_x_continuous(breaks = seq(0, 60 * 10, by = 60), 
  #                               limits = c(0, 60 * 10)) +
  #   # ggplot2::scale_x_continuous(breaks = seq(0, 300, by = 10), limits = c(0, 240)) +
  #   ggplot2::facet_wrap(~ unit_key, scales = "free") +
  #   ggplot2::theme_bw() +
  #   ggplot2::theme(
  #     title = ggplot2::element_text(face = "bold")
  #   )
  # 
  # ggsave("figures/D5.svg", width = 6000, height = 4000, units = "px")  
  
  # ### Extraktion fuer Janine und Pauline
  # pilot25_stim_times <-
  #   unit_page_logtimes %>%
  #   rename(variable_page = page_id) %>%
  #   anti_join(resp_page_logtimes) %>%
  #   semi_join(resp_page_logtimes %>% dplyr::distinct(group_id, login_name, login_code, 
  #                                   booklet_id, unit_key)) %>%
  #   dplyr::mutate(variable_id = ifelse(variable_page == 0, "Stimulus", NA_character_)) %>%
  #   filter(variable_page == 0 & !is.na(page_time))
  # 
  # p25_times <-
  #   resp_page_logtimes %>%
  #   dplyr::bind_rows(
  #     pilot25_stim_times
  #   ) %>%
  #   dplyr::mutate(
  #     domain = unit_key %>% stringr::str_extract(paste(c("^", fach, "[A-Z]"), collapse=""))
  #   ) %>%
  #   dplyr::select(
  #     login_name, login_code,
  #     unit_key, variable_id, variable_page,
  #     unit_time, unit_n_start, unit_has_pages,
  #     page_time, page_n_start, item_time
  #   ) %>%
  #   dplyr::arrange(
  #     unit_key, variable_page
  #   )
  # 
  # save(p25_times, file = paste(c(data_path, "p25_times_", fach, ".RData"), collapse=""))
}
