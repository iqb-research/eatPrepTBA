generate_xmls <- function(booklets_review, testtakers_review, login, tmps
                          , calculator = NULL, journal_lines = 0 # ÄNDERUNG: neu
                          ) {
  checkmate::assert_character(calculator, null.ok = TRUE)
  if (anyDuplicated(calculator)) {
    cli::cli_abort("{.arg calculator} must not contain duplicate subjects.")
  }

  # Booklets
  # booklet_config <- list(
  #   ask_for_fullscreen = "on",
  #   show_fullscreen_button = "on"
  # )
  
  # @Lara: Hier gern ändern, wenn nötig, verfügbare Einstellungen: https://github.com/franikowsp/eatPrepTBA/blob/53a6571641814330044ffea25d2bc346192ac67a/R/configure_booklet.R
  booklet_config <- list(
    paging_mode = "buttons",
    force_presentation_complete = "on",
    ask_for_fullscreen = "on",
    header_content = "none",
    navbar_unit_label = "HIDDEN", #other: INDEX, LABEL #ÄNDERUNG: war LABEL
    navbar_page_label = "hidden",
    navbar_page_controls_hidden = "true",
    toolbar_show_fullscreen_button = "true",
    toolbar_show_reload_button = "true",
    toolbar_show_unit_title = "TRUE", # TRUE/FALSE -> normalerweise TRUE
    unit_time_left_warnings = ""
   # lock_test_on_termination = "on"
  )
  
  view_settings = list(theme = "Sekundar", 
                       code_input = list(type = "text-field"))
  
  # ÄNDERUNG: alter code:
  
  # booklets_xml <- 
  #   booklets_review %>%
  #   select(
  #     booklet_id, booklet_label, booklet_description,
  #     id = unit_key, label, labelshort
  #   ) %>% 
  #   nest(
  #     units = c(id, label, labelshort)
  #   ) %>% 
  #   mutate(
  #     booklet_xml = pmap(
  #       list(booklet_id,
  #            booklet_label,
  #            booklet_description,
  #            units),
  #       function(booklet_id,
  #                booklet_label,
  #                booklet_description,
  #                units) {
  #         generate_booklet(booklet_id = booklet_id,
  #                          booklet_label = booklet_label,
  #                          booklet_description = booklet_description,
  #                          booklet_configuration = booklet_config,
  #                          units = units,
  #                          # testlets = NULL,
  #                          app_version = login@app_version,
  #                          # nach dem Fix in generate_booklets() zusätzlich noch diese Zeile mitlaufen lassen
  #                          booklet_config_version = "18.0"
  #                          ) %>% 
  #           xml2::write_xml(file = str_glue("{tmps$booklets_dir}/{booklet_id}.xml"))
  #         cli::cli_alert_success("Wrote {booklet_label} ({booklet_id})")
  #       }
  #     )
  #   ) 
  
  
  booklets_xml <- booklets_review %>%
    select(
      booklet_id,
      booklet_label,
      booklet_description,
      subject,
      id = unit_key,
      label,
      labelshort
    ) %>%
    nest(
      units = c(id, label, labelshort)
    ) %>%
    mutate(
      booklet_xml = pmap(
        list(
          booklet_id,
          booklet_label,
          booklet_description,
          subject,
          units
        ),
        function(
    booklet_id,
    booklet_label,
    booklet_description,
    subject,
    units
        ) {
          generate_booklet(
            booklet_id = booklet_id,
            booklet_label = booklet_label,
            booklet_description = booklet_description,
            booklet_configuration = booklet_config,
            units = units,
            app_version = login@app_version,
            booklet_config_version = "18.0",
            calculator = subject %in% calculator,
            journal_lines = journal_lines
          ) %>%
            xml2::write_xml(
              file = str_glue(
                "{tmps$booklets_dir}/{booklet_id}.xml"
              )
            )
          
          cli::cli_alert_success("Wrote {booklet_label} ({booklet_id})")
        }
      )
    )
  
  # Testtakers  
  testtakers_review %>% 
    select(group_id = group_id, login_name = login_name, booklet_codes = login_code, group_label = label, login_mode = mode, booklet_id = booklet) %>% 
    unnest(booklet_id) %>% 
    arrange(booklet_codes) %>% 
    generate_testtakers(login = login, view_settings = view_settings) %>% 
    xml2::write_xml(file = str_glue("{tmps$testtakers_dir}/testtakers_{config$review_date_string}.xml"))
  
  cli::cli_alert_success("Wrote testtakers")
}
