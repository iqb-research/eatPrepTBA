#' Helper function to prepare booklet configuration header
#'
#' @param booklet_config_version Booklet configuration version. `"18.0"` emits
#'   the current Testcenter booklet configuration keys. `"legacy-16"` emits the
#'   legacy key set used by older Testcenter 16 workflows.
#' @param loading_mode Loading mode.
#' @param log_policy Log policy.
#' @param browser_behaviour Browser navigation behaviour.
#' @param paging_mode Verona paging mode.
#' @param force_presentation_complete Should navigation away from incompletely
#'   presented units be prevented?
#' @param force_response_complete Should navigation away from incompletely
#'   answered units be prevented?
#' @param unit_time_left_warnings Comma-separated remaining-minute warnings.
#' @param restore_current_page_on_return Should units reopen on their last page?
#' @param lock_test_on_termination Should a terminated test be locked?
#' @param ask_for_fullscreen Should fullscreen be requested when a booklet
#'   starts?
#' @param unit_responses_buffer_time Response save interval in milliseconds.
#' @param unit_state_buffer_time Unit-state save interval in milliseconds.
#' @param test_state_buffer_time Test-state save interval in milliseconds.
#' @param header_hidden Should the header be hidden?
#' @param header_content Header title content.
#' @param navbar_unit_label Unit label style in the navigation bar.
#' @param navbar_unit_controls_hidden Should unit navigation controls be hidden?
#' @param navbar_page_label Page label style in the navigation bar.
#' @param navbar_page_controls_hidden Should page navigation controls be hidden?
#' @param navbar_backward_button Backward button behaviour in the navigation bar.
#' @param navbar_forward_button Forward button behaviour in the navigation bar.
#' @param toolbar_show_unit_title Should the toolbar show the current unit title?
#' @param toolbar_show_unit_list Should the toolbar show the unit list button?
#' @param toolbar_show_fullscreen_button Should the toolbar show the fullscreen
#'   button?
#' @param toolbar_show_reload_button Should the toolbar show the reload button?
#' @param toolbar_show_time_left Should the toolbar show remaining time?
#' @param silent_mode Should navigation and timer overlays be suppressed?
#' @param page_navibuttons,unit_navibuttons,unit_menu,force_responses_complete,controller_design,unit_screenheader,unit_title,unit_show_time_left,show_end_button_in_player,allow_player_to_terminate_test,show_fullscreen_button,show_reload_button,ui_mode Deprecated legacy arguments.
#'
#' @return A list with a valid booklet configuration.
#'
#' @keywords internal
configure_booklet <- function(
    booklet_config_version = c("18.0", "legacy-16"),
    loading_mode = c("lazy", "eager"),
    log_policy = c("rich", "disabled", "lean", "debug"),
    browser_behaviour = c("standard", "preventNav"),
    paging_mode = c("separate", "concat-scroll", "concat-scroll-snap", "buttons"),
    force_presentation_complete = c("off", "always", "on"),
    force_response_complete = c("off", "always", "on"),
    unit_time_left_warnings = "5,1",
    restore_current_page_on_return = c("off", "on"),
    lock_test_on_termination = c("off", "on"),
    ask_for_fullscreen = c("off", "on"),
    unit_responses_buffer_time = 5000,
    unit_state_buffer_time = 6000,
    test_state_buffer_time = 1000,
    header_hidden = c("false", "true"),
    header_content = c("booklet_label", "none", "block_label", "unit_label"),
    navbar_unit_label = c("index", "hidden", "label"),
    navbar_unit_controls_hidden = c("false", "true"),
    navbar_page_label = c("index", "hidden", "label", "list"),
    navbar_page_controls_hidden = c("false", "true"),
    navbar_backward_button = c("hidden", "dynamic", "units", "pages"),
    navbar_forward_button = c("hidden", "dynamic", "units", "pages"),
    toolbar_show_unit_title = c("true", "false"),
    toolbar_show_unit_list = c("false", "true"),
    toolbar_show_fullscreen_button = c("false", "true"),
    toolbar_show_reload_button = c("false", "true"),
    toolbar_show_time_left = c("false", "true"),
    silent_mode = c("false", "true"),
    page_navibuttons = NULL,
    unit_navibuttons = NULL,
    unit_menu = NULL,
    force_responses_complete = NULL,
    controller_design = NULL,
    unit_screenheader = NULL,
    unit_title = NULL,
    unit_show_time_left = NULL,
    show_end_button_in_player = NULL,
    allow_player_to_terminate_test = NULL,
    show_fullscreen_button = NULL,
    show_reload_button = NULL,
    ui_mode = NULL
) {
  booklet_config_version <- match.arg(booklet_config_version)

  if (booklet_config_version == "legacy-16") {
    return(configure_booklet_legacy16(
      loading_mode = loading_mode,
      log_policy = log_policy,
      paging_mode = paging_mode,
      page_navibuttons = page_navibuttons,
      unit_navibuttons = unit_navibuttons,
      unit_menu = unit_menu,
      force_presentation_complete = force_presentation_complete,
      force_responses_complete = force_responses_complete,
      controller_design = controller_design,
      unit_screenheader = unit_screenheader,
      unit_title = unit_title,
      unit_show_time_left = unit_show_time_left,
      unit_time_left_warnings = unit_time_left_warnings,
      show_end_button_in_player = show_end_button_in_player,
      restore_current_page_on_return = restore_current_page_on_return,
      allow_player_to_terminate_test = allow_player_to_terminate_test,
      lock_test_on_termination = lock_test_on_termination,
      ask_for_fullscreen = ask_for_fullscreen,
      show_fullscreen_button = show_fullscreen_button,
      show_reload_button = show_reload_button
    ))
  }

  migrated <- migrate_legacy_booklet_arguments(
    page_navibuttons = page_navibuttons,
    unit_navibuttons = unit_navibuttons,
    unit_menu = unit_menu,
    force_responses_complete = force_responses_complete,
    controller_design = controller_design,
    unit_screenheader = unit_screenheader,
    unit_title = unit_title,
    unit_show_time_left = unit_show_time_left,
    show_end_button_in_player = show_end_button_in_player,
    allow_player_to_terminate_test = allow_player_to_terminate_test,
    show_fullscreen_button = show_fullscreen_button,
    show_reload_button = show_reload_button,
    ui_mode = ui_mode
  )

  force_response_complete <- prefer_current_booklet_arg(
    force_response_complete,
    migrated$force_response_complete
  )
  header_hidden <- prefer_current_booklet_arg(header_hidden, migrated$header_hidden)
  header_content <- prefer_current_booklet_arg(header_content, migrated$header_content)
  navbar_unit_label <- prefer_current_booklet_arg(navbar_unit_label, migrated$navbar_unit_label)
  navbar_unit_controls_hidden <- prefer_current_booklet_arg(
    navbar_unit_controls_hidden,
    migrated$navbar_unit_controls_hidden
  )
  navbar_page_label <- prefer_current_booklet_arg(navbar_page_label, migrated$navbar_page_label)
  navbar_page_controls_hidden <- prefer_current_booklet_arg(
    navbar_page_controls_hidden,
    migrated$navbar_page_controls_hidden
  )
  toolbar_show_unit_title <- prefer_current_booklet_arg(
    toolbar_show_unit_title,
    migrated$toolbar_show_unit_title
  )
  toolbar_show_unit_list <- prefer_current_booklet_arg(
    toolbar_show_unit_list,
    migrated$toolbar_show_unit_list
  )
  toolbar_show_fullscreen_button <- prefer_current_booklet_arg(
    toolbar_show_fullscreen_button,
    migrated$toolbar_show_fullscreen_button
  )
  toolbar_show_reload_button <- prefer_current_booklet_arg(
    toolbar_show_reload_button,
    migrated$toolbar_show_reload_button
  )
  toolbar_show_time_left <- prefer_current_booklet_arg(
    toolbar_show_time_left,
    migrated$toolbar_show_time_left
  )
  silent_mode <- prefer_current_booklet_arg(silent_mode, migrated$silent_mode)

  list(
    loading_mode = upper_booklet_arg(loading_mode, c("lazy", "eager")),
    logPolicy = match_booklet_arg(log_policy, c("disabled", "lean", "rich", "debug")),
    browserBehaviour = match_booklet_arg(browser_behaviour, c("standard", "preventNav")),
    pagingMode = match_booklet_arg(
      paging_mode,
      c("separate", "concat-scroll", "concat-scroll-snap", "buttons")
    ),
    force_presentation_complete = upper_booklet_arg(
      force_presentation_complete,
      c("off", "always", "on")
    ),
    force_response_complete = upper_booklet_arg(
      force_response_complete,
      c("off", "always", "on")
    ),
    unit_time_left_warnings = format_booklet_value(unit_time_left_warnings),
    restore_current_page_on_return = upper_booklet_arg(
      restore_current_page_on_return,
      c("off", "on")
    ),
    lock_test_on_termination = upper_booklet_arg(lock_test_on_termination, c("off", "on")),
    ask_for_fullscreen = upper_booklet_arg(ask_for_fullscreen, c("off", "on")),
    unit_responses_buffer_time = format_booklet_value(unit_responses_buffer_time),
    unit_state_buffer_time = format_booklet_value(unit_state_buffer_time),
    test_state_buffer_time = format_booklet_value(test_state_buffer_time),
    header_hidden = upper_booklet_arg(header_hidden, c("false", "true")),
    header_content = upper_booklet_arg(
      header_content,
      c("none", "booklet_label", "block_label", "unit_label")
    ),
    navbar_unit_label = upper_booklet_arg(navbar_unit_label, c("hidden", "index", "label")),
    navbar_unit_controls_hidden = upper_booklet_arg(
      navbar_unit_controls_hidden,
      c("true", "false")
    ),
    navbar_page_label = upper_booklet_arg(
      navbar_page_label,
      c("hidden", "index", "label", "list")
    ),
    navbar_page_controls_hidden = upper_booklet_arg(
      navbar_page_controls_hidden,
      c("true", "false")
    ),
    navbar_backward_button = upper_booklet_arg(
      navbar_backward_button,
      c("hidden", "dynamic", "units", "pages")
    ),
    navbar_forward_button = upper_booklet_arg(
      navbar_forward_button,
      c("hidden", "dynamic", "units", "pages")
    ),
    toolbar_show_unit_title = upper_booklet_arg(toolbar_show_unit_title, c("true", "false")),
    toolbar_show_unit_list = upper_booklet_arg(toolbar_show_unit_list, c("true", "false")),
    toolbar_show_fullscreen_button = upper_booklet_arg(
      toolbar_show_fullscreen_button,
      c("true", "false")
    ),
    toolbar_show_reload_button = upper_booklet_arg(
      toolbar_show_reload_button,
      c("true", "false")
    ),
    toolbar_show_time_left = upper_booklet_arg(toolbar_show_time_left, c("true", "false")),
    silent_mode = upper_booklet_arg(silent_mode, c("true", "false"))
  ) %>%
    config_list_to_xml_ready()
}

configure_booklet_legacy16 <- function(loading_mode,
                                       log_policy,
                                       paging_mode,
                                       page_navibuttons,
                                       unit_navibuttons,
                                       unit_menu,
                                       force_presentation_complete,
                                       force_responses_complete,
                                       controller_design,
                                       unit_screenheader,
                                       unit_title,
                                       unit_show_time_left,
                                       unit_time_left_warnings,
                                       show_end_button_in_player,
                                       restore_current_page_on_return,
                                       allow_player_to_terminate_test,
                                       lock_test_on_termination,
                                       ask_for_fullscreen,
                                       show_fullscreen_button,
                                       show_reload_button) {
  list(
    loading_mode = upper_booklet_arg(loading_mode, c("lazy", "eager"), default = "lazy"),
    logPolicy = match_booklet_arg(
      log_policy,
      c("rich", "disabled", "lean", "debug"),
      default = "rich"
    ),
    pagingMode = match_booklet_arg(
      paging_mode,
      c("buttons", "separate", "concat-scroll", "concat-scroll-snap"),
      default = "buttons"
    ),
    page_navibuttons = upper_booklet_arg(
      page_navibuttons,
      c("off", "separate_bottom"),
      default = "off"
    ),
    unit_navibuttons = upper_booklet_arg(
      unit_navibuttons,
      c("full", "arrows_only", "off"),
      default = "full"
    ),
    unit_menu = upper_booklet_arg(unit_menu, c("off", "full"), default = "off"),
    force_presentation_complete = upper_booklet_arg(
      force_presentation_complete,
      c("always", "on", "off"),
      default = "always"
    ),
    force_responses_complete = upper_booklet_arg(
      force_responses_complete,
      c("off", "always", "on"),
      default = "off"
    ),
    controller_design = upper_booklet_arg(
      controller_design,
      c("2018", "2022"),
      default = "2018"
    ),
    unit_screenheader = upper_booklet_arg(
      unit_screenheader,
      c("empty", "with_unit_title", "with_block_title", "with_booklet_title", "off"),
      default = "empty"
    ),
    unit_title = upper_booklet_arg(unit_title, c("on", "off"), default = "on"),
    unit_show_time_left = upper_booklet_arg(
      unit_show_time_left,
      c("off", "on"),
      default = "off"
    ),
    unit_time_left_warnings = if (is.null(unit_time_left_warnings)) {
      "5"
    } else {
      format_booklet_value(unit_time_left_warnings)
    },
    show_end_button_in_player = upper_booklet_arg(
      show_end_button_in_player,
      c("off", "always", "on_last_unit"),
      default = "off"
    ),
    restore_current_page_on_return = upper_booklet_arg(
      restore_current_page_on_return,
      c("off", "on"),
      default = "off"
    ),
    allow_player_to_terminate_test = upper_booklet_arg(
      allow_player_to_terminate_test,
      c("on", "last_unit", "off"),
      default = "on"
    ),
    lock_test_on_termination = upper_booklet_arg(
      lock_test_on_termination,
      c("off", "on"),
      default = "off"
    ),
    ask_for_fullscreen = upper_booklet_arg(ask_for_fullscreen, c("on", "off"), default = "on"),
    show_fullscreen_button = upper_booklet_arg(
      show_fullscreen_button,
      c("on", "off"),
      default = "on"
    ),
    show_reload_button = upper_booklet_arg(show_reload_button, c("on", "off"), default = "on")
  ) %>%
    config_list_to_xml_ready()
}

migrate_legacy_booklet_arguments <- function(page_navibuttons,
                                             unit_navibuttons,
                                             unit_menu,
                                             force_responses_complete,
                                             controller_design,
                                             unit_screenheader,
                                             unit_title,
                                             unit_show_time_left,
                                             show_end_button_in_player,
                                             allow_player_to_terminate_test,
                                             show_fullscreen_button,
                                             show_reload_button,
                                             ui_mode) {
  migrated <- list()

  if (!is.null(force_responses_complete)) {
    warn_legacy_booklet_arg(
      "force_responses_complete",
      "using force_response_complete instead"
    )
    migrated$force_response_complete <- force_responses_complete
  }

  if (!is.null(unit_navibuttons)) {
    warn_legacy_booklet_arg("unit_navibuttons", "mapping to navbar_unit_* settings")
    unit_navibuttons <- upper_booklet_arg(
      unit_navibuttons,
      c("full", "arrows_only", "off")
    )
    if (unit_navibuttons == "FULL") {
      migrated$navbar_unit_label <- "INDEX"
      migrated$navbar_unit_controls_hidden <- "FALSE"
    } else if (unit_navibuttons == "ARROWS_ONLY") {
      migrated$navbar_unit_label <- "HIDDEN"
      migrated$navbar_unit_controls_hidden <- "FALSE"
    } else {
      migrated$navbar_unit_label <- "HIDDEN"
      migrated$navbar_unit_controls_hidden <- "TRUE"
    }
  }

  if (!is.null(page_navibuttons)) {
    warn_legacy_booklet_arg("page_navibuttons", "mapping to navbar_page_* settings")
    page_navibuttons <- upper_booklet_arg(
      page_navibuttons,
      c("off", "separate_bottom")
    )
    if (page_navibuttons == "OFF") {
      migrated$navbar_page_label <- "HIDDEN"
      migrated$navbar_page_controls_hidden <- "TRUE"
    } else {
      migrated$navbar_page_label <- "INDEX"
      migrated$navbar_page_controls_hidden <- "FALSE"
    }
  }

  if (!is.null(unit_menu)) {
    warn_legacy_booklet_arg("unit_menu", "mapping to toolbar_show_unit_list")
    migrated$toolbar_show_unit_list <- legacy_on_off_to_true_false(
      unit_menu,
      on = "full",
      off = "off"
    )
  }

  if (!is.null(unit_screenheader)) {
    warn_legacy_booklet_arg("unit_screenheader", "mapping to header_* settings")
    unit_screenheader <- upper_booklet_arg(
      unit_screenheader,
      c("empty", "with_unit_title", "with_block_title", "with_booklet_title", "off")
    )
    if (unit_screenheader == "OFF") {
      migrated$header_hidden <- "TRUE"
    } else if (unit_screenheader == "EMPTY") {
      migrated$header_content <- "NONE"
    } else if (unit_screenheader == "WITH_UNIT_TITLE") {
      migrated$header_content <- "UNIT_LABEL"
    } else if (unit_screenheader == "WITH_BLOCK_TITLE") {
      migrated$header_content <- "BLOCK_LABEL"
    } else {
      migrated$header_content <- "BOOKLET_LABEL"
    }
  }

  if (!is.null(unit_title)) {
    warn_legacy_booklet_arg("unit_title", "mapping to toolbar_show_unit_title")
    migrated$toolbar_show_unit_title <- legacy_on_off_to_true_false(unit_title)
  }

  if (!is.null(unit_show_time_left)) {
    warn_legacy_booklet_arg("unit_show_time_left", "mapping to toolbar_show_time_left")
    migrated$toolbar_show_time_left <- legacy_on_off_to_true_false(unit_show_time_left)
  }

  if (!is.null(show_fullscreen_button)) {
    warn_legacy_booklet_arg(
      "show_fullscreen_button",
      "mapping to toolbar_show_fullscreen_button"
    )
    migrated$toolbar_show_fullscreen_button <- legacy_on_off_to_true_false(
      show_fullscreen_button
    )
  }

  if (!is.null(show_reload_button)) {
    warn_legacy_booklet_arg("show_reload_button", "mapping to toolbar_show_reload_button")
    migrated$toolbar_show_reload_button <- legacy_on_off_to_true_false(show_reload_button)
  }

  if (!is.null(ui_mode)) {
    warn_legacy_booklet_arg("ui_mode", "mapping to silent_mode")
    ui_mode <- upper_booklet_arg(ui_mode, c("all", "none"))
    migrated$silent_mode <- if (ui_mode == "NONE") "TRUE" else "FALSE"
  }

  warn_legacy_only_booklet_arg(controller_design, "controller_design")
  warn_legacy_only_booklet_arg(show_end_button_in_player, "show_end_button_in_player")
  warn_legacy_only_booklet_arg(allow_player_to_terminate_test, "allow_player_to_terminate_test")

  migrated
}

match_booklet_arg <- function(x, choices, default = NULL) {
  if (is.null(x)) {
    x <- default
  }

  if (is.null(x)) {
    return(NULL)
  }

  if (length(x) > 1L && !is.null(default)) {
    x <- default
  }

  choice_lookup <- stats::setNames(choices, stringr::str_to_lower(choices))
  normalized <- stringr::str_to_lower(as.character(x))
  if (length(normalized) > 1L) {
    normalized <- normalized[[1]]
  }
  choice_lookup[[match.arg(normalized, names(choice_lookup))]]
}

upper_booklet_arg <- function(x, choices, default = NULL) {
  stringr::str_to_upper(match_booklet_arg(x, choices, default = default))
}

format_booklet_value <- function(x) {
  paste(as.character(x), collapse = ",")
}

config_list_to_xml_ready <- function(configuration) {
  configuration %>%
    purrr::imap(function(x, n) {
      list(
        list(x),
        key = n
      )
    }) %>%
    purrr::set_names("Config")
}

prefer_current_booklet_arg <- function(current, migrated) {
  if (is.null(migrated)) {
    current
  } else if (length(current) > 1L) {
    migrated
  } else {
    current
  }
}

legacy_on_off_to_true_false <- function(x, on = "on", off = "off") {
  x <- upper_booklet_arg(x, c(on, off))
  if (x == stringr::str_to_upper(on)) {
    "TRUE"
  } else {
    "FALSE"
  }
}

warn_legacy_booklet_arg <- function(arg, action) {
  warning(
    sprintf(
      "Booklet configuration argument `%s` is deprecated for booklet_config_version = \"18.0\"; %s.",
      arg,
      action
    ),
    call. = FALSE
  )
}

warn_legacy_only_booklet_arg <- function(value, arg) {
  if (!is.null(value)) {
    warn_legacy_booklet_arg(
      arg,
      "it has no current 18.0 equivalent and is only emitted in legacy-16 mode"
    )
  }
}
