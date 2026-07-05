# Helper function to prepare booklet configuration header

Helper function to prepare booklet configuration header

## Usage

``` r
configure_booklet(
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
)
```

## Arguments

- booklet_config_version:

  Booklet configuration version. `"18.0"` emits the current Testcenter
  booklet configuration keys. `"legacy-16"` emits the legacy key set
  used by older Testcenter 16 workflows.

- loading_mode:

  Loading mode.

- log_policy:

  Log policy.

- browser_behaviour:

  Browser navigation behaviour.

- paging_mode:

  Verona paging mode.

- force_presentation_complete:

  Should navigation away from incompletely presented units be prevented?

- force_response_complete:

  Should navigation away from incompletely answered units be prevented?

- unit_time_left_warnings:

  Comma-separated remaining-minute warnings.

- restore_current_page_on_return:

  Should units reopen on their last page?

- lock_test_on_termination:

  Should a terminated test be locked?

- ask_for_fullscreen:

  Should fullscreen be requested when a booklet starts?

- unit_responses_buffer_time:

  Response save interval in milliseconds.

- unit_state_buffer_time:

  Unit-state save interval in milliseconds.

- test_state_buffer_time:

  Test-state save interval in milliseconds.

- header_hidden:

  Should the header be hidden?

- header_content:

  Header title content.

- navbar_unit_label:

  Unit label style in the navigation bar.

- navbar_unit_controls_hidden:

  Should unit navigation controls be hidden?

- navbar_page_label:

  Page label style in the navigation bar.

- navbar_page_controls_hidden:

  Should page navigation controls be hidden?

- navbar_backward_button:

  Backward button behaviour in the navigation bar.

- navbar_forward_button:

  Forward button behaviour in the navigation bar.

- toolbar_show_unit_title:

  Should the toolbar show the current unit title?

- toolbar_show_unit_list:

  Should the toolbar show the unit list button?

- toolbar_show_fullscreen_button:

  Should the toolbar show the fullscreen button?

- toolbar_show_reload_button:

  Should the toolbar show the reload button?

- toolbar_show_time_left:

  Should the toolbar show remaining time?

- silent_mode:

  Should navigation and timer overlays be suppressed?

- page_navibuttons, unit_navibuttons, unit_menu,
  force_responses_complete, controller_design, unit_screenheader,
  unit_title, unit_show_time_left, show_end_button_in_player,
  allow_player_to_terminate_test, show_fullscreen_button,
  show_reload_button, ui_mode:

  Deprecated legacy arguments.

## Value

A list with a valid booklet configuration.
