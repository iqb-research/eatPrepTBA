# Estimates loading and stay times for units, unit plays and pages from log data

**\[experimental\]**

Calculates estimated processing and loading times for units and pages.
Excludes units that never actually played. New columns and nested
columns:

- unit_start_time: Timestamp of first "PLAYER = RUNNING" log in this
  unit & booklet

- unit_n_play: Number of playbacks of the unit in this session,
  incomplete playbacks included

- n_run_no_load: Number of playbacks without previous loading log

- unit_time: Time interval at each playback of the unit between the
  player's "RUNNING" state and the next timestamp (usually the LOADING
  of the next unit, re-loading of the same unit, sometimes the end of
  the booklet), summed over playbacks

- unit_loadtime: Time interval between the player's "LOADING" and
  "RUNNING" states at each playback of the unit, including the duration
  of unsuccessful loading attempts, summed over playbacks

- unit_playbacks: Tibble containing one row with information for each
  playback of the unit

  - unit_start_i: Running number of unit playbacks

  - unit_time_i: Time interval at each playback of the unit between the
    player's "RUNNING" state and the next significant entry (usually the
    LOADING of the next unit, re-loading of the same unit, sometimes the
    end of the booklet)

  - unit_end_time_i: Timestamp of the next significant log entry
    (usually the LOADING of the next unit, re-loading of the same unit,
    sometimes the end of the booklet, i. e. the final timestamp within
    the current booklet playback) after start of current unit playback

  - unit_start_time_i: Timestamp of start of current unit playback

  - unit_loadtime_i: Time interval between the player's "LOADING" and
    "RUNNING" states at each playback of the unit, including the
    duration of unsuccessful loading attempts before playback

  - unit_loadstart_i: Timestamp of first "PLAYER = LOADING" for current
    playback of unit

  - run_no_load_i: Player was logged as RUNNING, but not previously as
    LOADING. In this case, load times were not calculated.

- n_failed_loadings: Number of failed loading attempts for the unit

- focus_events: Tibble containing all focus lost and regained events
  within each unit, based on log entries (FOCUS HAS or HAS NOT), as well
  as unit and page switches (which are considered as marking regained
  focus)

  - focus_event_ts: Timestamp of the focus event

  - focus_event_type: Type of event ("LOST" or "REGAINED", but REGAINED
    events are not returned)

  - focus_event_unfollowed: Boolean flag for lost focus events = TRUE
    when NOT followed by a regained event before another lost event
    appears

  - focus_lost_duration: For focus lost events, time until focus
    regained

- n_invalid_current_page_id_events: Number of raw negative-integer
  `CURRENT_PAGE_ID` events such as `CURRENT_PAGE_ID = -1` within the
  unit.

- delay_first_valid_page_id_ms: Time from first `PLAYER = RUNNING` to
  the first valid page ID; `0` if a valid page ID was already observed
  before the first `PLAYER = RUNNING`, and `NA` if no valid page ID is
  observed.

- valid_page_id_before_running: Boolean flag for units where a valid
  page ID was observed before the first `PLAYER = RUNNING`.

- unmapped_page_time_ms: Raw indicator for intervals after unmapped
  negative page IDs until the next valid page ID, unit load, or booklet
  end. This is not the total unassigned unit time.

- unit_page_logs: Tibble containing one row with information for each
  page of the unit. NULL when a unit only has one page.

  - page_id: Digit(s) extracted from the log entry for this page

  - page_start_time: Timestamp at which page is logged in log entry

  - page_n_play: Number of playbacks of current page

  - page_time: Time interval between CURRENT_PAGE_ID =
    [...](https://rdrr.io/r/base/dots.html) (page load completion) and
    the next page load completion, the loading of a new unit, or until
    the end of the booklet, summed over playbacks of current page

  - page_logs_i: Tibble containing one row with information for each
    playback of the page

    - page_start_i: Running number of playbacks of the page

    - page_time_i: Time interval between CURRENT_PAGE_ID =
      [...](https://rdrr.io/r/base/dots.html) (page load completion) and
      the next page load completion, the loading of a new unit, or until
      the end of the booklet

    - page_end_time_i: Timestamp of the next page load completion, the
      loading of a new unit, or the end of the booklet (i. e. the final
      timestamp within the current booklet playback)

    - page_start_time_i: Timestamp of CURRENT_PAGE_ID =
      [...](https://rdrr.io/r/base/dots.html) (page load completion)

- unit_has_pages: Boolean, marks whether there is a unit_page_logs
  tibble

- unit_ident: Either a copy of unit_alias or unit_key, depending on the
  value of use_unit_alias

- Environment columns from
  [summarise_log_environment()](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_environment.md)
  if `include_environment = TRUE`, including `browser_name`,
  `browser_version`, `os_name`, `device`, screen-size fields,
  `load_time`, and LOADCOMPLETE diagnostic columns.

Data grouped by group, login, booklet, and a unit identifier which
depends on use_unit_alias.

## Usage

``` r
estimate_unit_times(
  logs,
  use_unit_alias = FALSE,
  full_design = NULL,
  block_self_switch = FALSE,
  include_environment = TRUE
)
```

## Arguments

- logs:

  Tibble. Must be a logs tibble retrieved with
  [`get_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/get_logs.md)
  or
  [`read_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/read_logs.md).

- use_unit_alias:

  Boolean value. Determines whether to use unit_alias as unit
  identifier. If FALSE, use unit_key instead, which is the default. By
  default, unit_alias == unit_key (mapping to the Studio unit). In
  special cases — particularly for unit start pages and units that
  appear identically in multiple test booklets but are to be evaluated
  separately (which is rare; this has so far only applied to instruction
  pages or clarification questions) — a different unit_alias is
  intentionally assigned to logically identical units. In these cases,
  setting this parameter to TRUE can be advisable.

- full_design:

  Tibble. Design tibble containing block information, and all columns to
  be joined with logs. Relevant for finding lost focus events.

- block_self_switch:

  Boolean value. Were subjects able to switch to the next block
  themselves, or did they have to wait for the time to run out / the
  test conductor to switch blocks? This is relevant for finding lost
  focus events.

- include_environment:

  Boolean value. If TRUE, session-level browser, operating system,
  device, screen, and initial load-time metadata from `LOADCOMPLETE`
  logs are joined to the unit-time output via
  [summarise_log_environment()](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_environment.md).
  Sessions without `LOADCOMPLETE` are kept without warnings and receive
  missing environment fields plus diagnostic count flags.

## Value

Tibble containing various times and timestamps per unit and page
