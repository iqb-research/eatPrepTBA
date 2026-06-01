# Estimates loading and stay times for units, unit plays and pages from log data

**\[experimental\]**

Calculates estimated processing and loading times for units and pages.
New columns and nested columns:

- unit_start_time: Timestamp of first "PLAYER = RUNNING" log in this
  unit & booklet

- unit_n_play: Number of playbacks of the unit in this session,
  incomplete playbacks included

- unit_time: Time interval at each playback of the unit between the
  player's “RUNNING” state and the next timestamp (usually the LOADING
  of the next unit, re-loading of the same unit, sometimes the end of
  the booklet), summed over playbacks

- unit_loadtime: Time interval between the player's “LOADING” and
  “RUNNING” states at each playback of the unit, including the duration
  of unsuccessful loading attempts, summed over playbacks

- unit_playbacks: Tibble containing one row with information for each
  playback of the unit

  - unit_start_i: Running number of unit playbacks

  - unit_time_i: Time interval at each playback of the unit between the
    player's “RUNNING” state and the next significant entry (usually the
    LOADING of the next unit, re-loading of the same unit, sometimes the
    end of the booklet)

  - unit_end_time_i: Timestamp of the next significant log entry
    (usually the LOADING of the next unit, re-loading of the same unit,
    sometimes the end of the booklet, i. e. the final timestamp within
    the current booklet playback) after start of current unit playback

  - unit_start_time_i: Timestamp of start of current unit playback

  - unit_loadtime_i: Time interval between the player's “LOADING” and
    “RUNNING” states at each playback of the unit, including the
    duration of unsuccessful loading attempts

  - unit_loadstart_i: Timestamp of first "PLAYER = LOADING" for current
    playback of unit

  - run_no_load_i: Player was logged as RUNNING, but not previously as
    LOADING. In this case, load times were not calculated.

- n_failed_loadings: Number of unsuccessful loading attempts for the
  unit

- unit_page_logs: Tibble containing one row with information for each
  page of the unit NULL when a unit only has one page.

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

Data grouped by group, login, booklet, and a unit identifier which
depends on use_unit_alias.

## Usage

``` r
estimate_unit_times(logs, use_unit_alias = FALSE)
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

## Value

Tibble containing various times and timestamps per unit and page
