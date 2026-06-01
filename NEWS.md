# eatPrepTBA 0.9.8.9011

* Added focus lost/regained event extraction to `estimate_unit_times()`, including optional block-aware handling of automatic block switches.
* Improved unit loading summaries with failed loading counts and explicit `run_no_load` handling.
* Added warnings when block information from `full_design` cannot be joined for automatic block-switch detection.
* Added regression tests for focus-event durations, automatic block-switch handling, failed loading counts, and missing load starts.

# eatPrepTBA 0.9.8.9010

* Fixed `add_metadata()` so item and unit metadata are matched against the workspace metadata profile when Studio returns stale `isCurrent` flags. This preserves item metadata such as `Variablenbezeichnung` even when the relevant profile is marked as not current in the returned properties JSON.
* Added regression tests for metadata profiles with stale `isCurrent` values.
* Removed a deprecated dplyr usage in `add_metadata()` that produced a lifecycle warning when deriving `unit_has_uuids`.

# eatPrepTBA 0.9.8.9009

* Added `download_responses()` for retrieving raw response reports from the Testcenter response endpoint.
* Updated `get_responses()`, `read_responses()`, and response documentation for the current response report format.
* Preserved response rows with empty nested response data so units without stored responses remain visible in `download_responses()`, `get_responses()`, and `read_responses()`.
* Preserved units whose response report payload only contains empty coded responses (`responses = []`) so they remain available for design-based missing completion.
* Fixed response report edge cases for empty API results, parsed `laststate` objects, and `units_filter_off` handling.
* Kept rows with `responses = NA` out of `code_responses()` before coding-scheme preparation so design-based missing completion in `complete_design()` remains responsible for those cases.
* Added aggregate info and warning messages for empty response payloads, skipped automatic coding rows, filtered response units, and empty response report results.
* Added regression tests for response reports with empty nested response data.

# eatPrepTBA 0.9.8.9008

* Added `compute_staytime_tables()` for preparing stay-time quantile tables and related report output.
* Improved `estimate_unit_times()` with faster processing, failed loading counts, failed loading times, and expanded documentation.
* Restored and documented `layout_staytime_tables()`, including a pkgdown entry.
* Fixed booklet metadata parsing in `read_booklet()` so metadata with mixed text nodes no longer breaks booklet parsing. Added a regression test for this case.
* Updated package governance metadata in `DESCRIPTION`.

# eatPrepTBA 0.9.8.9007

* Updated `login_studio()` for Studio app version `16.0.0`.
* Adjusted Studio authentication handling to read the access token from the JSON login response.
* Refreshed the generated `login_studio()` documentation.

# eatPrepTBA 0.9.8.9006

* Fixed `get_system_checks()` for workspaces with no retrievable system-check data.

# eatPrepTBA 0.9.8.9005

* Improved compatibility with STAR Player response data.
* Added `prepare_coded()` for preparing coded response data with list-column values.
* Renamed prepared response status output from `variable_status` to `code_status`.
* Made `get_responses()` and `get_logs()` handle successful but empty API responses more gracefully, with clearer warnings.
* Added `prepare_responses()` to the pkgdown configuration.

# eatPrepTBA 0.9.8.9004

* Incremented the development version.
* Corrected author metadata.

# eatPrepTBA 0.9.8.9003

* Added `test_coding_scheme()` for checking common coding-scheme problems.
* Exported and documented `test_coding_scheme()`.
* Updated startup/package helper code used by the new coding-scheme checks.

# eatPrepTBA 0.9.8.9002

* Adjusted `complete_design()` missing-code handling so `-94` remains available for missing-by-design cases and `-93` is used for no-code cases.

# eatPrepTBA 0.9.8.9001

* Made `get_design()` available as a top-level exported function.
* Fixed `prepare_coding_scheme()`.
* Updated Studio login and codebook documentation.
* Updated README/pkgdown links to the `iqb-research` repository location.
* Added the package logo and refreshed package site configuration.
* Updated contributor metadata.

# eatPrepTBA 0.9.8.9000

* Added support in `change_unit_settings()` for changing unit metadata such as unit keys, names, descriptions, player/editor/schemer versions, groups, and states.
* Updated Studio login handling used by the unit-setting workflow.
* Updated display and documentation for the changed unit-setting behavior.
