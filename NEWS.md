# eatPrepTBA 0.9.8.9009

* Added broad `testthat` coverage for XML readers/generators, response and log preparation, metadata/codebook helpers, S4 workspace/login methods, mocked API wrappers, and analysis routines.
* Fixed `compute_sizes()` by assigning the intermediate dependency-size table before summarising resource sizes.
* Made codebook preparation helpers robust to single-variable and single-code JSON structures.
* Removed a redundant deprecated `dplyr::if_any()` call in `add_metadata()`.

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
