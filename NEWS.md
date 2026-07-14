# eatPrepTBA 0.9.8.9025 [2026-07-14]

* Fixed `evaluate_psychometrics()` for missing category-code completion when internally generated response rows lacked Testcenter identifier columns, and made the unused-category path robust to incomplete preparation output.
* Ignored unlinked item metadata rows with missing `variable_id` when checking psychometric item-variable link uniqueness.
* Added regression coverage for psychometric summaries with categories that are present in the coding scheme but absent from observed responses.

# eatPrepTBA 0.9.8.9024 [2026-07-13]

* Updated `generate_testtakers()` to target Testcenter testtakers XML specification 18.0 by default, including the new `w3id.org` schema URL and `testtakers_version = "legacy-16"` for older Testcenter 16 output.
* Raised the default `generate_testtakers()` `app_version` to `"18.0.0"`.
* Added current testtakers XML support for `booklet_state`, `login_monitor_code`, `booklet_states_columns`, and `filter_sub_value`, and guarded against current-mode logins that mix `Booklet` and `Profile` children.
* Clarified `generate_testtakers()` documentation around the required target table format used by project-specific wrappers, and added input checks for required values, named custom texts, profile references, and consistent login definitions.
* Expanded `generate_testtakers()` documentation and examples for large project-specific `custom_texts` lists.

# eatPrepTBA 0.9.8.9023 [2026-07-08]

* Updated metadata preparation to read IQB `metadata-values` 3.0 structures, including `order`, `raw`, `asText`, language-coded text values, vocabulary `annotation`, and `order = -1` hidden profiles, while keeping legacy `isCurrent` and `valueAsText` support.

# eatPrepTBA 0.9.8.9022 [2026-07-05]

* Added a vignette showing how eatPrepTBA communicates with IQB Studio APIs through `httr2`, including browser developer tools, bearer-token headers, `GET`, `POST`, and `PATCH` examples, metadata extraction, and coding-scheme JSON inspection.
* Added anonymized example API response data and Studio screenshots used by the API vignette.

# eatPrepTBA 0.9.8.9021 [2026-07-03]

* Updated `generate_booklets()` and deprecated `generate_booklet()` to target Testcenter booklet XML specification 18.0 by default, including the new `w3id.org` schema URL.
* Reworked `configure_booklet()` to emit current active BookletConfig keys by default and added `booklet_config_version = "legacy-16"` for reproducing older Testcenter 16 configuration output.
* Kept deprecated legacy booklet-configuration arguments accepted in current mode where they can be mapped to 18.0 settings, with warnings.
* Rejected nested `TimeMax` restrictions during booklet generation because nested time constraints are not supported reliably by Testcenter.
* Added compact `times` sheet support to `prepare_booklets_from_block_design()` with `design`, `block`, `seconds`, and optional `block_group` and `leave` columns. Missing or empty `block_group` values fall back to the block name, and missing or empty `leave` values default to `"allowed"`.

# eatPrepTBA 0.9.8.9020 [2026-06-26]

* Added and refined input validation across API helpers, XML generation, stay-time table preparation, response coding inputs, and shared validation helpers.
* Kept response-coding edge cases compatible with current behavior, including ordinary data-frame inputs, missing response payload rows, and structured empty coded outputs.

# eatPrepTBA 0.9.8.9019 [2026-06-26]

* Added `unpack_response_jsons()` for auto-detecting and unpacking response JSON columns distributed across wide response tables, including matching `*_ts` timestamp columns and showing progress while JSON payloads are parsed.
* Added `prepare_unpacked_codes()` to convert code-bearing unpacked slots into the core `code_responses(..., prepare = TRUE)` output shape, including `code_type` and unnested `value` output for direct binding before `complete_design()`.
* These helpers are particularly useful for BKT-like question-slot preparation, where coded responses are stored across `question_*_content` columns rather than in one `coded` column.
* Made the `unpack_response_jsons()` progress indicator visible immediately and persistent during long JSON parsing runs.
* Added `keep_empty_rows = TRUE` as the default for `unpack_response_jsons()`, preserving one empty output row for source rows that do not produce unpacked JSON records, and renamed the payload-level empty-cell argument to `keep_empty_payloads`.
* Relaxed `code_responses()` input validation so ordinary data frames are accepted and normalised internally to tibbles.
* Extended `prepare_unpacked_codes(keep_uncoded = TRUE)` to preserve source rows that have no target response record, so identifiers such as `unit_key` survive BKT-like preparation.

# eatPrepTBA 0.9.8.9018 [2026-06-16]
* Fixed `read_booklet()` for booklet XMLs where `Unit` elements already carry `testlet_id` or `testlet_label` attributes, avoiding duplicate-column failures while preserving testlet information.
* Added regression tests for `read_booklet()` with pre-existing testlet attributes on standalone and nested units.

# eatPrepTBA 0.9.8.9017 [2026-06-11]

* Added `recode_omissions_to_not_reached` to `complete_design()` so users can choose whether trailing omission sequences at the end of a testlet are recoded as not reached.
* Kept `complete_design()` not-reached detection within each `testlet_no`.

# eatPrepTBA 0.9.8.9016 [2026-06-11]

* Added and corrected input validation across response coding, booklet/testtaker generation, metadata, settings, and psychometric helper functions.
* Removed the redundant plain-text `Author` field from `DESCRIPTION`; contributor metadata is now maintained via `Authors@R`.

# eatPrepTBA 0.9.8.9015 [2026-06-11]

* Refreshed the getting-started vignette with IQB Studio login, workspace, unit metadata, and coding-scheme walkthroughs.
* Added anonymized example unit data and Studio screenshots used by the vignette.
* Kept the shared RStudio project file tracked in the repository.

# eatPrepTBA 0.9.8.9014 [2026-06-05]

* Added shape-aware diagnostics in `download_responses()`, `get_responses()`, and `read_responses()` for changed Testcenter response slot ids. The new `diagnostics` argument controls compact, full, or suppressed feedback without changing output behavior.
* Refined response slot diagnostics to classify subform/state response containers separately from standard Testcenter wrapper slots.
* Made compact response slot diagnostics less alarming and less silent by confirming OK standard slots and pointing to `diagnostics = "full"` when id examples are shortened.
* Let `diagnostics = "none"` suppress missing-payload announcements and animated preparation progress while keeping stable preparation checkpoint messages.
* Kept elapsed-time response preparation completion messages for all response diagnostics modes.
* Added stable checkpoint messages while reading and combining multiple response files.
* Added stable checkpoint messages while checking response payload structure before response slot diagnostics are printed.
* Treated the known coded-response slot id `responses` as a special response slot, which can occur for stored coded responses such as StarS Player data, instead of warning that it is unexpected.
* Restored default response preparation progress indicators for compact and full diagnostics while keeping `diagnostics = "none"` free of animated progress.
* Aligned response report preparation and raw empty-payload announcements in `get_responses()` and `download_responses()` with the `diagnostics` modes used by `read_responses()`.

# eatPrepTBA 0.9.8.9013 [2026-06-05]

* Added broad `testthat` coverage for XML readers/generators, response and log preparation, metadata/codebook helpers, S4 workspace/login methods, mocked API wrappers, and analysis routines.
* Fixed `compute_sizes()` by assigning the intermediate dependency-size table before summarising resource sizes.
* Made codebook preparation helpers robust to single-variable and single-code JSON structures.
* Declared the `methods` dependency used by S4 class exports and constructors.
* Corrected `WorkspaceTestcenter` slot documentation.
* Reduced `R CMD check` diagnostics for startup messages, Rd files, imports, and data-masked column names.
* Added a GitHub Actions workflow for Codecov coverage uploads.

# eatPrepTBA 0.9.8.9012 [2026-06-02]

* Made `prepare_coding_scheme()` more robust for missing, partial, and mixed-type schemer payloads.
* Preserved multi-parameter rule expansion and normalized rule operators, rule positions, code models, and code identifiers to stable output types.
* Made `add_coding_scheme()` tolerate units with missing coding schemes while preserving the original unit rows.
* Kept `read_booklet()` working for both flat `Units > Unit` and nested `Units > Testlet > Unit` booklet structures.
* Added regression tests for missing coding schemes, incomplete schemer columns, multi-parameter rules, mixed rule-position types, and coded-response joins.

# eatPrepTBA 0.9.8.9011 [2026-06-01]

* Added focus lost/regained event extraction to `estimate_unit_times()`, including optional block-aware handling of automatic block switches.
* Improved unit loading summaries with failed loading counts and explicit `run_no_load` handling.
* Added warnings when block information from `full_design` cannot be joined for automatic block-switch detection.
* Added regression tests for focus-event durations, automatic block-switch handling, failed loading counts, and missing load starts.

# eatPrepTBA 0.9.8.9010 [2026-05-28]

* Fixed `add_metadata()` so item and unit metadata are matched against the workspace metadata profile when Studio returns stale `isCurrent` flags. This preserves item metadata such as `Variablenbezeichnung` even when the relevant profile is marked as not current in the returned properties JSON.
* Added regression tests for metadata profiles with stale `isCurrent` values.
* Removed a deprecated dplyr usage in `add_metadata()` that produced a lifecycle warning when deriving `unit_has_uuids`.

# eatPrepTBA 0.9.8.9009 [2026-05-13]

* Added `download_responses()` for retrieving raw response reports from the Testcenter response endpoint.
* Updated `get_responses()`, `read_responses()`, and response documentation for the current response report format.
* Added `geometry_variables` and `geometry_variables_ts` columns to `get_responses()` and `read_responses()` for Testcenter `geometryVariableCodes` payloads.
* Preserved response rows with empty nested response data so units without stored responses remain visible in `download_responses()`, `get_responses()`, and `read_responses()`.
* Preserved units whose response report payload only contains empty coded responses (`responses = []`) so they remain available for design-based missing completion.
* Fixed response report edge cases for empty API results, parsed `laststate` objects, and `units_filter_off` handling.
* Kept rows with `responses = NA` out of `code_responses()` before coding-scheme preparation so design-based missing completion in `complete_design()` remains responsible for those cases.
* Added aggregate info and warning messages for empty response payloads, skipped automatic coding rows, filtered response units, and empty response report results.
* Added regression tests for response reports with empty nested response data.

# eatPrepTBA 0.9.8.9008 [2026-05-05]

* Added `compute_staytime_tables()` for preparing stay-time quantile tables and related report output.
* Improved `estimate_unit_times()` with faster processing, failed loading counts, failed loading times, and expanded documentation.
* Restored and documented `layout_staytime_tables()`, including a pkgdown entry.
* Fixed booklet metadata parsing in `read_booklet()` so metadata with mixed text nodes no longer breaks booklet parsing. Added a regression test for this case.
* Updated package governance metadata in `DESCRIPTION`.

# eatPrepTBA 0.9.8.9007 [2026-05-04]

* Updated `login_studio()` for Studio app version `16.0.0`.
* Adjusted Studio authentication handling to read the access token from the JSON login response.
* Refreshed the generated `login_studio()` documentation.

# eatPrepTBA 0.9.8.9006 [2026-02-26]

* Fixed `get_system_checks()` for workspaces with no retrievable system-check data.

# eatPrepTBA 0.9.8.9005 [2026-02-24]

* Improved compatibility with STAR Player response data.
* Added `prepare_coded()` for preparing coded response data with list-column values.
* Renamed prepared response status output from `variable_status` to `code_status`.
* Made `get_responses()` and `get_logs()` handle successful but empty API responses more gracefully, with clearer warnings.
* Added `prepare_responses()` to the pkgdown configuration.

# eatPrepTBA 0.9.8.9004 [2026-01-14]

* Incremented the development version.
* Corrected author metadata.

# eatPrepTBA 0.9.8.9003 [2026-01-09]

* Added `test_coding_scheme()` for checking common coding-scheme problems.
* Exported and documented `test_coding_scheme()`.
* Updated startup/package helper code used by the new coding-scheme checks.

# eatPrepTBA 0.9.8.9002 [2026-01-07]

* Adjusted `complete_design()` missing-code handling so `-94` remains available for missing-by-design cases and `-93` is used for no-code cases.

# eatPrepTBA 0.9.8.9001 [2025-12-15]

* Made `get_design()` available as a top-level exported function.
* Fixed `prepare_coding_scheme()`.
* Updated Studio login and codebook documentation.
* Updated README/pkgdown links to the `iqb-research` repository location.
* Added the package logo and refreshed package site configuration.
* Updated contributor metadata.

# eatPrepTBA 0.9.8.9000 [2025-11-04]

* Added support in `change_unit_settings()` for changing unit metadata such as unit keys, names, descriptions, player/editor/schemer versions, groups, and states.
* Updated Studio login handling used by the unit-setting workflow.
* Updated display and documentation for the changed unit-setting behavior.
