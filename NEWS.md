# eatPrepTBA 0.9.8.9039 [2026-10-07]

## missing classification

* Use consistent VOMD item sequence, dependency closure, and page/element
  relationships by default in eatPrepTBA (`order_method = "hybrid"`). Coding Box
  retains VOMD order. Explicit VOMD-only and structure-only methods, overrides,
  and trusted names remain available.
* Exclude physical before/after relations involving always-visible pages.
  VOMD item positions and explicit overrides remain usable for these variables;
  an unspecified always-visible flag does not exclude ordinary page metadata.

## documentation

* Make the detailed rule-set options collapsible in the standard workflow.
  Update the workflow, missing-classification, and Studio vignettes for the
  hybrid default and distinguish VOMD groups from additional order relations.

# eatPrepTBA 0.9.8.9038 [2026-10-07]

## missing classification

* Keep `complete_design()` as the single public entry point for completion,
  position annotation, and missing classification. Ordering and classification
  helpers are internal; `recode_omissions_to_not_reached = NULL` still only
  completes expected rows without calculating new positions.
* Add `missing_policy = "eatPrepTBA"` and `"coding_box"`. The latter follows the
  coding-box item-dataset rules pinned to commit `39468e5`, including numeric
  derived-result preservation, source aggregation, equal-position groups, and
  scope restrictions. Both policies retain `code_status` and response `value`.
* Use VOMD item sequence plus dependency closure by default. Optional hybrid
  and structure methods use page/element relationships; explicit overrides
  and trusted names can establish further ordering. Dense variable indices
  remain separate from evidence of presentation order.
* Let items, derived results, and required sources provide work evidence.
  eatPrepTBA gives source-supported trailing classification priority over
  derived fallback results. Valid and invalid derived results become NR only
  when every known transitive basis source is NR in the proven trailing region;
  `derived_not_reached = "preserve"` retains existing numeric results.
* Protect existing numeric NR by default. Optional
  `recode_existing_not_reached = TRUE` permits source-supported correction to
  omission. The default scope is testlet for eatPrepTBA and unit for coding-box;
  unit, testlet, and booklet scopes are available.
* Keep unlinked basis variables in the response data and deterministic index.
  Without within-unit location they supply only unit-level work evidence in
  eatPrepTBA; the box policy excludes variables outside its item/source universe
  from boundary classification.
* Preserve `code_id_input`, `code_score_input`, and `code_type_input` for
  comparisons and repeated calls. Distinguish optional `input_missings` from
  the output `missings` profile, retaining noncolliding standard input codes
  when custom output IDs are used.
* Remain in long format and do not create additional missing-by-design rows.
  Item selection affects item positions for subsequent filtering, never the
  complete missing-classification universe.

## documentation

* Rewrite the executable missing-classification vignette for both rule sets,
  input preservation, VOMD groups, optional structural ordering, and custom
  profiles. Update the standard workflow and Studio introduction to use only
  `complete_design()` as the public completion/classification API.

## bug fixes

* Retain numeric `code_id` and `code_score` columns when the autocoder omits
  these entirely for an unclassified batch, including prepared output consumed
  by `complete_design()`.
* Supplement partial manual missing-code profiles with omitted standard IDs
  in `code_responses()`. Explicit profile entries replace matching IDs without
  discarding the other standard manual-code mappings.

# eatPrepTBA 0.9.8.9037 [2026-10-06]

## new features

* Added `get_design_order()` for static variable and item positions per booklet, using unit occurrences, page/element locations, natural naming fallback, source dependencies, and optional manual basis-variable orders. Studio item mappings are the default; explicit item selections leave variable positions unchanged.
* Added independently callable `recode_missings()`. `complete_design()` uses the same ordering and classification functions; `recode_omissions_to_not_reached = NULL` now completes the design without assigning positions or changing coding fields.
* Added `response_present` to distinguish original response rows with missing coding fields from rows newly added from the design.
* Added `use_variable_names_for_recoding = FALSE`: natural variable-name order remains available for display, but names alone do not establish analytical reachability unless explicitly enabled. Known unit/page/element relations and manual overrides remain usable by default.
* Added `diagnostics = "compact"` for a current-call summary; `"full"` adds grouped details and `"none"` suppresses classification diagnostics. Newly completed rows are reported separately from changes to existing rows; repeating unchanged input reports no new changes.

## bug fixes

* Apply the same automatic rule to valid and invalid derived results: classify them as not reached when all transitive basis sources are known and demonstrably not reached. This includes zero-score fallback codes, applies whenever missing classification is enabled, and does not let derived results move the reached boundary.
* Classify not-reached values within units rather than treating every missing in the last partly worked unit as trailing. Only basis variables determine the boundary per person and testlet; derived results cannot count as evidence of later work.
* Preserve technical `code_status` values, including `NA`, and response values in all completion/classification modes. Source-supported recoding updates analytical type, ID, and score together. A mixture of omitted and not-reached basis sources is insufficient to replace a valid or invalid derived result.
* Fixed `evaluate_psychometrics()` accepting completed data with an existing `item_id` column. Domain-score item membership continues to come from `units`, independently of IDs supplied on the response table.

## migration

* `complete_design()` now includes `item_id` during classification, including an all-`NA` column when Studio links are unavailable. Remove a following `add_item_id()` call, or use its `overwrite = TRUE` argument when intentionally replacing that mapping.
* Every supplied unit occurrence is expanded to all active coding variables, including when the input design lists only selected items. Filter after missing classification. To retain an explicit `item_selection`, filter on nonmissing `item_order`, not `item_id`.
* `variable_order` is a deterministic display order, not proof of physical reachability. Review fallback orders before enabling `use_variable_names_for_recoding = TRUE`; resolve conflicts with known physical order using `order_overrides`.

## tests and documentation

* Added regression coverage for within-unit boundaries, all three modes, source trees, static ranks, repeated occurrences, overrides, item selections, coding-result preservation, and separate versus integrated calls, including the completion-to-psychometrics workflow. Updated the standard workflow and function help.

# eatPrepTBA 0.9.8.9036 [2026-09-24]

## bug fixes

* Fixed `add_metadata()` failing when the same metadata field allows single values in one workspace profile and multiple values in another. Such fields now remain list-columns across workspaces, preserving all values without expanding rows. This applies to both unit and item metadata.

## tests

* Added regression coverage for mixed workspace profiles with two and four values, preserved unit rows and profile attributes, and unchanged scalar columns for fields that allow only single values across profiles.

## documentation

* Documented how `add_metadata()` represents fields with differing multiplicity across workspace profiles.

# eatPrepTBA 0.9.8.9035 [2026-09-22]

## bug fixes

* Connected `missings_profile` to Studio's codebook export for JSON and DOCX. Profile labels are checked against the server and retain their exact spelling; unknown profiles raise an error.
* Added Studio profile missing-value codes to each variable returned by `prepare_codebook()`. User-supplied `missings` replace profile entries with the same code ID and supplement the remaining entries.
* Isolated temporary codebook downloads per preparation call and clean them up on both success and failure.

## tests

* Added regression coverage for profile lookup, case-sensitive labels, failed requests, unit-specific profile codes, user overrides, empty profiles, and temporary-file cleanup.

## documentation

* Expanded the codebook vignette with Studio profile selection, custom missing-value codes and their precedence, and JSON, DOCX, and Excel exports. Clarified login prerequisites and the limits of general instructions in the prepared table.
* Renamed the example DOCX document to use a portable filename and included it in package builds.

# eatPrepTBA 0.9.8.9034 [2026-09-17]

## new features

* Added `add_item_id()` to attach Studio item identifiers to variable-level data without changing row count or order, with explicit errors for ambiguous mappings.
* Supported item links from both `items_list` and prepared `item_metadata`, with unmatched rows returned as `NA` and explicit opt-in to replace an existing `item_id` column.

## documentation

* Added a German beginner workflow vignette separating Studio resources from Testcenter response retrieval or CSV import, explaining coding schemes and the autocoder, and demonstrating design-based not-reached completion, psychometrics, and a persons-by-items data set for eatModel.
* Linked function help pages and further coding and scaling documentation, using direct function calls and short base-pipe examples.

## tests

* Added regression coverage for item lookup, repeated and conflicting mappings, missing links, empty inputs, prepared metadata, and explicit overwriting.

# eatPrepTBA 0.9.8.9033 [2026-09-03]

## bug fixes

* Restored masked password dialogs for credential prompts, including keyring setup, by using an interactive GUI credential dialog before falling back to RStudio dialogs or console input.
* Updated `login_testcenter()` to fall back to the Testcenter 18.2+ challenge-based admin login when brute-force protection blocks the legacy direct login endpoint.

## tests

* Added regression coverage for the Testcenter challenge-login fallback, ALTCHA challenge solver, and masked credential prompts.

# eatPrepTBA 0.9.8.9032 [2026-09-02]

## bug fixes

* Fixed `read_system_checks()` so missing or empty `Responses` payloads no longer drop otherwise usable system-check rows, preserving available wide system-check values such as network metrics for `summarise_system_checks()`.
* Restored standard CSV missing-value handling for non-response columns in `read_system_checks()`.

## tests

* Added regression coverage for system-check exports with mixed valid and missing `Responses` payloads.

# eatPrepTBA 0.9.8.9031 [2026-08-31]

## bug fixes

* Fixed `estimate_unit_times()` so `CURRENT_PAGE_ID = -1` events, which can occur when the Testcenter cannot map the player-reported page to its current valid pages, are not interpreted as page 1; page summaries, anomaly detection, and unit-time outputs now expose these unmapped page states diagnostically.
* Kept `unit_has_pages = FALSE` available when a log data set contains no valid page IDs at all, preserving downstream compatibility for `compute_staytime_tables()`.
* Kept valid `CURRENT_PAGE_ID` events visible when they are also the final log entry in a damaged or aborted booklet log, and added `valid_page_id_before_running` to disambiguate page IDs observed before `PLAYER = RUNNING`.

# eatPrepTBA 0.9.8.9030 [2026-08-17]

## new features

* Added booklet-level `CustomTexts` output to `generate_booklet()` via `custom_texts` and to `generate_booklets()` via the optional `booklet_custom_texts` list-column.

# eatPrepTBA 0.9.8.9029 [2026-08-13]

## new features

* Added `estimate_audio_video_plays()` for extracting audio and video playback counts from response JSONs with robust handling for missing, malformed, nested, or non-media responses.
* Let `estimate_unit_times()` join robust `LOADCOMPLETE` environment summaries by default via `include_environment = TRUE`, while keeping sessions without `LOADCOMPLETE` quiet and diagnosable through the existing summary columns.
* Added explicit `min_page_n_valid` and `response_filter` options to `compute_staytime_tables()`, using a more inclusive default of two observed page stay times while keeping coded-response filtering as the default response-row policy.

## documentation

* Updated the log-data vignette to show the environment columns now available from `estimate_unit_times()` and to document the stay-time threshold recommendation.

# eatPrepTBA 0.9.8.9028 [2026-08-12]

## new features

* Started a shared log-analysis layer with `summarise_log_inventory()` for cheap event inventories and `summarise_log_environment()` for robust `LOADCOMPLETE` parsing of browser, OS, device, screen size, orientation, and initial load time.
* Added `detect_log_anomalies()` and `summarise_log_qc()` for structural log reliability checks, including malformed or conflicting `LOADCOMPLETE` rows, loading/running inconsistencies, connection loss, unresolved focus loss, runtime errors, timestamp problems, and page counter inconsistencies.
* Added state-specific log summaries for connections, focus, player states, page states, and response/presentation progress via `summarise_log_connections()`, `summarise_log_focus()`, `summarise_log_player()`, `summarise_log_pages()`, and `summarise_log_progress()`.
* Added optional log enrichment helpers `add_unit_sizes()`, `summarise_system_checks()`, and `add_system_check_summary()` for joining `compute_sizes()` output and summarising system-check data from `get_system_checks()` or `read_system_checks()`.

## bug fixes

* Fixed log-analysis edge cases found in review: `summarise_log_environment()` now keeps sessions without `LOADCOMPLETE`, valid `LOADCOMPLETE` JSON with empty string fields is parsed correctly, repeated focus-loss events no longer shorten loss intervals, page completeness distinguishes reaching the last page number from observing every numeric page number, and final boolean flags default to `FALSE` when no corresponding events were logged.
* Restored support for CSV-escaped `LOADCOMPLETE` payloads with doubled quotes while preserving valid empty JSON strings, and allowed log summary functions to return global summaries when no session identifier columns are available.

## documentation

* Added a `Log-Daten` vignette and pkgdown reference entries for the new log-analysis workflow.

# eatPrepTBA 0.9.8.9027 [2026-08-10]

## new features

* Added structured `validation_problems` output to `get_coding_report()` for Studio Lite `validationProblems[]` details while preserving the aggregate `validation` status.
* Added `booklet_label` to `download_units()` so Studio Lite 18.0 can fill the generated booklet `Metadata/Label`, and documented the current `<booklet-id>_testtaker.xml` filename.

## changes

* Updated Studio Lite item-metadata preparation for current IQB `unit-items` output by reading `sourceVariableId` and `sourceVariableUuid`, while keeping legacy `variableId` and `variableReadOnlyId` input compatible.
* Stopped carrying retired internal Studio item fields such as `item_position`, `item_locked`, and `item_weighting`; use `item_no` for item-list order, `item_order` for the Studio/spec `order` value, and `variable_pages` from `get_units(..., unit_definition = TRUE)` for page locations.

# eatPrepTBA 0.9.8.9026 [2026-07-15]

## new features

* Added optional Testcenter 18.0 `ViewSettings` output to `generate_testtakers()` via `view_settings`, including `theme`, `code_input`, and monitor booklet visibility settings.

## changes

* Ensured `ViewSettings` is emitted as proper nested XML below `Login` and after any `Booklet` or `Profile` children, while `legacy-16` output keeps omitting unsupported current Testcenter nodes.

## tests

* Added regression coverage for current `ViewSettings` XML, legacy omission, and invalid view-setting values.

# eatPrepTBA 0.9.8.9025 [2026-07-14]

## bug fixes

* Fixed `evaluate_psychometrics()` for missing category-code completion when internally generated response rows lacked Testcenter identifier columns, and made the unused-category path robust to incomplete preparation output.
* Ignored unlinked item metadata rows with missing `variable_id` when checking psychometric item-variable link uniqueness, and reported them separately from genuinely ambiguous links.
* Skipped unlinked item rows with missing `variable_id` when deriving item-level stay-time summaries.

## tests

* Added regression coverage for psychometric summaries with categories that are present in the coding scheme but absent from observed responses.

# eatPrepTBA 0.9.8.9024 [2026-07-13]

## new features

* Added current testtakers XML support for `booklet_state`, `login_monitor_code`, `booklet_states_columns`, and `filter_sub_value`, and guarded against current-mode logins that mix `Booklet` and `Profile` children.

## changes

* Updated `generate_testtakers()` to target Testcenter testtakers XML specification 18.0 by default, including the new `w3id.org` schema URL and `testtakers_version = "legacy-16"` for older Testcenter 16 output.
* Raised the default `generate_testtakers()` `app_version` to `"18.0.0"`.
* Clarified `generate_testtakers()` documentation around the required target table format used by project-specific wrappers, and added input checks for required values, named custom texts, profile references, and consistent login definitions.

## documentation

* Expanded `generate_testtakers()` documentation and examples for large project-specific `custom_texts` lists.

# eatPrepTBA 0.9.8.9023 [2026-07-08]

## new features

* Updated metadata preparation to read IQB `metadata-values` 3.0 structures, including `order`, `raw`, `asText`, language-coded text values, vocabulary `annotation`, and `order = -1` hidden profiles, while keeping legacy `isCurrent` and `valueAsText` support.

# eatPrepTBA 0.9.8.9022 [2026-07-05]

## documentation

* Added a vignette showing how eatPrepTBA communicates with IQB Studio APIs through `httr2`, including browser developer tools, bearer-token headers, `GET`, `POST`, and `PATCH` examples, metadata extraction, and coding-scheme JSON inspection.
* Added anonymized example API response data and Studio screenshots used by the API vignette.

# eatPrepTBA 0.9.8.9021 [2026-07-03]

## new features

* Added compact `times` sheet support to `prepare_booklets_from_block_design()` with `design`, `block`, `seconds`, and optional `block_group` and `leave` columns. Missing or empty `block_group` values fall back to the block name, and missing or empty `leave` values default to `"allowed"`.

## changes

* Updated `generate_booklets()` and deprecated `generate_booklet()` to target Testcenter booklet XML specification 18.0 by default, including the new `w3id.org` schema URL.
* Reworked `configure_booklet()` to emit current active BookletConfig keys by default and added `booklet_config_version = "legacy-16"` for reproducing older Testcenter 16 configuration output.
* Kept deprecated legacy booklet-configuration arguments accepted in current mode where they can be mapped to 18.0 settings, with warnings.
* Rejected nested `TimeMax` restrictions during booklet generation because nested time constraints are not supported reliably by Testcenter.

# eatPrepTBA 0.9.8.9020 [2026-06-26]

## changes

* Added and refined input validation across API helpers, XML generation, stay-time table preparation, response coding inputs, and shared validation helpers.
* Kept response-coding edge cases compatible with current behavior, including ordinary data-frame inputs, missing response payload rows, and structured empty coded outputs.

# eatPrepTBA 0.9.8.9019 [2026-06-26]

## new features

* Added `unpack_response_jsons()` for auto-detecting and unpacking response JSON columns distributed across wide response tables, including matching `*_ts` timestamp columns and showing progress while JSON payloads are parsed.
* Added `prepare_unpacked_codes()` to convert code-bearing unpacked slots into the core `code_responses(..., prepare = TRUE)` output shape, including `code_type` and unnested `value` output for direct binding before `complete_design()`.
* These helpers are particularly useful for BKT-like question-slot preparation, where coded responses are stored across `question_*_content` columns rather than in one `coded` column.
* Added `keep_empty_rows = TRUE` as the default for `unpack_response_jsons()`, preserving one empty output row for source rows that do not produce unpacked JSON records, and renamed the payload-level empty-cell argument to `keep_empty_payloads`.
* Extended `prepare_unpacked_codes(keep_uncoded = TRUE)` to preserve source rows that have no target response record, so identifiers such as `unit_key` survive BKT-like preparation.

## changes

* Made the `unpack_response_jsons()` progress indicator visible immediately and persistent during long JSON parsing runs.
* Relaxed `code_responses()` input validation so ordinary data frames are accepted and normalised internally to tibbles.

# eatPrepTBA 0.9.8.9018 [2026-06-16]

## bug fixes

* Fixed `read_booklet()` for booklet XMLs where `Unit` elements already carry `testlet_id` or `testlet_label` attributes, avoiding duplicate-column failures while preserving testlet information.

## tests

* Added regression tests for `read_booklet()` with pre-existing testlet attributes on standalone and nested units.

# eatPrepTBA 0.9.8.9017 [2026-06-11]

## new features

* Added `recode_omissions_to_not_reached` to `complete_design()` so users can choose whether trailing omission sequences at the end of a testlet are recoded as not reached.

## changes

* Kept `complete_design()` not-reached detection within each `testlet_no`.

# eatPrepTBA 0.9.8.9016 [2026-06-11]

## changes

* Added and corrected input validation across response coding, booklet/testtaker generation, metadata, settings, and psychometric helper functions.

## internal

* Removed the redundant plain-text `Author` field from `DESCRIPTION`; contributor metadata is now maintained via `Authors@R`.

# eatPrepTBA 0.9.8.9015 [2026-06-11]

## documentation

* Refreshed the getting-started vignette with IQB Studio login, workspace, unit metadata, and coding-scheme walkthroughs.
* Added anonymized example unit data and Studio screenshots used by the vignette.

## internal

* Kept the shared RStudio project file tracked in the repository.

# eatPrepTBA 0.9.8.9014 [2026-06-05]

## new features

* Added shape-aware diagnostics in `download_responses()`, `get_responses()`, and `read_responses()` for changed Testcenter response slot ids. The new `diagnostics` argument controls compact, full, or suppressed feedback without changing output behavior.
* Let `diagnostics = "none"` suppress missing-payload announcements and animated preparation progress while keeping stable preparation checkpoint messages.
* Added stable checkpoint messages while reading and combining multiple response files.
* Added stable checkpoint messages while checking response payload structure before response slot diagnostics are printed.

## changes

* Refined response slot diagnostics to classify subform/state response containers separately from standard Testcenter wrapper slots.
* Made compact response slot diagnostics less alarming and less silent by confirming OK standard slots and pointing to `diagnostics = "full"` when id examples are shortened.
* Kept elapsed-time response preparation completion messages for all response diagnostics modes.
* Aligned response report preparation and raw empty-payload announcements in `get_responses()` and `download_responses()` with the `diagnostics` modes used by `read_responses()`.

## bug fixes

* Treated the known coded-response slot id `responses` as a special response slot, which can occur for stored coded responses such as StarS Player data, instead of warning that it is unexpected.
* Restored default response preparation progress indicators for compact and full diagnostics while keeping `diagnostics = "none"` free of animated progress.

# eatPrepTBA 0.9.8.9013 [2026-06-05]

## bug fixes

* Fixed `compute_sizes()` by assigning the intermediate dependency-size table before summarising resource sizes.
* Made codebook preparation helpers robust to single-variable and single-code JSON structures.

## documentation

* Corrected `WorkspaceTestcenter` slot documentation.

## tests

* Added broad `testthat` coverage for XML readers/generators, response and log preparation, metadata/codebook helpers, S4 workspace/login methods, mocked API wrappers, and analysis routines.

## internal

* Declared the `methods` dependency used by S4 class exports and constructors.
* Reduced `R CMD check` diagnostics for startup messages, Rd files, imports, and data-masked column names.
* Added a GitHub Actions workflow for Codecov coverage uploads.

# eatPrepTBA 0.9.8.9012 [2026-06-02]

## bug fixes

* Made `prepare_coding_scheme()` more robust for missing, partial, and mixed-type schemer payloads.
* Preserved multi-parameter rule expansion and normalized rule operators, rule positions, code models, and code identifiers to stable output types.
* Made `add_coding_scheme()` tolerate units with missing coding schemes while preserving the original unit rows.
* Kept `read_booklet()` working for both flat `Units > Unit` and nested `Units > Testlet > Unit` booklet structures.

## tests

* Added regression tests for missing coding schemes, incomplete schemer columns, multi-parameter rules, mixed rule-position types, and coded-response joins.

# eatPrepTBA 0.9.8.9011 [2026-06-01]

## new features

* Added focus lost/regained event extraction to `estimate_unit_times()`, including optional block-aware handling of automatic block switches.
* Improved unit loading summaries with failed loading counts and explicit `run_no_load` handling.
* Added warnings when block information from `full_design` cannot be joined for automatic block-switch detection.

## tests

* Added regression tests for focus-event durations, automatic block-switch handling, failed loading counts, and missing load starts.

# eatPrepTBA 0.9.8.9010 [2026-05-28]

## bug fixes

* Fixed `add_metadata()` so item and unit metadata are matched against the workspace metadata profile when Studio returns stale `isCurrent` flags. This preserves item metadata such as `Variablenbezeichnung` even when the relevant profile is marked as not current in the returned properties JSON.

## tests

* Added regression tests for metadata profiles with stale `isCurrent` values.

## internal

* Removed a deprecated dplyr usage in `add_metadata()` that produced a lifecycle warning when deriving `unit_has_uuids`.

# eatPrepTBA 0.9.8.9009 [2026-05-13]

## new features

* Added `download_responses()` for retrieving raw response reports from the Testcenter response endpoint.
* Added `geometry_variables` and `geometry_variables_ts` columns to `get_responses()` and `read_responses()` for Testcenter `geometryVariableCodes` payloads.
* Added aggregate info and warning messages for empty response payloads, skipped automatic coding rows, filtered response units, and empty response report results.

## changes

* Updated `get_responses()`, `read_responses()`, and response documentation for the current response report format.

## bug fixes

* Preserved response rows with empty nested response data so units without stored responses remain visible in `download_responses()`, `get_responses()`, and `read_responses()`.
* Preserved units whose response report payload only contains empty coded responses (`responses = []`) so they remain available for design-based missing completion.
* Fixed response report edge cases for empty API results, parsed `laststate` objects, and `units_filter_off` handling.
* Kept rows with `responses = NA` out of `code_responses()` before coding-scheme preparation so design-based missing completion in `complete_design()` remains responsible for those cases.

## tests

* Added regression tests for response reports with empty nested response data.

# eatPrepTBA 0.9.8.9008 [2026-05-05]

## new features

* Added `compute_staytime_tables()` for preparing stay-time quantile tables and related report output.
* Restored and documented `layout_staytime_tables()`, including a pkgdown entry.

## changes

* Improved `estimate_unit_times()` with faster processing, failed loading counts, failed loading times, and expanded documentation.

## bug fixes

* Fixed booklet metadata parsing in `read_booklet()` so metadata with mixed text nodes no longer breaks booklet parsing. Added a regression test for this case.

## internal

* Updated package governance metadata in `DESCRIPTION`.

# eatPrepTBA 0.9.8.9007 [2026-05-04]

## changes

* Updated `login_studio()` for Studio app version `16.0.0`.
* Adjusted Studio authentication handling to read the access token from the JSON login response.

## documentation

* Refreshed the generated `login_studio()` documentation.

# eatPrepTBA 0.9.8.9006 [2026-02-26]

## bug fixes

* Fixed `get_system_checks()` for workspaces with no retrievable system-check data.

# eatPrepTBA 0.9.8.9005 [2026-02-24]

## new features

* Added `prepare_coded()` for preparing coded response data with list-column values.

## changes

* Improved compatibility with STAR Player response data.
* Renamed prepared response status output from `variable_status` to `code_status`.

## bug fixes

* Made `get_responses()` and `get_logs()` handle successful but empty API responses more gracefully, with clearer warnings.

## documentation

* Added `prepare_responses()` to the pkgdown configuration.

# eatPrepTBA 0.9.8.9004 [2026-01-14]

## internal

* Incremented the development version.
* Corrected author metadata.

# eatPrepTBA 0.9.8.9003 [2026-01-09]

## new features

* Added `test_coding_scheme()` for checking common coding-scheme problems.

## documentation

* Exported and documented `test_coding_scheme()`.

## internal

* Updated startup/package helper code used by the new coding-scheme checks.

# eatPrepTBA 0.9.8.9002 [2026-01-07]

## changes

* Adjusted `complete_design()` missing-code handling so `-94` remains available for missing-by-design cases and `-93` is used for no-code cases.

# eatPrepTBA 0.9.8.9001 [2025-12-15]

## new features

* Made `get_design()` available as a top-level exported function.

## bug fixes

* Fixed `prepare_coding_scheme()`.

## documentation

* Updated Studio login and codebook documentation.
* Updated README/pkgdown links to the `iqb-research` repository location.
* Added the package logo and refreshed package site configuration.

## internal

* Updated contributor metadata.

# eatPrepTBA 0.9.8.9000 [2025-11-04]

## new features

* Added support in `change_unit_settings()` for changing unit metadata such as unit keys, names, descriptions, player/editor/schemer versions, groups, and states.

## changes

* Updated Studio login handling used by the unit-setting workflow.

## documentation

* Updated display and documentation for the changed unit-setting behavior.
