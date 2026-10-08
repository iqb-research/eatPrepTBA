# eatPrepTBA 0.9.8.9037 [2026-10-08]

Changes since eatPrepTBA 0.9.8.9036.

## Missing classification

* Extend `complete_design()` with variable and item positions and person-specific
  missing classification in long format. The default eatPrepTBA policy uses
  testlet scope; unit and booklet scopes are also available.
* Hybrid ordering prioritizes explicit overrides and known page/element
  positions. Compatible VOMD relationships supplement them; variable names
  supply analytical order only when explicitly trusted. Conflicting
  lower-priority relationships are discarded with a warning and exposed in
  `order_conflicts`. Item positions and derived source relationships no longer
  produce artificial ordering conflicts.
* Add `missing_policy = "coding_box"`, reproducing the Coding Box item resolver
  at commit `39468e5`. Its VOMD item-list order may differ from actual page order;
  the default scope is unit and numerical derived results are preserved.
* `recode_omissions_to_not_reached = FALSE` preserves existing omissions;
  `TRUE` also classifies trailing omissions as not reached in supported scopes;
  `NULL` only adds expected response rows. Existing numerical NR codes remain
  protected unless `recode_existing_not_reached = TRUE` is selected in eatPrepTBA.
* In eatPrepTBA, valid or invalid derived results become not reached when all
  known basis sources are demonstrably not reached in the trailing region.
  `derived_not_reached = "preserve"` retains existing numerical derived results.
  Incomplete sources and uncertain order are handled conservatively.
* Preserve technical `code_status` and response values. Add `response_present`,
  original analytical input fields, configurable missing profiles, and compact
  or detailed reports of actual changes. Repeated classification starts from
  `code_id_input`, `code_type_input`, and `code_score_input`.

## Reliability and performance

* Prepare only units referenced by the design and complete all active variables
  of their occurrences. Exclude deactivated variables (`BASE_NO_VALUE`) and,
  by default, unknown design variables with a warning. Select
  `unknown_variables = "error"` for strict validation.
* Validate response keys and prepare the static ordering before expanding to
  person-by-variable rows. Reuse unit metadata, source graphs and local orders.
* Build variable metadata in bulk, index static classification metadata instead
  of expanding its source lists over all responses, and avoid unused structural
  matrices and detailed aggregation for compact reports. Batch item ordering
  across booklets, reuse occurrence templates, and hash response/design keys
  for duplicate checks. Avoid repeated grouping of large designs and repeated
  copying when building detailed reports. Large full reports retain every
  occurrence as a plain-text detail line without costly per-line CLI formatting.
* Add optional progress reporting, enabled in interactive sessions. RGui uses
  one Windows progress window with phase, completed count and elapsed time;
  console updates are throttled. Clean up displays after completion, failure
  and interruption.
* Retain numeric code/score columns when autocoding returns an unclassified
  batch, and keep unspecified standard entries in partial manual missing
  profiles. Allow psychometric evaluation of completed data with an existing
  `item_id` while taking domain membership from `units`.

## Usage and examples

* `overwrite = TRUE` rebuilds `unit_codes` from locally stored schemes; it does
  not download data. Classification precedes item selection and already adds
  item identifiers.
* Document both missing policies, ordering priorities, source handling and
  progress reporting. Add executable comparisons with eatPrepTBA 0.9.8.9001,
  including raw versus numerical NR, differing page/item orders and derivations.
* Add regression coverage for result preservation, repeated unit occurrences,
  missing profiles, metadata exclusions, ordering conflicts and progress cleanup.
