# Complete design with coded responses

This function automatically completes missings for coded responses.

## Usage

``` r
complete_design(
  coded,
  units,
  design,
  identifiers = c("group_id", "login_name", "login_code"),
  overwrite = FALSE,
  missings = NULL,
  recode_omissions_to_not_reached = FALSE,
  preserve_not_reached = FALSE
)
```

## Arguments

- coded:

  Tibble. Response data coded with
  [`code_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/code_responses.md).
  The argument `prepare` must be `TRUE`.

- units:

  Tibble. Unit data retrieved from the IQB Studio after setting the
  argument `unit_definition = TRUE` for
  [`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md)
  – otherwise the page order of variables could not be correctly
  inferred from the variable source tree. Could already be treated by
  [`add_coding_scheme()`](https://iqb-research.github.io/eatPrepTBA/reference/add_coding_scheme.md)
  to save some time.

- design:

  Tibble. Design retrieved from Testcenter via
  [`get_design()`](https://iqb-research.github.io/eatPrepTBA/reference/get_design.md)
  or an object formatted in the same way.

- identifiers:

  Character. Contains person identifiers of the dataset `coded`.
  Defaults to `c("group_id", "login_name", "login_code")` which
  corresponds to the identifiers of the IQB Testcenter.

- overwrite:

  Logical. Should column `unit_codes` be overwritten if they exist on
  `units`. Defaults to `FALSE`, i.e., `unit_codes` will be used if they
  were added to `units` beforehand by applying `add_coding_schemes()`.

- missings:

  Tibble (optional). Provide missing meta data with `code_id`,
  `code_status`, `code_score`, and `code_type`. Defaults to `NULL` and
  uses default scheme. The supplied `code_status` is always preserved,
  including `NA`; the scheme assigns analytical codes and scores only.

- recode_omissions_to_not_reached:

  Logical. Should trailing omissions (`MISSING_BY_OMISSION`) be recoded
  to not reached (`MISSING_NOT_REACHED`) when they occur at the end of a
  testlet? Defaults to `FALSE`.

- preserve_not_reached:

  Logical. Should not-reached results be protected from recoding to
  omissions, even before later work? Defaults to `FALSE`. Protects
  supplied `MISSING_NOT_REACHED` types, the missing scheme's NR code
  when `code_type` is absent, and NR inferred during completion,
  including newly completed rows with `code_status = NA`. Other supplied
  types take precedence over a technical `NOT_REACHED` status or an NR
  code.

## Value

A tibble including integer `variable_position`: the one-based position
within `unit_key` used for sorting and missing classification, including
derived variables. It is calculated from the unit metadata, replacing
any input column of the same name. Positions can have gaps when `design`
contains only part of a unit; variables absent from the unit metadata
receive `NA`.

## Details

Only variables explicitly linked to items in `items_list` (or
`item_metadata` when `items_list` is absent), and their direct and
indirect source variables, can establish the not-reached boundary. Own
scores and variable names do not establish item relevance. After the
existing code normalization, `NO_CODING` never establishes this
boundary, even for items or their sources. Missing, empty, conflicting
item metadata or unresolvable required sources cause a unit-wide
fallback: all variables in that unit may establish the boundary except
`NO_CODING`. One aggregated warning identifies affected units. An empty
item list cannot distinguish an item-free unit from missing metadata.
This selection affects only the boundary. All design rows still receive
the existing missing treatment; values, technical statuses, `id_used`,
and the full-metadata ordering retain their previous treatment. Source
evidence is evaluated at the source's own position, not the derived
item's position.

Within each unit, base variables are ordered by numeric page, numeric
section, and natural variable name (for example, `V2` precedes `V10`).
Missing sections follow known sections on the same page. Variables
without a finite page follow known pages and are ordered by name alone.
Derived variables with fully resolvable sources are inserted immediately
after their last source, including chains of derivations. Simultaneously
available derivations are ordered by natural name; their newly available
dependants are inserted before the next sibling. Missing, ambiguous, or
cyclic source links use the variable's own page, section, and name
instead. Source links come from `variable_sources`, or from
`derive_sources` when the former is empty. Element position and
derivation level are not additional sorting keys. The trailing missing
sequence is determined separately for each person, booklet, and testlet,
including positions within a partly answered unit. Existing valid,
invalid, and technical-error results retain their previous treatment;
derived results are not reclassified from their source variables. A
valid derived result included in the boundary can turn preceding
not-reached sources into omissions unless `preserve_not_reached = TRUE`.
Derived missing codes still follow the usual missing rules; no
derivation is recalculated. Newly completed rows retain `NA` in
`code_status`. The two missing options are independent: preserving NR
does not turn inner omissions into NR. With both options `TRUE`,
existing and newly inferred NR remain NR, and trailing omissions
additionally become NR. Missing schemes still assign the corresponding
codes and scores.

## See also

Online guides: [Standard
workflow](https://iqb-research.github.io/eatPrepTBA/articles/standard-workflow.html)
and [Missing
scenarios](https://iqb-research.github.io/eatPrepTBA/articles/missing-examples.html).
The online documentation may differ from the installed package version.
