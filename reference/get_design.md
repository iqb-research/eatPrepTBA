# Recovers a design from IQB Testcenter

Returns the assigned booklets and units for testtakers in a Testcenter
workspace. Supplying `units` expands the design to variables and
restricts it to units with available variables. Supply all units used in
the test. With `units = NULL`, the result remains at unit level, without
`variable_id`.

## Usage

``` r
get_design(workspace, units = NULL, overwrite = FALSE, mode = "run-hot-return")

# S4 method for class 'WorkspaceTestcenter'
get_design(workspace, units = NULL, overwrite = FALSE, mode = "run-hot-return")
```

## Arguments

- workspace:

  [WorkspaceTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceTestcenter-class.md).
  Workspace information necessary to retrieve design information.

- units:

  Tibble (optional). Unit data retrieved from
  [`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md).
  Coding schemes supply the variables for each unit. Units without a
  coding scheme use `unit_variables$variable_id` instead. If these are
  unavailable, explicit variable IDs in a StarS `unit_definition` are
  used. Retrieve definitions with
  `get_units(..., unit_definition = TRUE)`. May also contain
  `unit_codes` prepared by
  [`add_coding_scheme()`](https://iqb-research.github.io/eatPrepTBA/reference/add_coding_scheme.md)
  to save time.

- overwrite:

  Logical. Should existing `unit_codes` be rebuilt? Defaults to `FALSE`,
  which reuses codes prepared by
  [`add_coding_scheme()`](https://iqb-research.github.io/eatPrepTBA/reference/add_coding_scheme.md).

- mode:

  Character. Only testtakers with the specified modes will be filtered.
  Defaults to `"run-hot-return"`.

## Value

A tibble.

## Details

Variable selection is performed separately for each unit, supporting
mixed tests with and without coding schemes. Existing schemes supply
their base and derived variables, excluding deactivated
(`BASE_NO_VALUE`) variables. Only a missing or empty scheme (without
`variableCodings`) allows a fallback to `unit_variables`; a scheme
containing only deactivated variables does not. Cached `unit_codes` are
reused unless `overwrite = TRUE`. Empty cached results allow the
fallback only if the underlying scheme is missing or empty; otherwise
they remain empty until rebuilt with `overwrite = TRUE`. The fallback
uses the normalized IDs from
[`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md),
including its alias handling. Duplicate IDs are collapsed and missing or
blank IDs are ignored. If no usable metadata IDs remain, StarS
definitions supply explicit IDs from `interactionParameters$variableId`,
`variableInfo`, and the selection/outcome fields of
`closingMetaButtons`. Reference IDs and unrelated JSON fields are not
included. Implicit player defaults are not inferred; definitions must
explicitly name the variables needed for the design. Units without any
available variable IDs contribute no variable-level rows.

The variable list comes from metadata or definitions, not observed
responses, so it can include variables that nobody answered. For StarS
data, check that the IDs in the metadata or definitions match those
returned by
[`prepare_coded()`](https://iqb-research.github.io/eatPrepTBA/reference/prepare_coded.md).
A variable-level design alone does not supply the coding metadata
required by
[`complete_design()`](https://iqb-research.github.io/eatPrepTBA/reference/complete_design.md).

## Functions

- `get_design(WorkspaceTestcenter)`: Get design in a Testcenter
  workspace
