# Recovers a design from IQB Testcenter

This function returns a frame of all booklets and testtakers in a given
Testcenter instance. Optionally, coding schemes can be used to add
variables that provides for a dataset to be merged to coded responses.
Please note that adding a the units object will remove the other test
parts from the design that are not reflected by the units. A remedy
would be to retrieve all units that are used in the test.

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

  Tibble (optional). Unit data retrieved from the IQB Studio after
  setting the argument `metadata = TRUE` for
  [`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md)
  – otherwise the item values could only be inferred from the variable
  source tree, i.e., item scores are taken from variable scores that are
  no source variables for other derived variables. Could optionally also
  contain `unit_codes` prepared by
  [`add_coding_scheme()`](https://iqb-research.github.io/eatPrepTBA/reference/add_coding_scheme.md)
  (saves some time).

- overwrite:

  Logical. Should column `unit_codes` be overwritten if they exist on
  `units`. Defaults to `FALSE`, i.e., `unit_codes` will be used if they
  were added to `units` beforehand by applying `add_coding_schemes()`.

- mode:

  Character. Only testtakers with the specified modes will be filtered.
  Defaults to `"run-hot-return"`.

## Value

A tibble.

## Functions

- `get_design(WorkspaceTestcenter)`: Get design in a Testcenter
  workspace
