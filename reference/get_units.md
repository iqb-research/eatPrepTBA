# Get multiple units with resources

This function returns the unit information for multiple units. Current
Studio Lite versions store item-variable links as
`sourceVariableId`/`sourceVariableUuid`; eatPrepTBA returns them as
`variable_id` and `variable_ref`. Older locally stored metadata with
`variableId`/`variableReadOnlyId` is still read for backward
compatibility. Retired internal fields such as `position`, `locked`, and
`weighting` are ignored. Use `item_no` for the row order in the item
list, `item_order` for the Studio/spec `order` value, and call
`get_units(..., unit_definition = TRUE)` when page locations via
`variable_pages` are needed. VariableInfo 1 and 2 records may coexist in
a workspace. Known uppercase VariableInfo 2 type and format values are
returned with their legacy spelling (for example, `NO_VALUE` becomes
`no-value` and `TEXT_SELECTION` becomes `text-selection`); unknown
values retain their spelling. Missing formats become empty strings;
omitted value lists are handled like empty lists. VariableInfo 2 has no
page field, so `variable_page` is `NA` for those records. When unit
definitions are retrieved,
[`add_coding_scheme()`](https://iqb-research.github.io/eatPrepTBA/reference/add_coding_scheme.md)
uses the extracted locations in preference to legacy page fields.

## Usage

``` r
get_units(workspace, units = NULL, metadata = TRUE, unit_definition = FALSE)

# S4 method for class 'WorkspaceStudio'
get_units(workspace, units = NULL, metadata = TRUE, unit_definition = FALSE)
```

## Arguments

- workspace:

  [Workspace](https://iqb-research.github.io/eatPrepTBA/reference/Workspace-class.md).
  Workspace information necessary to retrieve unit information and
  resources from the API.

- units:

  Tibble. Recently queried units of this workspace that should be
  updated. Please note that units that are no longer in the workspace
  will automatically be deleted by the procedure.

- metadata:

  Logical. Should the metadata be added? Defaults to `TRUE`.

- unit_definition:

  Logical. Should the unit definition be added? Defaults to `FALSE`.
  This function also infers the pages of the variables from the unit
  definition.

## Value

A tibble.

## Functions

- `get_units(WorkspaceStudio)`: Get multiple unit information and coding
  schemes in a given Studio workspace
