# Change settings of multiple units

This function updates the Studio metadata, i.e., versions for player,
editor, or schemer, and the groups and the states of multiple units. To
change a single unit, use
[`change_unit_settings()`](https://iqb-research.github.io/eatPrepTBA/reference/change_unit_settings.md).

## Usage

``` r
change_units_settings(
  workspace,
  unit_ids,
  player = NULL,
  editor = NULL,
  schemer = NULL,
  group_name = NULL,
  state = NULL
)

# S4 method for class 'WorkspaceStudio'
change_units_settings(
  workspace,
  unit_ids,
  player = NULL,
  editor = NULL,
  schemer = NULL,
  group_name = NULL,
  state = NULL
)
```

## Arguments

- workspace:

  [WorkspaceStudio](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceStudio-class.md).
  Workspace information necessary to retrieve unit information and
  resources from the API.

- unit_ids:

  IDs of the units to be changed.

- player:

  New player version.

- editor:

  New editor version.

- schemer:

  New schemer version.

- group_name:

  New group name.

- state:

  New state.

## Functions

- `change_units_settings(WorkspaceStudio)`: Get unit information and
  coding scheme in a defined workspace
