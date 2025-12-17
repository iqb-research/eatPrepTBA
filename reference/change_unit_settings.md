# Change unit settings

This function updates the Studio metadata, i.e., versions for player,
editor, or schemer, and the groups and the states of a single unit. To
change multiple units, use
[`change_units_settings()`](https://iqb-research.github.io/eatPrepTBA/reference/change_units_settings.md).
Please note that invalid inputs will result in a default version (e.g.,
the newest one in case of `editor`, `player`, or `schemer`) or be
ignored (in case of group_name or `state`).

## Usage

``` r
change_unit_settings(
  workspace,
  unit_id,
  unit_key = NULL,
  unit_name = NULL,
  description = NULL,
  player = NULL,
  editor = NULL,
  schemer = NULL,
  group_name = NULL,
  state = NULL
)

# S4 method for class 'WorkspaceStudio'
change_unit_settings(
  workspace,
  unit_id,
  unit_key = NULL,
  unit_name = NULL,
  description = NULL,
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

- unit_id:

  Integer. Unit ID to be changed.

- unit_key:

  Character. New unit key. Please note that this has to be between 3 and
  20 characters, whereas only capital and small letters of a-z, digits
  of 0-9, and underscores \_ are allowed.

- unit_name:

  Character. New unit name.

- description:

  Character. New unit description / notes.

- player:

  Character. New minor player version, format `"A.B"` (A = major
  version, B = minor version). E.g., 2.10 (major release 2, minor
  version 10). Patch version will be omitted (`"A.B.C"` becomes `"A.B"`,
  e.g., `"2.10.4"` becomes `"2.10"`).

- editor:

  Character. New minor editor version. Patch version will be omitted.

- schemer:

  Character. New minor schemer version. Patch version will be omitted.

- group_name:

  Character. New group name.

- state:

  Numeric or character. New state id (when numeric) or label (when
  character).

## Functions

- `change_unit_settings(WorkspaceStudio)`: Get unit information and
  coding scheme in a defined workspace
