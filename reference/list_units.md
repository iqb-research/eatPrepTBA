# List unit files

List unit files

## Usage

``` r
list_units(workspace)

# S4 method for class 'WorkspaceStudio'
list_units(workspace)

# S4 method for class 'WorkspaceTestcenter'
list_units(workspace)
```

## Arguments

- workspace:

  [Workspace](https://iqb-research.github.io/eatPrepTBA/reference/Workspace-class.md).
  Workspace information necessary to retrieve unit list from the API.

## Value

A tibble.

## Details

This function returns a list of all units in a given workspace.

This function serves as a wrapper for
[`list_files()`](https://iqb-research.github.io/eatPrepTBA/reference/list_files.md).

## Functions

- `list_units(WorkspaceStudio)`: List all units in a given IQB Studio
  workspace

- `list_units(WorkspaceTestcenter)`: List all units in a given IQB
  Testcenter workspace
