# Get system check reports

This function returns system check reports for the selected (or all)
groups.

## Usage

``` r
get_system_checks(workspace, groups = NULL, prepare = TRUE)

# S4 method for class 'WorkspaceTestcenter'
get_system_checks(workspace, groups = NULL, prepare = TRUE)
```

## Arguments

- workspace:

  [WorkspaceTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceTestcenter-class.md).
  Workspace information necessary to retrieve unit information and
  resources from the API.

- groups:

  Character. Name of the groups to be retrieved or all groups if not
  specified.

- prepare:

  Logical. Should the unit responses be prepared, i.e., should the JSON
  objects in the responses be unpacked? Defaults to `TRUE`.

## Value

A tibble.

## Functions

- `get_system_checks(WorkspaceTestcenter)`: Get responses of a given
  Testcenter workspace
