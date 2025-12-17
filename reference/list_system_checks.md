# List system check files

This function returns a list of system checks that were completed in a
given workspace.

## Usage

``` r
list_system_checks(workspace)

# S4 method for class 'WorkspaceTestcenter'
list_system_checks(workspace)
```

## Arguments

- workspace:

  [WorkspaceTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceTestcenter-class.md).
  Workspace information necessary to retrieve system check file list
  from the API.

## Value

A tibble.

## Functions

- `list_system_checks(WorkspaceTestcenter)`: List all system checks in a
  given Testcenter workspace.
