# List testtakers files

This function serves as a wrapper for
[`list_files()`](https://iqb-research.github.io/eatPrepTBA/reference/list_files.md).

## Usage

``` r
list_testtakers(workspace)

# S4 method for class 'WorkspaceTestcenter'
list_testtakers(workspace)
```

## Arguments

- workspace:

  [WorkspaceTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceTestcenter-class.md).
  Workspace information necessary to retrieve testtakers list from the
  API.

## Value

A tibble.

## Functions

- `list_testtakers(WorkspaceTestcenter)`: List all testtakers in a given
  IQB Testcenter workspace
