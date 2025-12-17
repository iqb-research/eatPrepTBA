# Get a testtakers from IQB Testcenter

This function returns all testtakers with specified files.

## Usage

``` r
get_testtakers(workspace, files = NULL)

# S4 method for class 'WorkspaceTestcenter'
get_testtakers(workspace, files = NULL)
```

## Arguments

- workspace:

  [WorkspaceTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceTestcenter-class.md).
  Workspace information necessary to retrieve unit information and
  resources from the API.

- files:

  Character (optional). Names of the testtakers files to be retrieved.
  Defaults to `NULL` which returns all testtakers defined on the
  workspace.

## Value

A tibble.

## Functions

- `get_testtakers(WorkspaceTestcenter)`: Get testtakers in a Testcenter
  workspace
