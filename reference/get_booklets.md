# Get a booklets from IQB Testcenter

This function returns all booklets with specified files.

## Usage

``` r
get_booklets(workspace, files = NULL)

# S4 method for class 'WorkspaceTestcenter'
get_booklets(workspace, files = NULL)
```

## Arguments

- workspace:

  [WorkspaceTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceTestcenter-class.md).
  Workspace information necessary to retrieve unit information and
  resources from the API.

- files:

  Character (optional). Names of the booklet files to be retrieved.
  Defaults to `NULL` which returns all booklets defined on the
  workspace.

## Value

A tibble.

## Functions

- `get_booklets(WorkspaceTestcenter)`: Get booklets in a Testcenter
  workspace
