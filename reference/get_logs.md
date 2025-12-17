# Get logs

This function returns logs from the IQB Testcenter

## Usage

``` r
get_logs(workspace, groups = NULL)

# S4 method for class 'WorkspaceTestcenter'
get_logs(workspace, groups = NULL)
```

## Arguments

- workspace:

  [WorkspaceTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceTestcenter-class.md).
  Workspace information necessary to retrieve unit information and
  resources from the API.

- groups:

  Character. Name of the groups to be retrieved or all groups if not
  specified. Please note, that this has to be specified in all-capital
  letters.

## Value

A tibble.

## Functions

- `get_logs(WorkspaceTestcenter)`: Get responses of a given Testcenter
  workspace
