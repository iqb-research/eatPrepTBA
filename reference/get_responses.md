# Get responses directly from Testcenter

This function returns responses for the selected groups.

## Usage

``` r
get_responses(workspace, groups = NULL)

# S4 method for class 'WorkspaceTestcenter'
get_responses(workspace, groups = NULL)
```

## Arguments

- workspace:

  [WorkspaceTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceTestcenter-class.md).
  Workspace information necessary to retrieve unit information and
  resources from the API.

- groups:

  Character. Name of the groups to be retrieved or all groups if not
  specified.

## Value

A tibble.

## Functions

- `get_responses(WorkspaceTestcenter)`: Get responses of a given
  Testcenter workspace
