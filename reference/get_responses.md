# Get responses directly from Testcenter

This function returns responses for the selected groups and prepares the
nested response report from the Testcenter for further processing with
[`code_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/code_responses.md).
Units with empty response payloads are kept as rows with
`responses = NA` so that the observed unit structure remains available;
final missing codes are assigned later by
[`complete_design()`](https://iqb-research.github.io/eatPrepTBA/reference/complete_design.md)
when the coded data are checked against the full test design.

## Usage

``` r
get_responses(workspace, groups = NULL, units_filter_off = NULL)

# S4 method for class 'WorkspaceTestcenter'
get_responses(workspace, groups = NULL, units_filter_off = NULL)
```

## Arguments

- workspace:

  [WorkspaceTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceTestcenter-class.md).
  Workspace information necessary to retrieve unit information and
  resources from the API.

- groups:

  Character. Name of the groups to be retrieved or all groups if not
  specified.

- units_filter_off:

  Character. Names of the units to be removed from the dataset.

## Value

A tibble.

## Functions

- `get_responses(WorkspaceTestcenter)`: Get responses of a given
  Testcenter workspace
