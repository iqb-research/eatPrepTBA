# Downloads responses directly from Testcenter

This function downloads the raw response report for the selected groups.
It keeps one row per reported unit, including units whose nested
response payload is empty. These empty payloads are represented as empty
list entries in the raw output; after
[`get_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/get_responses.md)
prepares the data, they appear as `responses = NA` and are left to
[`complete_design()`](https://iqb-research.github.io/eatPrepTBA/reference/complete_design.md)
for design-based missing completion.

## Usage

``` r
download_responses(workspace, groups = NULL, units_filter_off = NULL)

# S4 method for class 'WorkspaceTestcenter'
download_responses(workspace, groups = NULL, units_filter_off = NULL)
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

- `download_responses(WorkspaceTestcenter)`: Get responses of a given
  Testcenter workspace
