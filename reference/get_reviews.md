# Get reviews

This function returns responses for the selected groups.

## Usage

``` r
get_reviews(workspace, groups = NULL, use_new_version = TRUE)

# S4 method for class 'WorkspaceTestcenter'
get_reviews(workspace, groups = NULL, use_new_version = TRUE)
```

## Arguments

- workspace:

  [WorkspaceTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceTestcenter-class.md).
  IQB Testcenter workspace information necessary to retrieve reviews
  from the API.

- groups:

  Character. Name of the groups to be retrieved or all groups if not
  specified.

- use_new_version:

  Logical. Should the new or the output format be used. Defaults to
  `TRUE`.

## Value

A tibble.

## Functions

- `get_reviews(WorkspaceTestcenter)`: Get responses of a given
  Testcenter workspace
