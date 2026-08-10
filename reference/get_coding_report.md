# Get coding report

This function returns the coding report of the given IQB Studio
workspace. Current Studio Lite versions may provide `validationProblems`
in addition to the aggregate `validation` status. These details are
returned as a `validation_problems` list-column with one tibble per
variable and the columns `type`, `breaking`, and `code`.

## Usage

``` r
get_coding_report(workspace)

# S4 method for class 'WorkspaceStudio'
get_coding_report(workspace)
```

## Arguments

- workspace:

  [WorkspaceStudio](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceStudio-class.md).
  Workspace information necessary to retrieve the coding report.

## Value

A tibble.

## Functions

- `get_coding_report(WorkspaceStudio)`: Get the coding report for a
  Studio workspace.
