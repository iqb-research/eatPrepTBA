# List booklet files

This function serves as a wrapper for
[`list_files()`](https://iqb-research.github.io/eatPrepTBA/reference/list_files.md).

## Usage

``` r
list_booklets(workspace)

# S4 method for class 'WorkspaceTestcenter'
list_booklets(workspace)
```

## Arguments

- workspace:

  [WorkspaceTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceTestcenter-class.md).
  Workspace information necessary to retrieve booklet list from the API.

## Value

A tibble.

## Functions

- `list_booklets(WorkspaceTestcenter)`: List all booklets in a given IQB
  Testcenter workspace
