# List unit resource files

This function serves as a wrapper for
[`list_files()`](https://iqb-research.github.io/eatPrepTBA/reference/list_files.md).

## Usage

``` r
list_resources(workspace)

# S4 method for class 'Workspace'
list_resources(workspace)
```

## Arguments

- workspace:

  [WorkspaceTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceTestcenter-class.md).
  Workspace information necessary to retrieve unit resource list (coding
  scheme and aspect data) from the API.

## Value

A tibble.

## Functions

- `list_resources(Workspace)`: List all resources in a given IQB
  Testcenter workspace
