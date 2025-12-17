# List files

Please note the wrapper functions
[`list_units()`](https://iqb-research.github.io/eatPrepTBA/reference/list_units.md),
[`list_booklets()`](https://iqb-research.github.io/eatPrepTBA/reference/list_booklets.md),
and
[`list_testtakers()`](https://iqb-research.github.io/eatPrepTBA/reference/list_testtakers.md).

## Usage

``` r
list_files(workspace, type = NULL, dependencies = FALSE)

# S4 method for class 'WorkspaceTestcenter'
list_files(workspace, type = NULL, dependencies = FALSE)
```

## Arguments

- workspace:

  [WorkspaceTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceTestcenter-class.md).
  Workspace information necessary to retrieve file list from the API.

- type:

  Character (optional). Type of the files to retrieve from the API. If
  no type is specified, all files are listed.

- dependencies:

  Logical. Should the dependencies be listed along with the resources?
  Defaults to `FALSE`.

## Value

A tibble.

## Functions

- `list_files(WorkspaceTestcenter)`: List all files in a given IQB
  Testcenter workspace
