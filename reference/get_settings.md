# Get workspace settings

This function returns the workspace settings for a single workspace.

## Usage

``` r
get_settings(workspace, metadata = TRUE)

# S4 method for class 'WorkspaceStudio'
get_settings(workspace, metadata = TRUE)
```

## Arguments

- workspace:

  [WorkspaceStudio](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceStudio-class.md).
  Workspace information necessary to retrieve unit information and
  resources from the API.

- metadata:

  Logical. Should the unit and item metadata profiles be retrieved.
  Defaults to `TRUE`.

## Value

A tibble.

## Functions

- `get_settings(WorkspaceStudio)`: Get unit information and coding
  scheme in a defined workspace
