# Get states

Get states

## Usage

``` r
get_states(workspace)

# S4 method for class 'WorkspaceStudio'
get_states(workspace)
```

## Arguments

- workspace:

  [WorkspaceStudio](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceStudio-class.md).
  Workspace information necessary to retrieve state list from the API.

## Value

A tibble.

## Details

Returns a list of states of a workspace group in the IQB Studio, i.e.,
their id, their label, and their associated colors.

## Functions

- `get_states(WorkspaceStudio)`: List all units in a defined workspace
