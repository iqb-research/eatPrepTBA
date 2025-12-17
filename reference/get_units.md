# Get multiple units with resources

This function returns the unit information for multiple units. git

## Usage

``` r
get_units(workspace, units = NULL, metadata = TRUE, unit_definition = FALSE)

# S4 method for class 'WorkspaceStudio'
get_units(workspace, units = NULL, metadata = TRUE, unit_definition = FALSE)
```

## Arguments

- workspace:

  [Workspace](https://iqb-research.github.io/eatPrepTBA/reference/Workspace-class.md).
  Workspace information necessary to retrieve unit information and
  resources from the API.

- units:

  Tibble. Recently queried units of this workspace that should be
  updated. Please note that units that are no longer in the workspace
  will automatically be deleted by the procedure.

- metadata:

  Logical. Should the metadata be added? Defaults to `TRUE`.

- unit_definition:

  Logical. Should the unit definition be added? Defaults to `FALSE`.
  This function also infers the pages of the variables from the unit
  definition.

## Value

A tibble.

## Functions

- `get_units(WorkspaceStudio)`: Get multiple unit information and coding
  schemes in a given Studio workspace
