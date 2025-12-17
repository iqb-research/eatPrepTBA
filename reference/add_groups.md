# Add workspace groups for a Studio workspace

This function adds groups to a Studio workspace.

## Usage

``` r
add_groups(workspace, group_names)

# S4 method for class 'WorkspaceStudio'
add_groups(workspace, group_names)
```

## Arguments

- workspace:

  [WorkspaceStudio](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceStudio-class.md).
  Workspace information necessary to add groups

- group_names:

  Character. Names of the groups to be added (already available groups
  will not result in duplicate groups)

## Functions

- `add_groups(WorkspaceStudio)`: Add groups in a defined workspace
