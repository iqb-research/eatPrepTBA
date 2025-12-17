# Prepares a rectangular codebook

This function is a wrapper around
[`download_units()`](https://iqb-research.github.io/eatPrepTBA/reference/download_units.md)
that provides a rectangular codebook.

## Usage

``` r
prepare_codebook(
  workspace,
  unit_keys = NULL,
  missings = NULL,
  missings_profile = NULL,
  only_coded = FALSE,
  general_instructions = FALSE,
  hide_item_var_relation = TRUE,
  derived = TRUE,
  manual = TRUE,
  closed = TRUE,
  show_score = FALSE,
  code_label_to_upper = TRUE
)

# S4 method for class 'WorkspaceStudio'
prepare_codebook(
  workspace,
  unit_keys = NULL,
  missings = NULL,
  missings_profile = NULL,
  only_coded = FALSE,
  general_instructions = FALSE,
  hide_item_var_relation = TRUE,
  derived = TRUE,
  manual = TRUE,
  closed = TRUE,
  show_score = FALSE,
  code_label_to_upper = TRUE
)
```

## Arguments

- workspace:

  [WorkspaceStudio](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceStudio-class.md).
  Workspace information necessary to download codebook via the API.

- unit_keys:

  Character. Keys (short names) of the units in the workspace the
  codebook should be retrieved from. If set to `NULL` (default), the
  codebook will be generated for the all units.

- missings:

  Tibble (optional). Missing table to be added to each variable.

- missings_profile:

  Missings profile. (Currently without effect.)

- only_coded:

  Logical. Should only variables with codes be shown? Defaults to
  `TRUE`.

- general_instructions:

  Logical. Should the general coding instructions be printed? Defaults
  to `FALSE`. (Currently not displayed.)

- hide_item_var_relation:

  Logical. Should item-variable relations be printed? Defaults to
  `TRUE`.

- derived:

  Logical. Should the derived variables be printed? Defaults to `TRUE`.

- manual:

  Logical. Should only items with manual coding be printed? Defaults to
  `TRUE`.

- closed:

  Logical. Should items that could be automatically coded be printed?
  Defaults to `TRUE`.

- show_score:

  Logical. Should the score be printed? Defaults to `FALSE`.

- code_label_to_upper:

  Logical. Should the code labels be printed in capital letters?
  Defaults to `TRUE`.

## Value

A tibble.

## Functions

- `prepare_codebook(WorkspaceStudio)`: Download a file of a defined
  workspace
