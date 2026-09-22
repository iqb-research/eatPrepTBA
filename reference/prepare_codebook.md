# Prepares a rectangular codebook

This function is a wrapper around
[`download_codebook()`](https://iqb-research.github.io/eatPrepTBA/reference/download_codebook.md)
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

  Tibble (optional). Missing-value codes with columns `id`, `label`, and
  `description`, added to each variable. When a Studio profile is also
  selected, these codes replace profile codes with the same `id`; all
  other profile codes are retained. Studio itself is not modified.

- missings_profile:

  Character (optional). Exact, case-sensitive label of a missing-value
  profile configured in Studio. With `NULL` (default), no profile is
  selected. Profile codes are added to each variable; the Studio field
  `code` becomes `code_id` in the returned table. An unknown label
  raises an error before downloading.

- only_coded:

  Logical. Should only variables with codes be shown? Defaults to
  `FALSE`.

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

## Details

To include missing-value codes maintained in Studio, set
`missings_profile` to the exact profile label shown in Studio's codebook
export dialog. These codes are returned separately for each unit by
Studio and added to each variable in the prepared table. The numeric
Studio `code` is used as `code_id`; the missing category's identifier
(for example, `mci`) is not used as a code ID.

To supply your own missing-value codes, pass a tibble through
`missings`. If both arguments are supplied, your entries replace profile
entries with matching code IDs, including their labels and descriptions.
Other profile entries are retained, and additional user entries are
appended. These changes affect only the returned table; they do not
modify Studio.

## Functions

- `prepare_codebook(WorkspaceStudio)`: Download a file of a defined
  workspace
