# Download codebook

This function downloads codebooks from the IQB Studio.

## Usage

``` r
download_codebook(
  workspace,
  path,
  file_prefix = "",
  unit_keys = NULL,
  format = c("docx", "json"),
  missings_profile = NULL,
  only_coded = FALSE,
  general_instructions = TRUE,
  hide_item_var_relation = TRUE,
  derived = TRUE,
  manual = TRUE,
  closed = TRUE,
  show_score = FALSE,
  code_label_to_upper = TRUE
)

# S4 method for class 'WorkspaceStudio'
download_codebook(
  workspace,
  path,
  file_prefix = "",
  unit_keys = NULL,
  format = c("docx", "json"),
  missings_profile = NULL,
  only_coded = FALSE,
  general_instructions = TRUE,
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

- path:

  Character. Path of the directory for the codebook files to be
  downloaded to. The default filename is determined by the id and the
  label of the IQB Studio workspace.

- file_prefix:

  Character (optional). Path prefix, e.g., a date to keep track of
  different versions of the codebook.

- unit_keys:

  Character. Keys (short names) of the units in the workspace the
  codebook should be retrieved from. If set to `NULL` (default), the
  codebook will be generated for the all units.

- format:

  Character. Either `"docx"` (default) or `"json"`.

- missings_profile:

  Missings profile. (Currently without effect.)

- only_coded:

  Logical. Should only variables with codes be shown?

- general_instructions:

  Logical. Should the general coding instructions be printed? Defaults
  to `TRUE`.

- hide_item_var_relation:

  Logocal. Should item-variable relations be printed? Defaults to
  `FALSE`.

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

## Functions

- `download_codebook(WorkspaceStudio)`: Upload a file in a defined
  workspace
