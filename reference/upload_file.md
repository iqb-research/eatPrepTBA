# Upload file

This function uploads a file to the IQB Testcenter.

## Usage

``` r
upload_file(
  workspace,
  path,
  status = c("success", "info", "error", "warning"),
  verbose = FALSE
)

# S4 method for class 'WorkspaceTestcenter'
upload_file(
  workspace,
  path,
  status = c("success", "info", "error", "warning"),
  verbose = FALSE
)
```

## Arguments

- workspace:

  [Workspace](https://iqb-research.github.io/eatPrepTBA/reference/Workspace-class.md).
  Workspace information necessary to upload file via the API.

- path:

  Character. Path to the file to be uploaded.

- status:

  Character. Filters for status messages (`"success"`, `"info"`,
  `"error"`, or `"warning"`)

- verbose:

  Logical. Should status messages be reported? Defaults to `FALSE`.

## Functions

- `upload_file(WorkspaceTestcenter)`: Upload a file into a given
  workspace
