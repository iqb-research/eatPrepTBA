# Download units

This function downloads units from the IQB Studio Lite.

## Usage

``` r
download_units(
  workspace,
  path,
  unit_keys = NULL,
  add_players = TRUE,
  add_testtakers_review = 0,
  add_testtakers_monitor = 0,
  add_testtakers_hot = 0,
  password_less = FALSE,
  booklet_settings = list()
)
```

## Arguments

- workspace:

  [WorkspaceStudio](https://iqb-research.github.io/eatPrepTBA/reference/WorkspaceStudio-class.md).
  Workspace information necessary to download files via the API.

- path:

  Character. Path for the zip files of the workspaces to be downloaded.

- unit_keys:

  Character. Keys (short names) of the units in the workspace that
  should be downloaded. If set to `NULL` (default), all units in the
  workspace will be downloaded.

- add_players:

  Logical. Should the resepective Aspect Player(s) of the selected units
  be added? Defaults to `TRUE`.

- add_testtakers_review:

  Numeric. Number of Testcenter review logins (`run-review`). Defaults
  to `0`.

- add_testtakers_monitor:

  Numeric. Number of Testcenter monitor logins (`monitor-group`).
  Defaults to `0`.

- add_testtakers_hot:

  Numeric. Number of Testcenter testtaker logins (`run-review`).
  Defaults to `0`.

- password_less:

  Logical. Should passwords be added to the logins? Defaults to `FALSE`.

- booklet_settings:

  List. Settings for booklet parameters. Please refer to:
  https://pages.cms.hu-berlin.de/iqb/testcenter/pages/booklet-config.html
