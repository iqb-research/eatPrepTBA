# Login credentials

A class containing the token to communicate with the API of either the
IQB Studio Lite or the IQB Testcenter. Objects of this class will be
created by the function
[`login_studio()`](https://iqb-research.github.io/eatPrepTBA/reference/login_studio.md)
or
[`login_testcenter()`](https://iqb-research.github.io/eatPrepTBA/reference/login_testcenter.md).

## Slots

- `base_url`:

  Character. Base URL of the instance.

- `base_req`:

  Function. Base
  [httr2::httr2](https://httr2.r-lib.org/reference/httr2-package.html)
  request (will be handled internally).

- `ws_list`:

  Named list. Returns a list of the labels and ids of the workspaces the
  user has access to.

- `app_version`:

  Character. The version of the instance.
