# Login credentials for IQB Studio

A class extending the Login class with additional information for the
IQB Studio. It can be created by the function
[`login_studio()`](https://iqb-research.github.io/eatPrepTBA/reference/login_studio.md).

## Slots

- `base_url`:

  Character. Base URL of the IQB Studio installation.

- `base_req`:

  Function. Base
  [httr2::httr2](https://httr2.r-lib.org/reference/httr2-package.html)
  request (will be handled internally).

- `ws_list`:

  Named list. Returns a list of the labels and ids of the workspaces the
  user has access to.

- `wsg_list`:

  Named list. Returns a list of the workspace groups the user has access
  to.

- `user_id`:

  Numeric. ID of the user.

- `user_key`:

  Character. Short name of the user (login name).

- `user_label`:

  Character. Long name of the user.

- `app_version`:

  Character. The version of the IQB Studio installation.
