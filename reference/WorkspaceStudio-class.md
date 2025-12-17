# Workspace access for IQB Studio

A class extending the Login class with additional information for the
IQB Studio. It can be created by the function
[`login_studio()`](https://iqb-research.github.io/eatPrepTBA/reference/login_studio.md).

## Slots

- `ws_id`:

  ID of the workspace. The workspace ID can also be found in the
  workspace URL.

- `ws_label`:

  Label of the workspace.

- `login`:

  [LoginStudio](https://iqb-research.github.io/eatPrepTBA/reference/LoginStudio-class.md).
  Login information for the IQB Studio.

- `wsg_id`:

  ID of the workspace group the current workspace belongs to. See URL to
  identify the workspace group id.

- `wsg_label`:

  Label of the workspace group the current workspace belongs to.
