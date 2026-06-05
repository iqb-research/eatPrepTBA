# Workspace access for IQB Testcenter

A class extending the Workspace class with additional information for
the IQB Testcenter. It can be created by the function
[`access_workspace()`](https://iqb-research.github.io/eatPrepTBA/reference/access_workspace.md)
after
[`login_testcenter()`](https://iqb-research.github.io/eatPrepTBA/reference/login_testcenter.md).

## Slots

- `login`:

  [LoginTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/LoginTestcenter-class.md).
  Login information for the IQB Testcenter.

- `ws_id`:

  ID of the workspace. The workspace ID can also be found in the
  workspace URL.

- `ws_label`:

  Label of the workspace.
