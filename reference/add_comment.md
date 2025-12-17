# Add a comment for a unit

This function adds an entry in the comment tab of the Studio of a single
unit.

## Usage

``` r
add_comment(login, ws_id, unit_id, parent_id = NULL, comment = NULL)

# S4 method for class 'LoginStudio'
add_comment(login, ws_id, unit_id, parent_id = NULL, comment = NULL)
```

## Arguments

- login:

  [LoginStudio](https://iqb-research.github.io/eatPrepTBA/reference/LoginStudio-class.md).
  Login information necessary to add comments.

- ws_id:

  Workspace id of the unit.

- unit_id:

  Unit id for the comment to add.

- parent_id:

  Comment id in case of responding to another comment.

- comment:

  Comment to be added to the unit. Should contain html markup.

## Functions

- `add_comment(LoginStudio)`: Add a comment in a defined workspace
