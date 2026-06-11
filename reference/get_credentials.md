# Get credentials for access management

This function returns credentials object for either signing in to the
IQB Studio Lite or the IQB Testcenter (only used internally).

## Usage

``` r
get_credentials(base_url, keyring, change_key, dialog, ...)
```

## Arguments

- base_url:

  Character. Base URL of the instance.

- keyring:

  Logical. Should the
  [keyring](https://keyring.r-lib.org/reference/keyring-package.html)
  package be used?

- change_key:

  Logical. Should the
  [keyring](https://keyring.r-lib.org/reference/keyring-package.html)
  credentials be changed (only valid, if keyring is set to `TRUE`)?

- dialog:

  Logical. Should the dialog asking for username and password be used?

- ...:

  Additional arguments. Only for testing purposes.

## Value

A `list` with entries name and password.
