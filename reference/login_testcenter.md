# Generate a [LoginTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/LoginTestcenter-class.md) object for the IQB Testcenter

Provides a routine to login to an instance of the IQB Testcenter.

## Usage

``` r
login_testcenter(
  base_url = "https://iqb-testcenter2.de/",
  keyring = FALSE,
  change_key = FALSE,
  dialog = TRUE,
  insecure = FALSE,
  verbose = FALSE
)
```

## Arguments

- base_url:

  Character. Base URL of the hosted instance of the IQB Testcenter.
  Default is the https://iqb-testcenter2.de/.

- keyring:

  Logical. Should the
  [keyring::keyring](https://keyring.r-lib.org/reference/keyring-package.html)
  package be used to save the passkey? This saves your credentials to
  your local machine. Defaults to `FALSE`.

- change_key:

  Logical. If your password on the domain has changed - should the
  [keyring::keyring](https://keyring.r-lib.org/reference/keyring-package.html)
  password be changed? Defaults to `FALSE`.

- dialog:

  Logical. Should the password be entered using the RStudio dialog
  (`TRUE`) or using the console (`FALSE`). Defaults to `TRUE`.

- insecure:

  Logical. Should the https security certificate be ignored (only
  recommended for Intranet requests that might not have a valid security
  certificate).

- verbose:

  Logical. If `TRUE`, additional information is printed. Defaults to
  `FALSE`.

## Value

An object of the
[LoginTestcenter](https://iqb-research.github.io/eatPrepTBA/reference/LoginTestcenter-class.md)
class.

## Details

Calling the `login_testcenter()` function generates the following curl
request on the `base_url` (default is https://iqb-testcenter2.de/api)
with the `name` and the `password` provided by the user:

    curl --location --request PUT '{base_url}/session/admin'
    --header 'Content-Type: application/json'
    --data '{
        "name": "{name}",
        "password": "{password}"
    }'

Note that the name and the password are only available to the function
call and cannot be accessed later as they are not part of the
[Login](https://iqb-research.github.io/eatPrepTBA/reference/Login-class.md)
object generated.
