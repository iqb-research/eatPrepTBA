# Generate a [LoginStudio](https://iqb-research.github.io/eatPrepTBA/reference/LoginStudio-class.md) object for the IQB Studio Lite

Provides a routine to login to an instance of the IQB Studio Lite.

## Usage

``` r
login_studio(
  base_url = "https://www.iqb-studio.de/",
  app_version = "13.8.0",
  keyring = FALSE,
  change_key = FALSE,
  dialog = TRUE,
  verbose = FALSE
)
```

## Arguments

- base_url:

  Character. Base URL of the hosted instance of the IQB Studio Lite.
  Default is the https://www.iqb-studio.de/.

- app_version:

  Character. App version of the IQB Studio instance. Defaults to
  "13.8.0".

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

- verbose:

  Logical. If `TRUE`, additional information is printed. Defaults to
  `FALSE`.

## Value

An object of the
[LoginStudio](https://iqb-research.github.io/eatPrepTBA/reference/LoginStudio-class.md)
class.

## Details

Calling the `login_studio()` function generates the following curl
request on the `base_url` (default is "https://www.iqb-studio.de/) with
the `name` and the `password` provided by the user:

    curl --location --request POST '{base_url}/api/login?username={name}&password={password}'
    --header 'app-version: 13.8.0'
    }'

Note that the name and the password are only available to the function
call and cannot be accessed later as they are not part of the
[Login](https://iqb-research.github.io/eatPrepTBA/reference/Login-class.md)
object generated.
