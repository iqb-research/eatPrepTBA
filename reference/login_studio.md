# Generate a [LoginStudio](https://iqb-research.github.io/eatPrepTBA/reference/LoginStudio-class.md) object for the IQB Studio Lite

Provides a routine to login to an instance of the IQB Studio Lite.

## Usage

``` r
login_studio(
  base_url = "https://www.iqb-studio.de/",
  app_version = "16.0.0",
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

  Character. Exact app version of the IQB Studio instance, including any
  prerelease suffix. Defaults to "16.0.0" for compatibility; the server
  version is not detected automatically. Set this explicitly to the
  version displayed by your instance, e.g. `"21.0.0"` after its upgrade.

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

  Logical. Should credentials be entered using a GUI dialog with masked
  password input (`TRUE`) or using the console (`FALSE`). Defaults to
  `TRUE`.

- verbose:

  Logical. If `TRUE`, additional information is printed. Defaults to
  `FALSE`.

## Value

An object of the
[LoginStudio](https://iqb-research.github.io/eatPrepTBA/reference/LoginStudio-class.md)
class.

## Details

Studio rejects a mismatching version with HTTP 521. In that case this
function raises an `eatPrepTBA_studio_version_mismatch` error with a
reminder to set `app_version` to the instance's exact version. After a
Studio upgrade, log in again with the new version to create a new
request function.

The login request sends the username and password as a JSON body to
`{base_url}/api/login`. An equivalent curl request is:

    curl --request POST '{base_url}/api/login' \
      --header 'app-version: {app_version}' \
      --header 'Content-Type: application/json' \
      --data '{"username":"{name}","password":"{password}"}'

The returned
[LoginStudio](https://iqb-research.github.io/eatPrepTBA/reference/LoginStudio-class.md)
object contains workspace information and a request function that uses
the access token for subsequent API calls. It does not retain the
password. With `keyring = TRUE`, credentials are saved separately in the
local credential store and reused on later logins.
