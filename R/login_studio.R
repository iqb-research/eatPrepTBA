#' Generate a [LoginStudio-class] object for the IQB Studio Lite
#'
#' @description
#' Provides a routine to login to an instance of the IQB Studio Lite.
#'
#' @param base_url Character. Base URL of the hosted instance of the IQB Studio Lite. Default is the https://www.iqb-studio.de/.
#' @param app_version Character. Exact app version of the IQB Studio instance, including any prerelease suffix.
#'   If `NULL`, the version is retrieved automatically.
#' @param keyring Logical. Should the [keyring] package be used to save the passkey? This saves your credentials to your local machine. Defaults to `FALSE`.
#' @param change_key Logical. If your password on the domain has changed - should the [keyring] password be changed? Defaults to `FALSE`.
#' @param dialog Logical. Should credentials be entered using a GUI dialog with masked password input (`TRUE`) or using the console (`FALSE`). Defaults to `TRUE`.
#' @param verbose Logical. If `TRUE`, additional information is printed. Defaults to `FALSE`.
#'
#' @return An object of the [LoginStudio-class] class.
#'
#' @details
#' Studio rejects a mismatching version with HTTP 521. In that case this
#' function raises an `eatPrepTBA_studio_version_mismatch` error with a reminder
#' to set `app_version` to the instance's exact version. After a Studio upgrade,
#' log in again with the new version to create a new request function.
#'
#' The login request sends the username and password as a JSON body to
#' `{base_url}/api/login`. An equivalent curl request is:
#'
#' ```
#' curl --request POST '{base_url}/api/login' \
#'   --header 'app-version: {app_version}' \
#'   --header 'Content-Type: application/json' \
#'   --data '{"username":"{name}","password":"{password}"}'
#' ```
#' The returned [LoginStudio-class] object contains workspace information and
#' a request function that uses the access token for subsequent API calls.
#' It does not retain the password. With `keyring = TRUE`, credentials are saved
#' separately in the local credential store and reused on later logins.
#' @export
login_studio <- function(base_url = "https://www.iqb-studio.de/",
                         app_version = NULL,
                         keyring = FALSE,
                         change_key = FALSE,
                         dialog = TRUE,
                         verbose = FALSE) {
  cli_setting()

  # Define app_version automatically, or use manual input
  app_version <- if (is.null(app_version)) {
    get_studio_version(base_url)
    } else {
      app_version
      }

  # Input validation using checkmate
  assert_url(base_url, "base_url")
  checkmate::assert_character(app_version, len = 1)
  checkmate::assert_logical(keyring, len = 1)
  checkmate::assert_logical(change_key, len = 1)
  checkmate::assert_logical(dialog, len = 1)
  checkmate::assert_logical(verbose, len = 1)

  # Authentication
  credentials <- get_credentials(base_url = base_url,
                                 keyring = keyring,
                                 change_key = change_key,
                                 dialog = dialog)

  version_mismatch <- function(cnd) {
    cli::cli_abort(
      c(
        "Studio rejected {.arg app_version} = {.val {app_version}} (HTTP 521).",
        "i" = "Set {.arg app_version} to the exact version displayed by {.url {base_url}}, including any prerelease suffix, and log in again."
      ),
      class = "eatPrepTBA_studio_version_mismatch",
      app_version = app_version,
      base_url = base_url,
      status = 521L,
      parent = cnd
    )
  }

  token <-
    tryCatch(
      httr2::request(base_url = base_url) %>%
        httr2::req_url_path_append("api", "login") %>%
        httr2::req_headers("app-version" = app_version) %>%
        httr2::req_method("POST") %>%
        httr2::req_body_json(data = list(username = credentials$name,
                                      password = credentials$password)) %>%
        httr2::req_perform() %>%
        httr2::resp_body_json(),
      httr2_http_521 = version_mismatch
    )

  auth_token <- glue::glue("Bearer {token$accessToken}")

  base_req <- generate_base_req(type = "studio",
                                base_url = base_url,
                                auth_token = auth_token,
                                app_version = app_version)

  run_req <- function() {
    base_req(method = "GET",
             endpoint = "auth-data",
             query = list()) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()
  }

  # Perform request
  resp <-
    tryCatch(
      error = function(cnd) {
        if (inherits(cnd, "httr2_http_521")) version_mismatch(cnd)

        cli::cli_alert_danger("Login was not successful.",
                              wrap = TRUE)

        cli::cli_text("{.strong Status:}  {cnd$status} | {cnd$message}")
      },
      run_req()
    )

  # Process HTTP response to ws group list and ws list
  wsg_list <-
    resp %>%
    purrr::pluck("workspaces") %>%
    purrr::map(function(wsg) {
      # Streamline workspace groups
      list(
        wsg_id = wsg$id,
        wsg_label = wsg$name,
        ws_list = wsg$workspaces
      )
    }) %>%
    purrr::map(function(wsg) {
      # Streamline workspaces (overwrite list entries)
      wsg$ws_list <-
        wsg$ws_list %>%
        purrr::map(function(ws) {
          list(
            ws_id = ws$id,
            ws_label = ws$name
          )})

      wsg
    })

  ws_list <-
    wsg_list %>%
    purrr::map("ws_list") %>%
    purrr::list_flatten()

  # Initialize Login object
  Login <- new("LoginStudio",
               base_url = base_url,
               base_req = base_req,
               ws_list = ws_list,
               wsg_list = wsg_list,
               user_id = resp$userId,
               user_key = resp$userName,
               user_label = resp$userLongName,
               app_version = app_version
  )

  cli::cli_alert_success("IQB Studio login was successful.")

  if (verbose) {
    cli::cli_text("You are logged in to the IQB Studio Lite at {.url {base_url}} as {.user-label {resp$userName}}.")

    show(Login)
  }

  return(invisible(Login))
}
