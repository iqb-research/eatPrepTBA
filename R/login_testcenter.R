#' Generate a [LoginTestcenter-class] object for the IQB Testcenter
#'
#' @description
#' Provides a routine to login to an instance of the IQB Testcenter.
#'
#' @param base_url Character. Base URL of the hosted instance of the IQB Testcenter. Default is the https://iqb-testcenter2.de/.
#' @param keyring Logical. Should the [keyring] package be used to save the passkey? This saves your credentials to your local machine. Defaults to `FALSE`.
#' @param change_key Logical. If your password on the domain has changed - should the [keyring] password be changed? Defaults to `FALSE`.
#' @param dialog Logical. Should the password be entered using the RStudio dialog (`TRUE`) or using the console (`FALSE`). Defaults to `TRUE`.
#' @param insecure Logical. Should the https security certificate be ignored (only recommended for Intranet requests that might not have a valid security certificate).
#' @param verbose Logical. If `TRUE`, additional information is printed. Defaults to `FALSE`.
#'
#' @return An object of the [LoginTestcenter-class] class.
#'
#' @details
#' Calling the `login_testcenter()` function generates the following curl request
#' on the `base_url` (default is https://iqb-testcenter2.de/api) with the `name` and
#' the `password` provided by the user:
#'
#' ```
#' curl --location --request PUT '{base_url}/session/admin'
#' --header 'Content-Type: application/json'
#' --data '{
#'     "name": "{name}",
#'     "password": "{password}"
#' }'
#' ```
#' Note that the name and the password are only available to the function call
#' and cannot be accessed later as they are not part of the [Login-class] object generated.
#' @export
login_testcenter <- function(base_url = "https://iqb-testcenter2.de/",
                             keyring = FALSE,
                             change_key = FALSE,
                             dialog = TRUE,
                             insecure = FALSE,
                             verbose = FALSE) {
  cli_setting()

  # Authentication
  credentials <- get_credentials(base_url = base_url,
                                 keyring = keyring,
                                 change_key = change_key,
                                 dialog = dialog)

  run_req <- function() {
    base_req <-
      httr2::request(base_url = base_url) %>%
      httr2::req_url_path_append("api", "session", "admin") %>%
      httr2::req_headers("Content-Type" = "application/json") %>%
      httr2::req_method("PUT") %>%
      httr2::req_body_json(credentials)

    if (insecure) {
      # Added for Intranet requests
      base_req <-
        base_req %>%
        httr2::req_options(
          ssl_verifypeer = FALSE,
          ssl_verifyhost = FALSE
        )
    }

    base_req %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()
  }

  # Perform request
  resp <-
    tryCatch(
      error = function(cnd) {
        cli::cli_alert_danger("Login was not successful.
                              Please check if you have admin rights or
                              are already logged in on a browser.",
                              wrap = TRUE)

        cli::cli_text("{.strong Status:}  {cnd$status} | {cnd$message}")
      },
      run_req()
    )

  base_req <- generate_base_req(type = "testcenter",
                                base_url = base_url,
                                auth_token = resp$token,
                                insecure = insecure)

  ws_list <-
    resp$claims$workspaceAdmin %>%
    purrr::map(function(ws) {
      list(
        ws_id = ws$id,
        ws_label = ws$label
      )
    })

  app_version <-
    base_req(method = "GET", endpoint = c("version")) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  # Initialize Login object
  Login <- new("LoginTestcenter",
               base_url = base_url,
               base_req = base_req,
               ws_list = ws_list,
               app_version = app_version$version
  )

  cli::cli_alert_success("IQB Testcenter login was successful.")

  if (verbose) {
    cli::cli_text("You are logged in to the IQB Studio Lite at {.url {base_url}} as {.user-label {resp$displayName}}.")
    cli::cli_par()
    cli::cli_alert_warning("Please note that the login becomes invalid if you log in to the Testcenter manually.")


    show(Login)
  }

  return(invisible(Login))
}
