#' Generate a [LoginStudio-class] object for the IQB Studio Lite
#'
#' @description
#' Provides a routine to login to an instance of the IQB Studio Lite.
#'
#' @param base_url Character. Base URL of the hosted instance of the IQB Studio Lite. Default is the https://www.iqb-studio.de/.
#' @param app_version Character. App version of the IQB Studio instance. Defaults to "13.8.0".
#' @param keyring Logical. Should the [keyring] package be used to save the passkey? This saves your credentials to your local machine. Defaults to `FALSE`.
#' @param change_key Logical. If your password on the domain has changed - should the [keyring] password be changed? Defaults to `FALSE`.
#' @param dialog Logical. Should the password be entered using the RStudio dialog (`TRUE`) or using the console (`FALSE`). Defaults to `TRUE`.
#' @param verbose Logical. If `TRUE`, additional information is printed. Defaults to `FALSE`.
#'
#' @return An object of the [LoginStudio-class] class.
#'
#' @details
#' Calling the `login_studio()` function generates the following curl request
#' on the `base_url` (default is "https://www.iqb-studio.de/) with the `name`
#' and the `password` provided by the user:
#'
#' ```
#' curl --location --request POST '{base_url}/api/login?username={name}&password={password}'
#' --header 'app-version: 13.8.0'
#' }'
#' ```
#' Note that the name and the password are only available to the function call
#' and cannot be accessed later as they are not part of the [Login-class] object generated.
#' @export
login_studio <- function(base_url = "https://www.iqb-studio.de/",
                         app_version = "13.8.0",
                         keyring = FALSE,
                         change_key = FALSE,
                         dialog = TRUE,
                         verbose = FALSE) {
  cli_setting()

  # Authentication
  credentials <- get_credentials(base_url = base_url,
                                 keyring = keyring,
                                 change_key = change_key,
                                 dialog = dialog)

  token <-
    httr2::request(base_url = base_url) %>%
    httr2::req_url_path_append("api", "login") %>%
    httr2::req_headers("app-version" = app_version) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(data = list(username = credentials$name,
                                     password = credentials$password)) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
    stringr::str_remove_all("\"")

  auth_token <- glue::glue("Bearer {token}")

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
