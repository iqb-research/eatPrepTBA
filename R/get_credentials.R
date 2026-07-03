#' Get credentials for access management
#'
#' @param base_url Character. Base URL of the instance.
#' @param keyring Logical. Should the [keyring] package be used?
#' @param change_key Logical. Should the [keyring] credentials be changed (only valid, if keyring is set to `TRUE`)?
#' @param dialog Logical. Should the dialog asking for username and password be used?
#' @param ... Additional arguments. Only for testing purposes.
#'
#' @description
#' This function returns credentials object for either signing in to the IQB Studio Lite or the IQB Testcenter (only used internally).
#'
#' @return A `list` with entries name and password.
#'
#' @keywords internal
get_credentials <- function(base_url, keyring, change_key, dialog, ...) {
  # input validation
  assert_url(base_url, "base_url")
  checkmate::assert_logical(keyring, len = 1)
  checkmate::assert_logical(change_key, len = 1)
  checkmate::assert_logical(dialog, len = 1)

  is_r_studio <- Sys.getenv("RSTUDIO") == "1"
  test_mode <- getOption("eatPrepTBA.test_mode")

  name_prompt <- stringr::str_glue("Enter your username for {base_url}: ")
  password_prompt <- stringr::str_glue("Enter your password for {base_url}: ")


  if (keyring) {
    name <- keyring::key_list(service = base_url)[["username"]]
    has_key <- length(name) != 0

    if (! has_key || change_key) {
      if (change_key) {
        keyring::key_delete(service = base_url, username = name)
      }

      if (is_r_studio) {
        name <- rstudioapi::askForPassword(name_prompt)
      } else {
        name <- readline(name_prompt)
      }

      keyring::key_set(service = base_url, username = name)
    }

    password <- keyring::key_get(service = base_url, username = name)
  } else {
    if (is.null(test_mode) || ! test_mode) {
      if (is_r_studio & dialog) {
        name <- rstudioapi::askForPassword(name_prompt)
        password <- rstudioapi::askForPassword(password_prompt)
      } else {
        name <- readline(prompt = name_prompt)
        password <- readline(prompt = password_prompt)
      }
    } else {
      # Routine for testing purposes only
      credentials <- rlang::list2(...)

      if (is.null(credentials) || is.null(credentials$name) || is.null(credentials$password)) {
        name <- "eatPrepTBA"
        password <- "eatPrepTBA"
      }

      if (!is.null(credentials$name)) {
        name <- credentials$name
      }
      if (!is.null(credentials$password)) {
        password <- credentials$password
      }
    }
  }

  credentials <- list(
    name = name,
    password = password
  )

  return(credentials)
}
