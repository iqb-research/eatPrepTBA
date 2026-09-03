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
#' Calling the `login_testcenter()` function first tries the following curl request
#' on the `base_url` (default is https://iqb-testcenter2.de/api) with the `name` and
#' the `password` provided by the user. On Testcenter installations with active
#' brute-force protection, the function automatically falls back to the challenge
#' based login flow introduced in Testcenter 18.2.
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
  assert_url(base_url, "base_url")
  checkmate::assert_logical(keyring, len = 1)
  checkmate::assert_logical(change_key, len = 1)
  checkmate::assert_logical(dialog, len = 1)
  checkmate::assert_logical(insecure, len = 1)
  checkmate::assert_logical(verbose, len = 1)

  # Authentication
  credentials <- get_credentials(base_url = base_url,
                                 keyring = keyring,
                                 change_key = change_key,
                                 dialog = dialog)

  # Perform request
  resp <-
    tryCatch(
      error = function(cnd) {
        cli::cli_alert_danger("Login was not successful.
                              Please check if you have admin rights or
                              are already logged in on a browser.",
                              wrap = TRUE)

        cli::cli_text("{.strong Status:}  {cnd$status} | {cnd$message}")
        stop(cnd)
      },
      login_testcenter_admin_session(
        base_url = base_url,
        credentials = credentials,
        insecure = insecure
      )
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

login_testcenter_admin_session <- function(base_url,
                                           credentials,
                                           insecure = FALSE) {
  resp <- request_testcenter_admin_session(
    base_url = base_url,
    credentials = credentials,
    insecure = insecure
  )

  if (httr2::resp_status(resp) < 400) {
    return(httr2::resp_body_json(resp))
  }

  if (login_testcenter_challenge_required(resp)) {
    cli::cli_alert_info("Testcenter requires a login challenge; solving it now.")
    return(request_testcenter_challenge_session(
      base_url = base_url,
      credentials = credentials,
      insecure = insecure
    ))
  }

  httr2::resp_check_status(resp)
}

request_testcenter_admin_session <- function(base_url,
                                             credentials,
                                             insecure = FALSE) {
  login_testcenter_session_request(
    base_url = base_url,
    endpoint = c("session", "admin"),
    insecure = insecure
  ) %>%
    httr2::req_method("PUT") %>%
    httr2::req_body_json(credentials) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()
}

request_testcenter_challenge_session <- function(base_url,
                                                 credentials,
                                                 insecure = FALSE) {
  challenge <- login_testcenter_session_request(
    base_url = base_url,
    endpoint = c("session", "challenge"),
    insecure = insecure
  ) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(c(list(loginType = "admin"), credentials)) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  solution <- solve_altcha_v1_challenge(challenge)

  login_testcenter_session_request(
    base_url = base_url,
    endpoint = c("session"),
    insecure = insecure
  ) %>%
    httr2::req_method("PUT") %>%
    httr2::req_body_json(solution) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
}

login_testcenter_session_request <- function(base_url,
                                             endpoint,
                                             insecure = FALSE) {
  base_req <-
    httr2::request(base_url = base_url) %>%
    httr2::req_url_path_append("api", endpoint) %>%
    httr2::req_headers("Content-Type" = "application/json")

  if (insecure) {
    # Added for Intranet requests
    base_req <-
      base_req %>%
      httr2::req_options(
        ssl_verifypeer = FALSE,
        ssl_verifyhost = FALSE
      )
  }

  base_req
}

login_testcenter_challenge_required <- function(resp) {
  if (httr2::resp_status(resp) != 400) {
    return(FALSE)
  }

  body <- tryCatch(
    httr2::resp_body_string(resp),
    error = function(cnd) ""
  )

  grepl(
    "Brute Force protection active|Challenge .* must be solved",
    body,
    ignore.case = TRUE
  )
}

solve_altcha_v1_challenge <- function(challenge,
                                      timeout = getOption("eatPrepTBA.altcha_timeout", 90)) {
  required_fields <- c("algorithm", "challenge", "salt", "signature")
  missing_fields <- setdiff(required_fields, names(challenge))

  if (length(missing_fields) > 0) {
    stop(
      "Testcenter challenge response is missing field(s): ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }

  max_number <- challenge$maxNumber
  if (is.null(max_number)) {
    max_number <- challenge$maxnumber
  }
  if (is.null(max_number) || length(max_number) != 1 || is.na(max_number)) {
    stop("Testcenter challenge response is missing a valid maxNumber.", call. = FALSE)
  }

  algorithm <- switch(
    challenge$algorithm,
    "SHA-1" = "sha1",
    "SHA-256" = "sha256",
    "SHA-512" = "sha512",
    stop("Unsupported Testcenter challenge algorithm: ", challenge$algorithm, call. = FALSE)
  )

  max_number <- suppressWarnings(as.integer(max_number))
  if (is.na(max_number) || max_number < 0) {
    stop("Testcenter challenge response is missing a valid maxNumber.", call. = FALSE)
  }

  started_at <- Sys.time()

  for (number in seq.int(0L, max_number)) {
    digest <- digest::digest(
      paste0(challenge$salt, number),
      algo = algorithm,
      serialize = FALSE
    )

    if (identical(digest, challenge$challenge)) {
      return(list(
        algorithm = challenge$algorithm,
        challenge = challenge$challenge,
        salt = challenge$salt,
        signature = challenge$signature,
        number = number
      ))
    }

    if (number %% 10000L == 0L &&
        as.numeric(difftime(Sys.time(), started_at, units = "secs")) > timeout) {
      stop("Solving the Testcenter login challenge timed out.", call. = FALSE)
    }
  }

  stop("Could not solve the Testcenter login challenge.", call. = FALSE)
}
