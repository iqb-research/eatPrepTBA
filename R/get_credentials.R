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

      credentials <- prompt_credentials(name_prompt, password_prompt, dialog)
      name <- credentials$name

      keyring::key_set_with_value(
        service = base_url,
        username = name,
        password = credentials$password
      )
    }

    password <- keyring::key_get(service = base_url, username = name)
  } else {
    if (is.null(test_mode) || ! test_mode) {
      credentials <- prompt_credentials(name_prompt, password_prompt, dialog)
      name <- credentials$name
      password <- credentials$password
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

prompt_credentials <- function(name_prompt, password_prompt, dialog) {
  if (dialog) {
    credentials <- tcltk_prompt_credentials(name_prompt, password_prompt)
    if (!is.null(credentials)) {
      return(credentials)
    }

    credentials <- rstudio_prompt_credentials(name_prompt, password_prompt)
    if (!is.null(credentials)) {
      return(credentials)
    }
  }

  list(
    name = readline(prompt = name_prompt),
    password = readline(prompt = password_prompt)
  )
}

use_rstudio_credential_dialog <- function(dialog) {
  if (!dialog) {
    return(FALSE)
  }

  is_rstudio_env <- identical(Sys.getenv("RSTUDIO"), "1")
  is_rstudio_api_available <- tryCatch(
    {
      is_available_args <- names(formals(rstudioapi::isAvailable))
      if ("child_ok" %in% is_available_args) {
        rstudioapi::isAvailable(child_ok = TRUE)
      } else {
        rstudioapi::isAvailable()
      }
    },
    error = function(cnd) FALSE
  )

  is_rstudio_env || isTRUE(is_rstudio_api_available)
}

rstudio_prompt_credentials <- function(name_prompt, password_prompt) {
  if (!use_rstudio_credential_dialog(TRUE)) {
    return(NULL)
  }

  tryCatch(
    list(
      name = rstudio_prompt_username(name_prompt),
      password = rstudio_prompt_password(password_prompt)
    ),
    error = function(cnd) NULL
  )
}

rstudio_prompt_username <- function(prompt) {
  tryCatch(
    rstudioapi::showPrompt(
      title = "Login",
      message = prompt,
      default = ""
    ),
    error = function(cnd) rstudio_prompt_password(prompt)
  )
}

rstudio_prompt_password <- function(prompt) {
  rstudioapi::askForPassword(prompt)
}

tcltk_prompt_credentials <- function(name_prompt, password_prompt) {
  if (!interactive() ||
      !requireNamespace("tcltk", quietly = TRUE) ||
      !isTRUE(capabilities()[["tcltk"]])) {
    return(NULL)
  }

  top <- tryCatch(
    tcltk::tktoplevel(),
    error = function(cnd) NULL
  )
  if (is.null(top)) {
    return(NULL)
  }

  result <- new.env(parent = emptyenv())
  result$cancelled <- TRUE
  result$name <- NULL
  result$password <- NULL

  name_var <- tcltk::tclVar("")
  password_var <- tcltk::tclVar("")

  on.exit(try(tcltk::tkgrab.release(top), silent = TRUE), add = TRUE)
  on.exit(try(tcltk::tkdestroy(top), silent = TRUE), add = TRUE)

  accept <- function(...) {
    result$cancelled <- FALSE
    result$name <- tcltk::tclvalue(name_var)
    result$password <- tcltk::tclvalue(password_var)
    try(tcltk::tkdestroy(top), silent = TRUE)
  }
  cancel <- function(...) {
    result$cancelled <- TRUE
    try(tcltk::tkdestroy(top), silent = TRUE)
  }

  tcltk::tkwm.title(top, "Login")
  try(tcltk::tcl("wm", "attributes", top, "-topmost", TRUE), silent = TRUE)

  frame <- tcltk::tkframe(top, padx = 12, pady = 10)
  name_label <- tcltk::tklabel(frame, text = name_prompt, anchor = "w")
  name_entry <- tcltk::tkentry(frame, textvariable = name_var, width = 46)
  password_label <- tcltk::tklabel(frame, text = password_prompt, anchor = "w")
  password_entry <- tcltk::tkentry(frame, textvariable = password_var, show = "*", width = 46)
  button_frame <- tcltk::tkframe(frame)
  ok_button <- tcltk::tkbutton(button_frame, text = "OK", width = 10, command = accept)
  cancel_button <- tcltk::tkbutton(button_frame, text = "Cancel", width = 10, command = cancel)

  tcltk::tkgrid(frame)
  tcltk::tkgrid(name_label, sticky = "w", pady = c(0, 3))
  tcltk::tkgrid(name_entry, sticky = "ew", pady = c(0, 8))
  tcltk::tkgrid(password_label, sticky = "w", pady = c(0, 3))
  tcltk::tkgrid(password_entry, sticky = "ew", pady = c(0, 10))
  tcltk::tkgrid(button_frame, sticky = "e")
  tcltk::tkgrid(ok_button, cancel_button, padx = 3)

  tcltk::tkbind(top, "<Return>", accept)
  tcltk::tkbind(top, "<Escape>", cancel)
  try(tcltk::tkgrab.set(top), silent = TRUE)
  try(tcltk::tkfocus(name_entry), silent = TRUE)
  try(tcltk::tkraise(top), silent = TRUE)

  tcltk::tkwait.window(top)

  if (isTRUE(result$cancelled)) {
    stop("Credential entry cancelled.", call. = FALSE)
  }

  list(
    name = result$name,
    password = result$password
  )
}
