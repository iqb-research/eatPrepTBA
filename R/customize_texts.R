customize_texts <- function(...) {
  args <- rlang::list2(...)

  args %>%
    purrr::imap(function(x, n) {
      list(
        list(x),
        key = n
      )
    }) %>%
    purrr::set_names("CustomText")
}

prepare_custom_texts <- function(custom_texts, arg = "custom_texts") {
  checkmate::assert_list(custom_texts, null.ok = TRUE)
  validate_custom_texts(custom_texts, arg = arg)

  if (is.null(custom_texts) || length(custom_texts) == 0L) {
    return(NULL)
  }

  rlang::exec("customize_texts", !!!custom_texts)
}

validate_custom_texts <- function(custom_texts, arg = "custom_texts") {
  if (is.null(custom_texts) || length(custom_texts) == 0L) {
    return(invisible(NULL))
  }

  custom_text_names <- names(custom_texts)
  if (is.null(custom_text_names) ||
      any(is.na(custom_text_names) | custom_text_names == "")) {
    cli::cli_abort(
      "{.arg {arg}} must be a named list, for example {.code list(AppTitle = \"Pilot\")}."
    )
  }

  invisible(NULL)
}
