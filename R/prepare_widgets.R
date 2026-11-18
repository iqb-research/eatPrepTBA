#' Prepare widget configurations for booklet XML
#'
#' @param widgets Optional character vector specifying the widgets to include
#'   in the booklet. Possible values are `"calculator"`, `"molecule"` and
#'   `"periodic"`.
#' @param journal_lines Optional numeric value. Number of journal lines displayed in the
#'   calculator widget. Defaults to `0`.
#' @param arg Character. Name of the argument used in error messages.
#'
#' @return A list containing the widget configurations or NULL if no widgets
#'   are specified.

prepare_widgets <- function(widgets,
                            journal_lines = 0L,
                            arg = "widgets") {

  # Check whether widgets is a character vector
  checkmate::assert_character(widgets, null.ok = TRUE)

  # Check whether the selected widgets are valid and not duplicated
  validate_widgets(widgets, arg = arg)


  # Return NULL if no widgets have been selected
  if (is.null(widgets) || length(widgets) == 0L) {
    return(NULL)
  }

  # Identify widgets for which no XML configuration has been implemented yet
  not_implemented <- setdiff(
    widgets,
    "calculator"
  )

  # Stop execution if an unimplemented widget has been selected
  if (length(not_implemented) > 0L) {
    cli::cli_abort(
      "{.arg {arg}}: Configuration for {.val {not_implemented}} is not implemented yet."
    )
  }

  # Create the XML configuration for each selected widget
  widgets %>%
    purrr::map(function(x) {
      switch(
        x,

        # Configure the calculator widget
        calculator = {

          # Validate the number of journal lines
          checkmate::assert_integerish(
            journal_lines,
            len = 1,
            lower = 0,
            .var.name = "journal_lines"
          )

          # Create the calculator XML structure
          list(
            id = "calc",
            model = "CALCULATOR",
            Parameter = list(
              as.character(journal_lines),
              key = "JOURNAL_LINES"
            )
          )
        }
      )
    }) %>%
    # Assign the same XML node name to all selected widgets.
    # This allows multiple <Widget> elements within <Widgets>.
  purrr::set_names(rep("Widget", length(widgets))) %>%
  list(Widgets = .)

}


#' Validate selected widgets
#'
#' @param widgets Character vector. Names of widgets to validate.
#' @param arg Character. Name of the argument used in error messages.
#'
#' @return Invisibly returns NULL if validation succeeds.
validate_widgets <- function(widgets, arg = "widgets") {

  # No validation is necessary if no widgets have been specified
  if (is.null(widgets) || length(widgets) == 0L) {
    return(invisible(NULL))
  }

  # Define all supported widget names
  allowed_widgets <- c("calculator", "molecule", "periodic")

  # Check that each widget is selected only once
  if (anyDuplicated(widgets)) {
    cli::cli_abort(
      "{.arg {arg}} must not contain duplicate widgets."
    )
  }

  # Check whether all selected widgets are supported
  if (!all(widgets %in% allowed_widgets)) {

    # Identify widget names that are not part of the allowed selection
    invalid_widgets <- setdiff(widgets, allowed_widgets)

    # Stop execution and report the invalid widget names
    cli::cli_abort(
      "{.arg {arg}} contains invalid widget(s): {.val {invalid_widgets}}.
      Allowed values are {.val {allowed_widgets}}."
    )
  }

  invisible(NULL)
}
