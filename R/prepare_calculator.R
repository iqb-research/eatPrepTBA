#' Prepare calculator configuration for booklet XML
#'
#' @param calculator Logical. Whether to include the calculator.
#' @param journal_lines Non-negative whole number of journal lines displayed
#'   in the calculator. Missing values are not allowed. Defaults to `0`.
#'
#' @return A list containing the calculator configuration or NULL if disabled.
#' @keywords internal
prepare_calculator <- function(calculator = FALSE,
                               journal_lines = 0L) {
  checkmate::assert_flag(calculator)

  if (!calculator) {
    return(NULL)
  }

  checkmate::assert_integerish(
    journal_lines,
    len = 1,
    lower = 0,
    any.missing = FALSE,
    .var.name = "journal_lines"
  )

  journal_lines <- as.integer(round(journal_lines))

  list(
    Widgets = list(
      Widget = list(
        id = "calc",
        model = "CALCULATOR",
        Parameter = list(
          as.character(journal_lines),
          key = "JOURNAL_LINES"
        )
      )
    )
  )
}
