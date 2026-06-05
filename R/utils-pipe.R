#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @importFrom methods new show
#' @importFrom dplyr anti_join any_of case_when coalesce filter matches rename semi_join starts_with
#' @importFrom purrr imap_lgl keep
#' @importFrom stats complete.cases cor median na.omit quantile
#' @importFrom stringr str_c str_to_upper
#' @importFrom tidyr unnest
#' @importFrom utils head
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
