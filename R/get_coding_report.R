#' Get coding report
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve the coding report.
#'
#' @description
#' This function returns the coding report of the given IQB Studio workspace.
#' Current Studio Lite versions may provide `validationProblems` in addition to
#' the aggregate `validation` status. These details are returned as a
#' `validation_problems` list-column with one tibble per variable and the columns
#' `type`, `breaking`, and `code`.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_coding_report,WorkspaceStudio-method
setGeneric("get_coding_report", function(workspace) {
  cli_setting()

  standardGeneric("get_coding_report")
})

empty_validation_problems <- function() {
  tibble::tibble(
    type = character(),
    breaking = logical(),
    code = character()
  )
}

read_validation_problem <- function(problem) {
  if (is.null(problem) || length(problem) == 0 || all(is.na(problem))) {
    return(empty_validation_problems())
  }

  if (is.data.frame(problem)) {
    problem <- tibble::as_tibble(problem)
    problem_defaults <- list(
      type = NA_character_,
      breaking = NA,
      code = NA_character_
    )
    for (col in setdiff(names(problem_defaults), names(problem))) {
      problem[[col]] <- problem_defaults[[col]]
    }

    return(problem %>%
             dplyr::transmute(
               type = as.character(.data$type),
               breaking = as.logical(.data$breaking),
               code = as.character(.data$code)
             ))
  }

  tibble::tibble(
    type = read_scalar_character(purrr::pluck(problem, "type", .default = NA_character_)),
    breaking = read_scalar_logical(purrr::pluck(problem, "breaking", .default = NA)),
    code = read_scalar_character(purrr::pluck(problem, "code", .default = NA_character_))
  )
}

read_validation_problems <- function(problems) {
  if (is.null(problems) || length(problems) == 0 || all(is.na(problems))) {
    return(empty_validation_problems())
  }

  if (is.data.frame(problems)) {
    return(read_validation_problem(problems))
  }

  purrr::map(problems, read_validation_problem) %>%
    purrr::reduce(dplyr::bind_rows, .init = empty_validation_problems()) %>%
    dplyr::filter(!is.na(.data$type) | !is.na(.data$breaking) | !is.na(.data$code))
}

#' @describeIn get_coding_report Get the coding report for a Studio workspace.
setMethod("get_coding_report",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace) {
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            # units <-
            #   list_units(workspace) %>%
            #   purrr::map(function(ws) {
            #     ws$units %>%
            #       purrr::list_transpose() %>%
            #       tibble::as_tibble() %>%
            #       dplyr::mutate(ws_id = ws$ws_id, ws_label = ws$ws_label)
            #   }) %>%
            #   dplyr::bind_rows()

            run_req <- function(ws_id) {
              req <- function() {
                base_req(method = "GET",
                         endpoint = c(
                           "workspaces",
                           ws_id,
                           "units",
                           "scheme"
                         )) %>%
                  httr2::req_perform() %>%
                  httr2::resp_body_json() %>%
                  purrr::list_transpose() %>%
                  tibble::as_tibble()
              }

              return(req)
            }


            coding_reports <-
              purrr::map(ws_id,
                       function(ws_id) {
                         run_safe(run_req(ws_id),
                                  error_message = "Coding report could not be generated..",
                                  default = tibble::tibble())
                       }) %>%
              dplyr::bind_rows()

            if (!tibble::has_name(coding_reports, "validationProblems")) {
              coding_reports$validationProblems <- vector("list", nrow(coding_reports))
            }

            coding_reports %>%
              dplyr::mutate(
                validation_problems = purrr::map(.data$validationProblems, read_validation_problems)
              ) %>%
              tidyr::extract(
                unit, into = c("ws_id", "unit_id", "unit_key", "unit_label"),
                regex = "<a href=#/a/(\\d+)/(\\d+)>([^:]+): (.+)</a>"
              ) %>%
              dplyr::mutate(
                unit_id = as.integer(unit_id)
              ) %>%
              dplyr::select(
                ws_id,
                unit_id,
                unit_key,
                unit_label,
                variable_id = variable,
                item,
                validation,
                validation_problems,
                coding_type = codingType
              )
          })
