#' Get reviews
#'
#' @param workspace [WorkspaceTestcenter-class]. IQB Testcenter workspace information necessary to retrieve reviews from the API.
#' @param groups Character. Name of the groups to be retrieved or all groups if not specified.
#' @param use_new_version Logical. Should the new or the output format be used. Defaults to `TRUE`.
#'
#' @description
#' This function returns responses for the selected groups.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_reviews,WorkspaceTestcenter-method
setGeneric("get_reviews", function(workspace, groups = NULL, use_new_version = TRUE) {
  standardGeneric("get_reviews")
})

#' @describeIn get_reviews Get responses of a given Testcenter workspace
setMethod("get_reviews",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace, groups = NULL, use_new_version = TRUE) {
            if (is.null(groups)) {
              groups <- get_results(workspace)$groupName
            }

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            # TODO: Loop, but no safe-run by now
            run_req <- function() {
              base_req(method = "GET",
                       endpoint = c("workspace", ws_id, "report", "review"),
                       query = list(dataIds = groups,
                                    useNewVersion = tolower(use_new_version))) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            resp <-
              run_safe(run_req,
                       error_message = "Reviews could not be retrieved.")

            if (!is.null(resp)) {
              resp_table <-
                resp %>%
                purrr::list_transpose() %>%
                tibble::as_tibble()

              if (use_new_version) {
                # New format
                resp_table <-
                  resp_table %>%
                  dplyr::rename(
                    dplyr::any_of(c(
                      content = "category_content",
                      design = "category_design",
                      tech = "category_tech"
                    ))
                  )
              } else {
                # Old format
                resp_table <-
                  resp_table %>%
                  dplyr::rename(
                    dplyr::any_of(c(
                      content = "category_content",
                      design = "category_design",
                      tech = "category_tech"
                    ))
                  )
              }

              if (!all(c("content", "design", "tech") %in% names(resp_table))) {
                new_cols <- setdiff(c("content", "design", "tech"), names(resp_table))

                for (col in new_cols) {
                  resp_table <-
                    resp_table %>%
                    tibble::add_column(!!col := FALSE)
                }
              }

              resp_table %>%
                tidyr::unnest(dplyr::any_of(c("content", "design", "tech")),
                              keep_empty = TRUE) %>%
                dplyr::rename(any_of(c(
                  group_id = "groupname",
                  login_name = "loginname",
                  login_code = "code",
                  booklet_id = "bookletname",
                  unit_key = "originalUnitId",
                  unit_alias = "unitname",
                  priority = "priority",
                  content = "content",
                  design = "design",
                  tech = "tech",
                  review_time = "review_time",
                  review_comment = "review_comment",
                  user_agent = "userAgent",
                  page = "page",
                  page_label = "pagelabel"
                ))) %>%
                dplyr::relocate(dplyr::all_of(
                  c("content", "design", "tech")
                ), .before = "reviewtime"
                )
            } else {
              tibble::tibble()
            }
          })
