#' Add a comment for a unit
#'
#' @description
#' This function adds an entry in the comment tab of the Studio of a single unit.
#'
#' @param login [LoginStudio-class]. Login information necessary to add comments.
#' @param ws_id Workspace id of the unit.
#' @param unit_id Unit id for the comment to add.
#' @param parent_id Comment id in case of responding to another comment.
#' @param comment Comment to be added to the unit. Should contain html markup.
#'
#' @return NULL
#'
#' @aliases
#' add_comment,LoginStudio-method
#'
#' @keywords internal
setGeneric("add_comment", function(login,
                                   ws_id,
                                   unit_id,
                                   parent_id = NULL,
                                   comment = NULL) {
  cli_setting()

  standardGeneric("add_comment")
})


#' @describeIn add_comment Add a comment in a defined workspace
setMethod("add_comment",
          signature = signature(login = "LoginStudio"),
          function(login,
                   ws_id,
                   unit_id,
                   parent_id = NULL,
                   comment = NULL) {
            base_req <- login@base_req

            user_id <- login@user_id
            user_label <- login@user_label

            body <-
              list(
                body = comment,
                userName = user_label,
                userId = user_id,
                parentId = parent_id,
                unitId = unit_id
              )

            run_req <- function() {
              base_req(method = "POST",
                       endpoint = c("workspaces", ws_id, "units", unit_id, "comments")) %>%
                httr2::req_body_json(data = body, auto_unbox = TRUE) %>%
                httr2::req_perform()
            }

            # For the error message
            url <- glue::glue("https://www.iqb-studio.de/#/a/{ws_id}/{unit_id}/properties")

            run_safe(run_req,
                     error_message = "Comment could not be added for
                       unit with id {unit_id} {.url {url}}.")

          })
