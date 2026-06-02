#' Generate API request function
#'
#' @param type Character. Type of the URL request, e.g., `GET` or `POST`.
#' @param base_url Character. Base URL of the instance.
#' @param auth_token Character. Token to interact with the instance API.
#' @param app_version Character. Version of the IQB Studio Lite (not necessary for the IQB Testcenter).
#' @param insecure Logical. Should the https security certificate be ignored (only recommended for Intranet requests that might not have a valid security certificate).
#'
#' @description
#' This function returns the base API request for a an instance.
#'
#' @return A `function` with arguments `method`, `endpoint`, and `query`.
#'
#' @keywords internal
generate_base_req <- function(type,
                              base_url,
                              auth_token,
                              app_version = NULL,
                              insecure = FALSE) {
  # input validation
  checkmate::assert_choice(type, c("studio", "testcenter"))
  checkmate::assert_character(base_url, len = 1)
  checkmate::assert_character(auth_token, len = 1)
  checkmate::assert_character(app_version, null.ok = TRUE)
  checkmate::assert_logical(insecure, len = 1)

  if (type == "studio") {
    base_call <-
      httr2::request(base_url = base_url) %>%
      httr2::req_headers("app-version" = app_version,
                         Authorization = auth_token
      )
  } else if (type == "testcenter") {
    base_call <-
      httr2::request(base_url = base_url) %>%
      httr2::req_headers(AuthToken = auth_token)

    if (insecure) {
      # Added for Intranet requests
      base_call <-
        base_call %>%
        httr2::req_options(
          ssl_verifypeer = FALSE,
          ssl_verifyhost = FALSE
        )
    }
  }

  function(method, endpoint, query = NULL) {
    base_call %>%
      httr2::req_method(method) %>%
      httr2::req_url_path_append("api", endpoint) %>%
      httr2::req_url_query(!!! query, .multi = "comma")
  }
}
