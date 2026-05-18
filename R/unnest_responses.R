unnest_responses <- function(json, is_parsed = TRUE) {
  if (!is_parsed) {
    json_parsed <-
      json %>%
      purrr::map(jsonlite::parse_json) %>%
      purrr::flatten()
  } else {
    json_parsed <-
      json
  }

  if (length(json_parsed) == 0) {
    return(tibble::tibble(
      id = "elementCodes",
      content = NA_character_
    ))
  }

  if ("lastSeenPageIndex" %in% purrr::map_chr(json_parsed, "id")) {
    return(tibble::tibble(
      id = "elementCodes",
      content = as.character(jsonlite::toJSON(json_parsed))
    ))
  } else {
    json_parsed %>%
      purrr::list_transpose() %>%
      tibble::as_tibble() %>%
      dplyr::select(
        dplyr::any_of(c(
          "id",
          "content",
          "ts"
        ))
      ) %>%
      # TODO: Check for robustness of this new fix
      # This should remove all the duplicated entries
      dplyr::group_by(id) %>%
      dplyr::distinct() %>%
      dplyr::filter(ts == max(ts)) %>%
      dplyr::ungroup()
  }
}
