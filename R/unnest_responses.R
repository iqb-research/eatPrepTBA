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

  json_ids <- response_json_ids(json_parsed)

  if ("geometryVariableCodes" %in% json_ids) {
    json_parsed <- merge_geometry_variable_codes(json_parsed, json_ids)
    json_ids <- response_json_ids(json_parsed)
  }

  if (length(json_parsed) == 0) {
    return(tibble::tibble(
      id = "elementCodes",
      content = NA_character_
    ))
  }

  if ("lastSeenPageIndex" %in% json_ids) {
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

response_json_ids <- function(json_parsed) {
  if (length(json_parsed) == 0) {
    return(character())
  }

  vapply(json_parsed, function(x) {
    id <- x[["id"]]

    if (is.null(id) || length(id) == 0) {
      NA_character_
    } else {
      as.character(id[[1]])
    }
  }, character(1), USE.NAMES = FALSE)
}

merge_geometry_variable_codes <- function(json_parsed, ids = response_json_ids(json_parsed)) {
  geometry_idx <- which(ids == "geometryVariableCodes")

  if (length(geometry_idx) == 0) {
    return(json_parsed)
  }

  element_idx <- which(ids == "elementCodes")

  collect_content <- function(idx) {
    content <- lapply(json_parsed[idx], `[[`, "content")
    content <- content[!vapply(content, is.null, logical(1))]

    unlist(content, recursive = FALSE)
  }

  geometry_content <- collect_content(geometry_idx)
  element_content <- collect_content(element_idx)
  merged_content <- c(element_content, geometry_content)

  if (length(element_idx) > 0) {
    keep_idx <- setdiff(seq_along(json_parsed), c(element_idx[-1], geometry_idx))
    json_merged <- json_parsed[keep_idx]
    ids_merged <- ids[keep_idx]
    first_element_idx <- which(ids_merged == "elementCodes")[1]

    json_merged[[first_element_idx]][["content"]] <- merged_content
  } else {
    json_merged <- json_parsed[-geometry_idx]
    element_entry <- json_parsed[[geometry_idx[1]]]
    element_entry[["id"]] <- "elementCodes"
    element_entry[["content"]] <- merged_content
    json_merged <- c(list(element_entry), json_merged)
  }

  json_merged
}
