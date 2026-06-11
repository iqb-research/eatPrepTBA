#' Get a unit with resources
#'
#' @param url JSON file for the Skohub metadata profile.
#'
#' @description
#' This function only returns the unit information and coding scheme for a single unit. To retrieve multiple units, use [get_units()].
#'
#' @return A tibble.
#'
#' @keywords internal
get_metadata_profile <- function(url) {
  # input validation
  checkmate::assert_character(url, len = 1)

  str_replacements <- "[-:/ \\*]+"
  str_removals <- "[\\(\\)]"

  profile <- jsonlite::read_json(url)

  profile_prepared <-
    profile %>%
    get_deepest_elements("entries") %>%
    purrr::list_flatten() %>%
    purrr::map(function(x) unlist(x) %>% as.list() %>% tibble::as_tibble()) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      profile_name = stringr::str_replace_all(label.value, str_replacements, "_") %>%
        stringr::str_remove_all(str_removals)
    )

  profile_read <-
    profile_prepared %>%
    dplyr::select(profile_name,
                  profile_id = id,
                  profile_label = label.value,
                  profile_url = parameters.url,
                  profile_type = type,
                  multiple = parameters.allowMultipleValues) %>%
    dplyr::mutate(
      profile = purrr::map(profile_url, prepare_profile_list)
    ) %>%
    tidyr::unnest(profile)

  # Merge notation
  if (tibble::has_name(profile_read, "notation")) {
    profile_read <-
      profile_read %>%
      dplyr::mutate(
        prefLabel.de = dplyr::case_when(
          !is.na(notation) ~ stringr::str_glue("{{{notation}}} {prefLabel.de}") %>% as.character(),
          .default = prefLabel.de
        )
      )
  }

  profile_read %>%
    dplyr::select(
      profile_name,
      profile_label,
      value_id = id,
      value_label = prefLabel.de,
      profile_type,
      multiple
    ) %>%
    tidyr::nest(
      data = -c(profile_name, profile_label, profile_type, multiple)
    ) %>%
    dplyr::mutate(
      data = purrr::map(data, function(x) x %>% dplyr::mutate(value_label = factor(value_label, levels = value_label))),
      multiple = ifelse(is.na(multiple), FALSE, multiple) %>% as.logical()
    )
}

narrow_profile_list <- function(x) {
  purrr::map(x, function(x) {
    out <-
      purrr::discard(x, names(x) == "narrower") %>%
      unlist() %>%
      tibble::enframe() %>%
      tidyr::pivot_wider()

    if ("narrower" %in% names(x)) {
      add_narrow <-
        x %>%
        purrr::pluck("narrower") %>%
        narrow_profile_list()

      out <- dplyr::bind_rows(out, add_narrow)
    }
    return(out)
  }) %>%
    purrr::reduce(dplyr::bind_rows)
}

prepare_profile_list <- function(url) {
  if (!is.na(url)) {
    httr2::request(url) %>%
      httr2::req_url_path_append("index.json") %>%
      httr2::req_perform() %>%
      httr2::resp_body_json() %>%
      purrr::pluck("hasTopConcept") %>%
      narrow_profile_list()
  } else {
    tibble::tibble(id = NA)
  }
}
