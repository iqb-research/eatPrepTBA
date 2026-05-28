str_replacements <- "[-:/ \\*]+"
str_removals <- "[\\(\\)]"

read_profile_entries <- function(profile, collapse) {
  entries <- purrr::pluck(profile, "entries", .default = list())

  if (is.null(entries) || length(entries) == 0) {
    return(tibble::tibble())
  }

  profile_id <- purrr::pluck(profile, "profileId", .default = NA_character_)
  profile_is_current <- purrr::pluck(profile, "isCurrent", .default = NA)

  if (is.null(profile_id) || length(profile_id) == 0) {
    profile_id <- NA_character_
  }
  if (is.null(profile_is_current) || length(profile_is_current) == 0) {
    profile_is_current <- NA
  }

  entries %>%
    purrr::map(function(x) {
      unlist(x) %>%
        as.list() %>%
        tibble::enframe() %>%
        tidyr::unnest(value) %>%
        tidyr::pivot_wider(values_fn = function(x) stringr::str_c(x, collapse = collapse))
    }) %>%
    purrr::reduce(dplyr::bind_rows, .init = tibble::tibble()) %>%
    dplyr::mutate(
      profile_id = as.character(profile_id[[1]]),
      profile_is_current = as.logical(profile_is_current[[1]])
    )
}

prepare_metadata <- function(unit_metadata) {
  # Unit metadata
  unit_profiles <- read_unit_profiles(unit_metadata)
  items_profiles <- read_items_profiles(unit_metadata)
  items_list <- read_items_list(unit_metadata)

  tibble::tibble(
    unit_profiles = list(unit_profiles),
    items_list = list(items_list),
    items_profiles = list(items_profiles),
  )
}

read_unit_profiles <- function(unit_metadata) {
  if (!is.null(unit_metadata$profiles) && length(unit_metadata$profiles) > 0) {
    unit_profiles_prep <-
      unit_metadata$profiles %>%
      purrr::map(read_profile_entries, collapse = ";.;.;") %>%
      purrr::reduce(dplyr::bind_rows, .init = tibble::tibble())

    value_id_exists <- tibble::has_name(unit_profiles_prep, "value.id")
    value_text_exists <- tibble::has_name(unit_profiles_prep, "valueAsText.value")

    unit_profiles <-
      unit_profiles_prep %>%
      dplyr::mutate(
        value.id = if (value_id_exists) value.id else NA,
        valueAsText.value = if (value_text_exists) valueAsText.value else NA,
        dplyr::across(dplyr::any_of(c("value.id", "valueAsText.value")),
                      function(x) stringr::str_split(x, ";.;.;")),
        dplyr::across(dplyr::any_of(c("label.value")),
                      function(x) stringr::str_replace_all(x, str_replacements, "_") %>%
                        stringr::str_remove_all(str_removals))
      ) %>%
      tidyr::unnest(c(value.id, valueAsText.value)) %>%
      dplyr::select(
        dplyr::any_of(
          c(
            profile_id = "profile_id",
            profile_is_current = "profile_is_current",
            profile_name = "label.value",
            value_id = "value.id",
            value_text = "valueAsText.value"
          )
        )
      )
  } else {
    unit_profiles <-
      tibble::tibble(
        profile_name = NA,
        value_id = NA,
        value_text = NA
      )
  }

  return(unit_profiles)
}

read_items_profiles <- function(unit_metadata) {
  items_profiles <-
    tibble::tibble(
      item_no = NA_integer_,
      profile_id = NA_character_,
      profile_is_current = NA,
      profile_name = NA_character_,
      # profile_label = label.value,
      value_id = NA_character_,
      value_text = ""
    )

  if (is.null(unit_metadata$items) || length(purrr::compact(unit_metadata$items)) == 0) {
    return(items_profiles)
  }

  items_profiles_prep <-
    unit_metadata$items %>%
    purrr::map(function(x) {
      x %>%
        purrr::pluck("profiles") %>%
        purrr::map(read_profile_entries, collapse = "_-_-_") %>%
        purrr::reduce(dplyr::bind_rows, .init = tibble::tibble())
    }) %>%
    tibble::enframe(name = "item") %>%
    tidyr::unnest(value)


  if (nrow(items_profiles_prep) == 0) {
    return(items_profiles)
  }

  items_profiles <-
    items_profiles_prep %>%
    dplyr::select(dplyr::any_of(c("item", "profile_id", "profile_is_current", "label.value", "value.id", "valueAsText.value"))) %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("value.id", "valueAsText.value")),
                    function(x) stringr::str_split(x, "_-_-_"))
    ) %>%
    tidyr::unnest(dplyr::any_of(c("value.id", "valueAsText.value"))) %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of("label.value"),
                    function(x) {
                      stringr::str_replace_all(x, str_replacements, "_") %>%
                                      stringr::str_remove_all(str_removals)

                    },
                    .names = "profile_name")
    ) %>%
    dplyr::select(
      dplyr::any_of(
        c(
          item_no = "item",
          profile_id = "profile_id",
          profile_is_current = "profile_is_current",
          profile_name = "profile_name",
          # profile_label = label.value,
          value_id = "value.id",
          value_text = "valueAsText.value"
        )
      )
    )

  return(items_profiles)
}

read_items_list <- function(unit_metadata) {
  if (!is.null(unit_metadata$items) && length(purrr::compact(unit_metadata$items)) != 0) {
    items_list <-
      unit_metadata$items %>%
      purrr::map(function(x) {
        purrr::discard(x, .p = names(x) == "profiles") %>%
          purrr::map(function(x) if (is.null(x)) NA else x) %>%
          tibble::as_tibble()
      }) %>%
      tibble::enframe(name = "item") %>%
      tidyr::unnest(value) %>%
      dplyr::select(any_of(c(
        item_uuid = "uuid",
        item_no = "item",
        item_id = "id",
        variable_id = "variableId",
        variable_ref = "variableReadOnlyId",
        item_order = "order",
        item_locked = "locked",
        item_position = "position",
        item_weighting = "weighting",
        item_created = "createdAt",
        item_changed = "changedAt"
      )))

    if (tibble::has_name(items_list, "description")) {
      items_list <-
        items_list %>%
        dplyr::rename(
          item_description = description
        )
    }

    if (nrow(items_list) == 0) {
      items_list <-
        tibble::tibble(
          item_id = NA_character_,
          variable_id = NA_character_
        )
    }
  } else {
    items_list <-
      tibble::tibble(
        item_id = NA_character_,
        variable_id = NA_character_
      )
  }

  return(items_list)
}
