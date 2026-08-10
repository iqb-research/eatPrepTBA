str_replacements <- "[-:/ \\*]+"
str_removals <- "[\\(\\)]"

read_text_value <- function(texts, preferred_lang = "de") {
  if (is.null(texts) || length(texts) == 0) {
    return(NA_character_)
  }

  if (!is.list(texts)) {
    return(as.character(texts[[1]]))
  }

  if (!is.null(names(texts)) && "value" %in% names(texts)) {
    value <- purrr::pluck(texts, "value", .default = NA_character_)
    if (is.null(value) || length(value) == 0) {
      return(NA_character_)
    }

    return(as.character(value[[1]]))
  }

  values <- purrr::map_chr(texts, function(x) {
    value <- purrr::pluck(x, "value", .default = NA_character_)
    if (is.null(value) || length(value) == 0) {
      NA_character_
    } else {
      as.character(value[[1]])
    }
  })

  langs <- purrr::map_chr(texts, function(x) {
    lang <- purrr::pluck(x, "lang", .default = NA_character_)
    if (is.null(lang) || length(lang) == 0) {
      NA_character_
    } else {
      as.character(lang[[1]])
    }
  })

  lang_match <- which(langs == preferred_lang & !is.na(values))
  if (length(lang_match) > 0) {
    return(values[[lang_match[[1]]]])
  }

  first_value <- which(!is.na(values))
  if (length(first_value) > 0) {
    return(values[[first_value[[1]]]])
  }

  NA_character_
}

read_scalar_character <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  as.character(x[[1]])
}

read_scalar_logical <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA)
  }

  as.logical(x[[1]])
}

read_scalar_integer <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_integer_)
  }

  suppressWarnings(as.integer(x[[1]]))
}

coalesce_character <- function(...) {
  values <- list(...)
  for (value in values) {
    if (!is.null(value) && length(value) > 0 && !is.na(value[[1]])) {
      return(as.character(value[[1]]))
    }
  }

  NA_character_
}

clean_profile_name <- function(label, id = NA_character_) {
  name <- coalesce_character(read_text_value(label), read_scalar_character(id))

  if (is.na(name)) {
    return(NA_character_)
  }

  stringr::str_replace_all(name, str_replacements, "_") %>%
    stringr::str_remove_all(str_removals)
}

is_simple_metadata_value <- function(value) {
  is.list(value) &&
    !is.null(names(value)) &&
    any(c("raw", "asText") %in% names(value))
}

is_language_coded_texts <- function(value) {
  is.list(value) &&
    length(value) > 0 &&
    is.null(names(value)) &&
    all(purrr::map_lgl(value, function(x) {
      is.list(x) && "value" %in% names(x) && !("id" %in% names(x))
    }))
}

is_vocabulary_entries <- function(value) {
  is.list(value) &&
    length(value) > 0 &&
    is.null(names(value)) &&
    all(purrr::map_lgl(value, function(x) is.list(x) && "id" %in% names(x)))
}

read_entry_values <- function(entry) {
  value <- purrr::pluck(entry, "value", .default = NULL)
  value_as_text <- read_text_value(purrr::pluck(entry, "valueAsText", .default = NULL))

  if (is.null(value) || length(value) == 0) {
    return(tibble::tibble(
      value_id = NA_character_,
      value_text = value_as_text
    ))
  }

  if (is_simple_metadata_value(value)) {
    return(tibble::tibble(
      value_id = NA_character_,
      value_text = coalesce_character(
        value_as_text,
        read_text_value(purrr::pluck(value, "asText", .default = NULL)),
        read_scalar_character(purrr::pluck(value, "raw", .default = NA_character_))
      )
    ))
  }

  if (is_vocabulary_entries(value)) {
    return(purrr::map_dfr(value, function(x) {
      tibble::tibble(
        value_id = read_scalar_character(purrr::pluck(x, "id", .default = NA_character_)),
        value_text = coalesce_character(
          value_as_text,
          read_text_value(purrr::pluck(x, "annotation", .default = NULL)),
          read_text_value(purrr::pluck(x, "label", .default = NULL))
        )
      )
    }))
  }

  if (is_language_coded_texts(value)) {
    return(tibble::tibble(
      value_id = NA_character_,
      value_text = coalesce_character(value_as_text, read_text_value(value))
    ))
  }

  tibble::tibble(
    value_id = NA_character_,
    value_text = coalesce_character(value_as_text, read_scalar_character(value))
  )
}

empty_unit_profiles <- function() {
  tibble::tibble(
    profile_id = NA_character_,
    profile_is_current = NA,
    profile_order = NA_integer_,
    profile_name = NA_character_,
    value_id = NA_character_,
    value_text = NA_character_
  )
}

empty_items_profiles <- function() {
  tibble::tibble(
    item_no = NA_integer_,
    profile_id = NA_character_,
    profile_is_current = NA,
    profile_order = NA_integer_,
    profile_name = NA_character_,
    value_id = NA_character_,
    value_text = ""
  )
}

read_profile_entries <- function(profile) {
  entries <- purrr::pluck(profile, "entries", .default = list())

  if (is.null(entries) || length(entries) == 0) {
    return(tibble::tibble())
  }

  profile_id <- purrr::pluck(profile, "profileId", .default = NA_character_)
  profile_is_current <- purrr::pluck(profile, "isCurrent", .default = NA)
  profile_order <- purrr::pluck(profile, "order", .default = NA_integer_)

  if (is.null(profile_id) || length(profile_id) == 0) {
    profile_id <- NA_character_
  }
  if (is.null(profile_is_current) || length(profile_is_current) == 0) {
    profile_is_current <- NA
  }
  if (is.null(profile_order) || length(profile_order) == 0) {
    profile_order <- NA_integer_
  }

  entries %>%
    purrr::map(function(x) {
      read_entry_values(x) %>%
        dplyr::mutate(
          profile_name = clean_profile_name(
            purrr::pluck(x, "label", .default = NULL),
            purrr::pluck(x, "id", .default = NA_character_)
          ),
          .before = "value_id"
        )
    }) %>%
    purrr::reduce(dplyr::bind_rows, .init = tibble::tibble()) %>%
    dplyr::mutate(
      profile_id = as.character(profile_id[[1]]),
      profile_is_current = read_scalar_logical(profile_is_current),
      profile_order = read_scalar_integer(profile_order),
      .before = "profile_name"
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
      purrr::map(read_profile_entries) %>%
      purrr::reduce(dplyr::bind_rows, .init = tibble::tibble())

    if (nrow(unit_profiles_prep) == 0) {
      return(empty_unit_profiles())
    }

    unit_profiles <- unit_profiles_prep
  } else {
    unit_profiles <- empty_unit_profiles()
  }

  return(unit_profiles)
}

read_items_profiles <- function(unit_metadata) {
  items_profiles <- empty_items_profiles()

  if (is.null(unit_metadata$items) || length(purrr::compact(unit_metadata$items)) == 0) {
    return(items_profiles)
  }

  items_profiles_prep <-
    purrr::map2(unit_metadata$items, seq_along(unit_metadata$items), function(x, item) {
      profiles <- purrr::pluck(x, "profiles", .default = NULL)
      if (is.null(profiles) || length(purrr::compact(profiles)) == 0) {
        profiles <- purrr::pluck(x, "metadata", .default = NULL)
      }

      profile_rows <- profiles %>%
        purrr::map(read_profile_entries) %>%
        purrr::reduce(dplyr::bind_rows, .init = tibble::tibble())

      if (nrow(profile_rows) == 0) {
        return(profile_rows)
      }

      profile_rows %>%
        dplyr::mutate(item_no = as.integer(item), .before = "profile_id")
    }) %>%
    purrr::reduce(dplyr::bind_rows, .init = tibble::tibble())

  if (nrow(items_profiles_prep) == 0) {
    return(items_profiles)
  }

  items_profiles <- items_profiles_prep

  return(items_profiles)
}

read_item_character_column <- function(data, cols) {
  present_cols <- intersect(cols, names(data))
  if (length(present_cols) == 0) {
    return(rep(NA_character_, nrow(data)))
  }

  values <- purrr::map(present_cols, function(col) {
    x <- data[[col]]
    if (is.list(x) && !is.data.frame(x)) {
      return(purrr::map_chr(x, read_scalar_character))
    }

    as.character(x)
  })

  dplyr::coalesce(!!! values)
}

read_item_integer_column <- function(data, cols) {
  suppressWarnings(as.integer(read_item_character_column(data, cols)))
}

read_items_list <- function(unit_metadata) {
  if (!is.null(unit_metadata$items) && length(purrr::compact(unit_metadata$items)) != 0) {
    items_raw <-
      unit_metadata$items %>%
      purrr::map(function(x) {
        purrr::discard(x, .p = names(x) %in% c("profiles", "metadata")) %>%
          purrr::map(function(x) if (is.null(x)) NA else x) %>%
          tibble::as_tibble()
      }) %>%
      tibble::enframe(name = "item_no") %>%
      dplyr::mutate(item_no = dplyr::row_number()) %>%
      tidyr::unnest(value)

    items_list <- tibble::tibble(
      item_no = as.integer(items_raw$item_no),
      item_id = read_item_character_column(items_raw, "id"),
      variable_id = read_item_character_column(items_raw, c("variableId", "sourceVariableId")),
      variable_ref = read_item_character_column(items_raw, c("variableReadOnlyId", "sourceVariableUuid"))
    )

    optional_item_columns <- list(
      item_uuid = read_item_character_column(items_raw, "uuid"),
      item_order = read_item_integer_column(items_raw, "order"),
      item_description = read_item_character_column(items_raw, "description"),
      item_created = read_item_character_column(items_raw, "createdAt"),
      item_changed = read_item_character_column(items_raw, "changedAt")
    )

    for (col in names(optional_item_columns)) {
      values <- optional_item_columns[[col]]
      if (any(!is.na(values))) {
        items_list[[col]] <- values
      }
    }

    items_list <- items_list %>%
      dplyr::select(dplyr::any_of(c(
        "item_uuid",
        "item_no",
        "item_id",
        "variable_id",
        "variable_ref",
        "item_order",
        "item_description",
        "item_created",
        "item_changed"
      )))

    if (nrow(items_list) == 0) {
      items_list <-
        tibble::tibble(
          item_no = NA_integer_,
          item_id = NA_character_,
          variable_id = NA_character_
        )
    }
  } else {
    items_list <-
      tibble::tibble(
        item_no = NA_integer_,
        item_id = NA_character_,
        variable_id = NA_character_
      )
  }

  return(items_list)
}
