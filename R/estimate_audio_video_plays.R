#' Estimate audio and video play counts from response JSONs
#'
#' @param response_df Tibble. Response data retrieved with [get_responses()] or
#'   read with [read_responses()]. Must contain `responses`, `group_id`,
#'   `login_name`, `login_code`, `booklet_id`, and `unit_key`. If `page_no` is
#'   present, it is retained in the output.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Extracts audio and video response elements from the `responses` JSON column.
#' Media elements are identified by an `id` value that marks the element as audio
#' or video. Malformed or missing response JSONs are ignored.
#'
#' @return A tibble with one row per participant, unit, page, and media element.
#'   It contains the session and unit columns, `media_id`, `media_type`,
#'   `status`, `n_plays`, and `media_key`. `media_key` is a namespaced compound
#'   key of the form `unit_key__media__media_type__media_id` and is intended for
#'   display or export, not as a replacement for the separate key columns.
#'
#' @export
estimate_audio_video_plays <- function(response_df) {
  checkmate::assert_tibble(response_df)
  assert_cols(
    response_df,
    c("responses", "group_id", "login_name", "login_code", "booklet_id", "unit_key"),
    "response_df"
  )

  if (!"page_no" %in% names(response_df)) {
    response_df$page_no <- NA_integer_
  }

  context_cols <- c("group_id", "login_name", "login_code", "booklet_id", "unit_key", "page_no")

  if (nrow(response_df) == 0L) {
    return(empty_audio_video_plays())
  }

  rows <- lapply(seq_len(nrow(response_df)), function(i) {
    response_media_rows(response_df[i, , drop = FALSE], context_cols)
  })

  out <- dplyr::bind_rows(rows)

  if (nrow(out) == 0L) {
    return(empty_audio_video_plays())
  }

  out
}

empty_audio_video_plays <- function() {
  tibble::tibble(
    group_id = character(),
    login_name = character(),
    login_code = character(),
    booklet_id = character(),
    unit_key = character(),
    page_no = integer(),
    media_id = character(),
    media_type = character(),
    status = character(),
    n_plays = numeric(),
    media_key = character()
  )
}

response_media_rows <- function(row, context_cols) {
  parsed <- parse_response_json_deep(row$responses[[1]])
  records <- collect_response_records(parsed)

  if (length(records) == 0L) {
    return(empty_audio_video_plays())
  }

  media <- dplyr::bind_rows(lapply(records, media_record_row))
  media <- media[!is.na(media$media_type), , drop = FALSE]

  if (nrow(media) == 0L) {
    return(empty_audio_video_plays())
  }

  context <- row[rep(1L, nrow(media)), context_cols, drop = FALSE]
  context$page_no <- as.integer(context$page_no)
  media$media_key <- media_key(context$unit_key, media$media_type, media$media_id)
  dplyr::bind_cols(context, media)
}

parse_response_json_deep <- function(cell, max_depth = 5L) {
  value <- cell

  if (is.null(value) || length(value) == 0L || all(is.na(value))) {
    return(NULL)
  }

  for (i in seq_len(max_depth)) {
    if (!is.character(value) || length(value) != 1L) {
      break
    }

    parsed <- tryCatch(
      jsonlite::fromJSON(value, simplifyVector = FALSE),
      error = function(e) NULL
    )

    if (is.null(parsed)) {
      return(NULL)
    }

    value <- parsed
  }

  value
}

collect_response_records <- function(value) {
  records <- list()

  collect <- function(x) {
    if (!is.list(x)) {
      return(NULL)
    }

    if (!is.null(x$id)) {
      records[[length(records) + 1L]] <<- x
      return(NULL)
    }

    for (element in x) {
      collect(element)
    }

    NULL
  }

  collect(value)
  records
}

media_record_row <- function(record) {
  media_id <- media_scalar_character(record$id)
  media_type <- media_type_from_id(media_id)
  n_plays <- media_scalar_numeric(record$value)

  if (is.na(n_plays) && !is.null(record$n_plays)) {
    n_plays <- media_scalar_numeric(record$n_plays)
  }

  tibble::tibble(
    media_id = media_id,
    media_type = media_type,
    status = media_scalar_character(record$status),
    n_plays = n_plays
  )
}

media_type_from_id <- function(media_id) {
  id_lower <- stringr::str_to_lower(media_id)

  dplyr::case_when(
    is.na(media_id) | media_id == "" ~ NA_character_,
    stringr::str_detect(id_lower, "(^|[^a-z0-9])audio([^a-z0-9]|$)|^audio|audio$") ~ "audio",
    stringr::str_detect(id_lower, "(^|[^a-z0-9])video([^a-z0-9]|$)|^video|video$") ~ "video",
    TRUE ~ NA_character_
  )
}

media_scalar_character <- function(x) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) {
    return(NA_character_)
  }

  as.character(x[[1]])
}

media_scalar_numeric <- function(x) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(x[[1]]))
}

media_key <- function(unit_key, media_type, media_id) {
  missing_key <- is.na(unit_key) | is.na(media_type) | is.na(media_id)
  key <- paste(unit_key, "media", media_type, media_id, sep = "__")
  key[missing_key] <- NA_character_
  key
}
