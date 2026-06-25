#' Unpack response JSON columns
#'
#' @param responses Data frame. Response data with one or more JSON columns.
#' @param response_cols Character vector. Columns to unpack. If `NULL`, columns
#'   are auto-detected by parsing non-empty JSON payloads that contain response
#'   record fields such as `id`, `status`, `value`, `subform`, `code`, and
#'   `score`.
#' @param id_cols Character vector. Columns to keep as identifiers. If `NULL`,
#'   all non-JSON and non-timestamp columns are kept.
#' @param keep_empty Logical. Whether missing, empty, and `[]` payloads should
#'   be kept as rows with missing response identifiers.
#' @param progress Logical. Whether to show a progress bar while parsing JSON
#'   payloads.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function unpacks response JSON payloads that are distributed across
#' several columns, for example `question_0_content`, `sums_content`, or
#' `responses`. Matching timestamp columns are detected by replacing a
#' `_content` suffix with `_ts`, for example `question_0_content` is matched to
#' `question_0_ts`. This is particularly useful for BKT-like question-slot
#' preparation where coded responses are distributed across `question_*_content`
#' columns instead of one `coded` column.
#'
#' @return A tibble with the identifier columns, response slot metadata, and the
#'   parsed JSON fields.
#'
#' @export
unpack_response_jsons <- function(responses,
                                  response_cols = NULL,
                                  id_cols = NULL,
                                  keep_empty = FALSE,
                                  progress = TRUE) {
  checkmate::assert_data_frame(responses)
  checkmate::assert_logical(keep_empty, len = 1)
  checkmate::assert_logical(progress, len = 1)
  responses <- tibble::as_tibble(responses)

  if (is.null(response_cols)) {
    response_cols <- detect_response_json_cols(responses)
  } else {
    checkmate::assert_character(response_cols, min.len = 1, any.missing = FALSE)
    assert_cols(responses, response_cols, "responses")
  }

  timestamp_cols <- intersect(response_json_ts_col(response_cols), names(responses))

  if (is.null(id_cols)) {
    id_cols <- setdiff(names(responses), c(response_cols, timestamp_cols))
  } else {
    checkmate::assert_character(id_cols, any.missing = FALSE)
    assert_cols(responses, id_cols, "responses")
  }

  if (length(response_cols) == 0L) {
    return(empty_unpacked_response_jsons(responses, id_cols))
  }

  responses_indexed <-
    responses %>%
    dplyr::mutate(response_row = dplyr::row_number())

  json_long <-
    responses_indexed %>%
    dplyr::select(
      dplyr::all_of(c("response_row", id_cols)),
      dplyr::all_of(response_cols)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(response_cols),
      names_to = "response_column",
      values_to = "response_json"
    )

  ts_long <- response_json_ts_long(responses_indexed, response_cols, timestamp_cols)

  json_long %>%
    dplyr::left_join(
      ts_long,
      by = dplyr::join_by("response_row", "response_column")
    ) %>%
    dplyr::filter(
      keep_empty | response_json_is_non_empty(response_json)
    ) %>%
    dplyr::mutate(
      response_name = response_json_base(response_column),
      question_index = response_question_index(response_name),
      parsed = purrr::map(
        response_json,
        parse_response_json_cell,
        keep_empty = keep_empty,
        .progress = unpack_response_json_progress(progress)
      )
    ) %>%
    dplyr::select(-response_json) %>%
    tidyr::unnest(parsed) %>%
    standardize_unpacked_response_jsons(id_cols)
}

#' Prepare unpacked response codes
#'
#' @param unpacked Data frame. Output from [unpack_response_jsons()].
#' @param response_id Character vector. Parsed response identifiers to keep.
#'   Defaults to `"value"`, which is the code-bearing entry in BKT question
#'   slots.
#' @param variable_id_from Character. Source used to derive `variable_id`.
#'   Defaults to `subform`, with fallback to `response_name` and then
#'   `response_id`.
#' @param keep_uncoded Logical. Whether rows without `code_id` and `code_score`
#'   should be retained.
#' @param unnest_value Logical. Whether to unnest the `value` list-column for
#'   compatibility with `code_responses(..., prepare = TRUE)`.
#' @param code_type Character. Default `code_type` for prepared rows when the
#'   unpacked data do not already contain `code_type`.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This helper converts unpacked code-bearing response records into the same core
#' column shape returned by `code_responses(..., prepare = TRUE)`: `variable_id`,
#' `value`, `code_id`, `code_score`, `code_status`, and `code_type`. By default,
#' `value` is unnested so prepared rows can be bound with prepared
#' `code_responses()` output before calling `complete_design()`.
#'
#' @return A tibble.
#'
#' @export
prepare_unpacked_codes <- function(unpacked,
                                   response_id = "value",
                                   variable_id_from = c("subform", "response_name", "response_id"),
                                   keep_uncoded = FALSE,
                                   unnest_value = TRUE,
                                   code_type = NA_character_) {
  checkmate::assert_data_frame(unpacked)
  checkmate::assert_character(response_id, min.len = 1, any.missing = FALSE)
  checkmate::assert_logical(keep_uncoded, len = 1)
  checkmate::assert_logical(unnest_value, len = 1)
  checkmate::assert_character(code_type, len = 1, any.missing = TRUE)
  variable_id_from <- match.arg(variable_id_from)
  unpacked <- tibble::as_tibble(unpacked)

  assert_cols(
    unpacked,
    c("response_id", "response_status", "value", "code_id", "code_score"),
    "unpacked"
  )

  out <- unpacked[unpacked$response_id %in% response_id, , drop = FALSE]

  if (!keep_uncoded) {
    out <- out[!is.na(out$code_id) | !is.na(out$code_score), , drop = FALSE]
  }

  out$variable_id <- derive_unpacked_variable_id(out, variable_id_from)
  out$code_status <- out$response_status
  out <- complete_unpacked_code_type(out, code_type)

  if (unnest_value) {
    out <- tidyr::unnest(out, value, keep_empty = TRUE)
  }

  code_cols <- c(
    "response_row", "unit_key", "unit_alias", "group_id", "login_name",
    "login_code", "booklet_id", "source", "server", "file",
    "response_column", "response_name", "response_ts", "question_index",
    "subform", "variable_id", "value", "code_id", "code_score",
    "code_status", "code_type", "response_id", "response_status"
  )

  out %>%
    dplyr::select(dplyr::any_of(code_cols), dplyr::everything())
}

complete_unpacked_code_type <- function(out, code_type) {
  if (!"code_type" %in% names(out)) {
    out$code_type <- rep(code_type, nrow(out))
    return(out)
  }

  out$code_type <- dplyr::coalesce(as.character(out$code_type), rep(code_type, nrow(out)))
  out
}

unpack_response_json_progress <- function(progress) {
  if (!progress) {
    return(FALSE)
  }

  list(
    type = "custom",
    show_after = 0,
    format = paste(
      "Unpacking response JSON payloads ({cli::pb_current}/{cli::pb_total}):",
      "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
    ),
    format_done = "Unpacked {cli::pb_total} response JSON payload{?s} in {cli::pb_elapsed}.",
    clear = FALSE
  )
}

detect_response_json_cols <- function(responses, sample_size = 20L) {
  character_cols <- names(responses)[vapply(responses, is.character, logical(1))]
  character_cols <- setdiff(character_cols, grep("_ts$", names(responses), value = TRUE))

  character_cols[
    purrr::map_lgl(
      character_cols,
      function(col) response_json_col_detected(responses[[col]], sample_size)
    )
  ]
}

response_json_col_detected <- function(x, sample_size = 20L) {
  values <- unique(x[response_json_is_non_empty(x)])
  values <- utils::head(values, sample_size)

  if (length(values) == 0L) {
    return(FALSE)
  }

  any(purrr::map_lgl(values, response_json_has_record_fields))
}

response_json_has_record_fields <- function(json) {
  parsed <- tryCatch(
    jsonlite::parse_json(json, simplifyVector = FALSE),
    error = function(cnd) NULL
  )

  fields <- response_json_fields(parsed)

  all(c("id", "status") %in% fields) &&
    any(c("value", "subform", "code", "score") %in% fields)
}

response_json_fields <- function(parsed) {
  if (is.null(parsed) || length(parsed) == 0L) {
    return(character())
  }

  if (is_record(parsed)) {
    return(names(parsed))
  }

  unique(unlist(purrr::map(parsed, names), use.names = FALSE))
}

parse_response_json_cell <- function(json, keep_empty = FALSE) {
  if (!response_json_is_non_empty(json)) {
    if (keep_empty) {
      return(tibble::tibble(id = NA_character_))
    }

    return(tibble::tibble())
  }

  parsed <- tryCatch(
    jsonlite::parse_json(json, simplifyVector = FALSE),
    error = function(cnd) {
      tibble::tibble(.parse_error = conditionMessage(cnd))
    }
  )

  if (tibble::is_tibble(parsed)) {
    return(parsed)
  }

  records_to_tibble(parsed)
}

standardize_unpacked_response_jsons <- function(unpacked, id_cols) {
  unpacked <-
    unpacked %>%
    dplyr::rename(dplyr::any_of(c(
      "response_id" = "id",
      "response_status" = "status",
      "code_id" = "code",
      "code_score" = "score"
    ))) %>%
    complete_schema(
      list(
        response_id = character(),
        response_status = character(),
        value = list(),
        subform = character(),
        code_id = integer(),
        code_score = numeric()
      ),
      keep_extra = TRUE
    )

  response_cols <- c(
    "response_row", id_cols, "response_column", "response_name",
    "response_ts", "question_index", "response_id", "response_status",
    "value", "subform", "code_id", "code_score"
  )

  unpacked %>%
    dplyr::select(dplyr::any_of(response_cols), dplyr::everything())
}

empty_unpacked_response_jsons <- function(responses, id_cols) {
  out <- responses[0, id_cols, drop = FALSE]
  out$response_row <- integer()
  out$response_column <- character()
  out$response_name <- character()
  out$response_ts <- numeric()
  out$question_index <- integer()
  out$response_id <- character()
  out$response_status <- character()
  out$value <- list()
  out$subform <- character()
  out$code_id <- integer()
  out$code_score <- numeric()

  out %>%
    dplyr::select(
      dplyr::any_of(c(
        "response_row", id_cols, "response_column", "response_name",
        "response_ts", "question_index", "response_id", "response_status",
        "value", "subform", "code_id", "code_score"
      ))
    )
}

response_json_ts_long <- function(responses, response_cols, timestamp_cols) {
  if (length(timestamp_cols) == 0L) {
    return(tibble::tibble(
      response_row = integer(),
      response_column = character(),
      response_ts = numeric()
    ))
  }

  responses %>%
    dplyr::select(
      dplyr::all_of(c("response_row", timestamp_cols))
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(timestamp_cols),
      names_to = "response_ts_column",
      values_to = "response_ts"
    ) %>%
    dplyr::mutate(
      response_name = sub("_ts$", "", response_ts_column),
      response_column = ifelse(
        paste0(response_name, "_content") %in% response_cols,
        paste0(response_name, "_content"),
        response_name
      )
    ) %>%
    dplyr::select(response_row, response_column, response_ts)
}

response_json_is_non_empty <- function(x) {
  !is.na(x) & nzchar(x) & x != "[]"
}

response_json_base <- function(response_column) {
  sub("_content$", "", response_column)
}

response_json_ts_col <- function(response_column) {
  paste0(response_json_base(response_column), "_ts")
}

response_question_index <- function(response_name) {
  question_index <- rep(NA_integer_, length(response_name))
  is_question <- grepl("^question_[0-9]+$", response_name)

  question_index[is_question] <-
    as.integer(sub("^question_([0-9]+)$", "\\1", response_name[is_question]))

  question_index
}

derive_unpacked_variable_id <- function(unpacked, variable_id_from) {
  primary <- unpacked_variable_id_candidate(unpacked, variable_id_from)
  fallback_response_name <- unpacked_variable_id_candidate(unpacked, "response_name")
  fallback_response_id <- unpacked_variable_id_candidate(unpacked, "response_id")

  dplyr::coalesce(primary, fallback_response_name, fallback_response_id)
}

unpacked_variable_id_candidate <- function(unpacked, col) {
  if (!col %in% names(unpacked)) {
    return(rep(NA_character_, nrow(unpacked)))
  }

  value <- unpacked[[col]]

  if (is.list(value)) {
    return(list_to_character(value))
  }

  as.character(value)
}
