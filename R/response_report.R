response_report_to_tibble <- function(resp,
                                      progress = "Preparing response report") {
  entries <-
    resp %>%
    purrr::compact() %>%
    purrr::list_flatten()

  if (length(entries) == 0) {
    return(tibble::tibble())
  }

  entries %>%
    tibble::enframe(name = NULL) %>%
    dplyr::mutate(
      value = purrr::map(value, response_report_entry_to_tibble,
                         .progress = progress)
    ) %>%
    tidyr::unnest(value)
}

response_report_entry_to_tibble <- function(x) {
  x <- purrr::discard(x, is.null)

  responses <- x$responses
  if (is.null(responses) || length(responses) == 0) {
    responses <- list()
  }
  x$responses <- NULL

  laststate <- x$laststate
  if (is.null(laststate) || length(laststate) == 0) {
    laststate <- NA_character_
  }
  x$laststate <- NULL

  x <-
    purrr::map(x, function(value) {
      if (is.list(value) || length(value) != 1) {
        list(value)
      } else {
        value
      }
    })

  x$responses <- list(responses)
  x$laststate <- if (is.list(laststate)) list(laststate) else laststate

  tibble::as_tibble_row(x)
}

filter_response_units <- function(responses_raw, units_filter_off = NULL) {
  if (is.null(units_filter_off) || nrow(responses_raw) == 0) {
    return(responses_raw)
  }

  unit_filter_cols <- intersect(c("unitname", "originalUnitId"), names(responses_raw))

  if (length(unit_filter_cols) == 0) {
    return(responses_raw)
  }

  responses_raw %>%
    dplyr::filter(
      !dplyr::if_any(
        dplyr::all_of(unit_filter_cols),
        function(x) x %in% units_filter_off
      )
    )
}

preserve_empty_response_payloads <- function(responses) {
  if (!"responses" %in% names(responses)) {
    responses$responses <- NA_character_
  }

  if ("coded" %in% names(responses)) {
    empty_coded_payload <- is.na(responses$responses) &
      !is.na(responses$coded) &
      responses$coded == "[]"
    responses$responses[empty_coded_payload] <- "[]"

    if ("coded_ts" %in% names(responses)) {
      if (!"responses_ts" %in% names(responses)) {
        responses$responses_ts <- NA_character_
      }
      responses$responses_ts[empty_coded_payload] <- responses$coded_ts[empty_coded_payload]
    }
  }

  responses
}

expected_response_slot_ids <- function() {
  list(
    required = c("elementCodes", "stateVariableCodes"),
    optional = c("geometryVariableCodes")
  )
}

response_payload_entries <- function(payload, is_parsed = TRUE) {
  if (is.null(payload) || length(payload) == 0) {
    return(list())
  }

  if (!is_parsed) {
    payload <- payload[!is.na(payload) & payload != ""]

    if (length(payload) == 0) {
      return(list())
    }

    payload <- purrr::map(
      payload,
      function(x) {
        tryCatch(
          jsonlite::parse_json(x),
          error = function(cnd) list()
        )
      }
    ) %>%
      purrr::flatten()
  }

  payload
}

response_payload_entry_id <- function(x) {
  id <- tryCatch(x$id, error = function(cnd) NULL)

  if (is.null(id) || length(id) == 0 || is.na(id[[1]])) {
    NA_character_
  } else {
    as.character(id[[1]])
  }
}

response_entry_has_name <- function(x, name) {
  is.list(x) && name %in% names(x)
}

is_response_slot_like_id <- function(id) {
  expected <- expected_response_slot_ids()
  id %in% c(expected$required, expected$optional, "responses") ||
    grepl("Codes$", id)
}

is_wrapper_response_entry <- function(x) {
  id <- response_payload_entry_id(x)

  !is.na(id) &&
    response_entry_has_name(x, "content") &&
    is_response_slot_like_id(id)
}

is_subform_response_entry <- function(x) {
  id <- response_payload_entry_id(x)

  !is.na(id) &&
    response_entry_has_name(x, "content") &&
    !is_response_slot_like_id(id) &&
    (
      response_entry_has_name(x, "subForm") ||
        response_entry_has_name(x, "subform") ||
        response_entry_has_name(x, "responseType")
    )
}

is_direct_response_entry <- function(x) {
  id <- response_payload_entry_id(x)

  response_entry_has_name(x, "id") &&
    !is.na(id) &&
    (
      response_entry_has_name(x, "status") ||
        response_entry_has_name(x, "value")
    )
}

classify_response_payload <- function(payload, is_parsed = TRUE) {
  payload <- response_payload_entries(payload, is_parsed = is_parsed)

  if (length(payload) == 0) {
    return(list(
      shape = "empty",
      ids = character(),
      wrapper_ids = character(),
      subform_ids = character(),
      direct_ids = character(),
      unknown_ids = character()
    ))
  }

  ids <- purrr::map_chr(
    payload,
    response_payload_entry_id
  )

  wrapper <- purrr::map_lgl(payload, is_wrapper_response_entry)
  subform <- purrr::map_lgl(payload, is_subform_response_entry)
  direct <- purrr::map_lgl(payload, is_direct_response_entry)
  unknown <- !(wrapper | subform | direct)

  shape <- dplyr::case_when(
    all(wrapper) ~ "wrapper",
    all(subform) ~ "subform",
    all(direct) ~ "direct",
    any(wrapper) || any(subform) || any(direct) ~ "mixed",
    .default = "unknown"
  )

  list(
    shape = shape,
    ids = ids[!is.na(ids) & ids != ""],
    wrapper_ids = ids[wrapper & !is.na(ids) & ids != ""],
    subform_ids = ids[subform & !is.na(ids) & ids != ""],
    direct_ids = ids[direct & !is.na(ids) & ids != ""],
    unknown_ids = ids[unknown & !is.na(ids) & ids != ""]
  )
}

response_slot_diagnostics <- function(responses,
                                      is_parsed = TRUE,
                                      response_col = "responses") {
  expected <- expected_response_slot_ids()

  empty_diagnostics <- list(
    n_payloads = 0L,
    n_wrapper_payloads = 0L,
    n_subform_payloads = 0L,
    n_direct_payloads = 0L,
    n_mixed_payloads = 0L,
    n_unknown_payloads = 0L,
    wrapper_observed = character(),
    subform_observed = character(),
    direct_observed = character(),
    unknown_entry_ids = character(),
    missing_required = character(),
    missing_optional = character(),
    unknown_wrapper = character(),
    missing_required_counts = integer(),
    missing_optional_counts = integer()
  )

  if (!response_col %in% names(responses) || nrow(responses) == 0) {
    return(empty_diagnostics)
  }

  payload_diagnostics <- purrr::map(
    responses[[response_col]],
    classify_response_payload,
    is_parsed = is_parsed
  )

  non_empty_payloads <- purrr::keep(
    payload_diagnostics,
    function(x) x$shape != "empty"
  )

  if (length(non_empty_payloads) == 0) {
    return(empty_diagnostics)
  }

  wrapper_payloads <- purrr::keep(
    non_empty_payloads,
    function(x) x$shape == "wrapper"
  )

  wrapper_ids <- purrr::map(wrapper_payloads, "wrapper_ids")
  wrapper_observed <- unique_response_ids(wrapper_ids)
  subform_observed <- unique_response_ids(purrr::map(non_empty_payloads, "subform_ids"))
  direct_observed <- unique_response_ids(purrr::map(non_empty_payloads, "direct_ids"))
  unknown_entry_ids <- unique_response_ids(purrr::map(non_empty_payloads, "unknown_ids"))
  known <- c(expected$required, expected$optional)

  if (length(wrapper_payloads) > 0) {
    missing_required_counts <- purrr::map_int(
      stats::setNames(expected$required, expected$required),
      function(id) sum(!purrr::map_lgl(wrapper_ids, function(x) id %in% x))
    )

    missing_optional_counts <- purrr::map_int(
      stats::setNames(expected$optional, expected$optional),
      function(id) sum(!purrr::map_lgl(wrapper_ids, function(x) id %in% x))
    )
  } else {
    missing_required_counts <- stats::setNames(
      rep(0L, length(expected$required)),
      expected$required
    )

    missing_optional_counts <- stats::setNames(
      rep(0L, length(expected$optional)),
      expected$optional
    )
  }

  list(
    n_payloads = length(non_empty_payloads),
    n_wrapper_payloads = length(wrapper_payloads),
    n_subform_payloads = sum(purrr::map_chr(non_empty_payloads, "shape") == "subform"),
    n_direct_payloads = sum(purrr::map_chr(non_empty_payloads, "shape") == "direct"),
    n_mixed_payloads = sum(purrr::map_chr(non_empty_payloads, "shape") == "mixed"),
    n_unknown_payloads = sum(purrr::map_chr(non_empty_payloads, "shape") == "unknown"),
    wrapper_observed = wrapper_observed,
    subform_observed = subform_observed,
    direct_observed = direct_observed,
    unknown_entry_ids = unknown_entry_ids,
    missing_required = names(missing_required_counts)[missing_required_counts > 0],
    missing_optional = names(missing_optional_counts)[missing_optional_counts > 0],
    unknown_wrapper = setdiff(wrapper_observed, known),
    missing_required_counts = missing_required_counts,
    missing_optional_counts = missing_optional_counts
  )
}

announce_response_slot_diagnostics <- function(responses,
                                               source = "Response data",
                                               is_parsed = TRUE,
                                               response_col = "responses",
                                               diagnostics = c("compact", "full", "none")) {
  verbosity <- match.arg(diagnostics)

  if (verbosity == "none") {
    return(responses)
  }

  diagnostics <- response_slot_diagnostics(
    responses,
    is_parsed = is_parsed,
    response_col = response_col
  )

  if (diagnostics$n_payloads == 0) {
    return(responses)
  }

  if (diagnostics$n_direct_payloads > 0) {
    direct_examples <- format_response_slot_examples(
      diagnostics$direct_observed,
      n = if (identical(verbosity, "full")) Inf else 3,
      full_hint = identical(verbosity, "compact")
    )

    if (identical(verbosity, "full")) {
      cli::cli_alert_info(
        "{source}: detected {format_response_count(diagnostics$n_direct_payloads)} direct response-shaped payload{?s}; ids such as {direct_examples} are response ids, not response slots, and were excluded from slot diagnostics.",
        wrap = TRUE
      )
    } else {
      cli::cli_alert_info(
        "{source}: detected {format_response_count(diagnostics$n_direct_payloads)} direct response-shaped payload{?s}; ids such as {direct_examples} are response ids.",
        wrap = TRUE
      )
    }
  }

  if (diagnostics$n_subform_payloads > 0) {
    subform_examples <- format_response_slot_examples(
      diagnostics$subform_observed,
      n = if (identical(verbosity, "full")) Inf else 3,
      full_hint = identical(verbosity, "compact")
    )

    if (identical(verbosity, "full")) {
      cli::cli_alert_info(
        "{source}: detected {format_response_count(diagnostics$n_subform_payloads)} subform response payload{?s}; ids such as {subform_examples} are subform response containers, not standard response slots, and were excluded from standard slot diagnostics.",
        wrap = TRUE
      )
    } else {
      cli::cli_alert_info(
        "{source}: detected {format_response_count(diagnostics$n_subform_payloads)} subform response payload{?s}; ids such as {subform_examples} are subform response containers.",
        wrap = TRUE
      )
    }
  }

  if (diagnostics$n_mixed_payloads > 0 || diagnostics$n_unknown_payloads > 0) {
    shape_summary <- format_response_shape_counts(
      mixed = diagnostics$n_mixed_payloads,
      unknown = diagnostics$n_unknown_payloads
    )

    cli::cli_alert_info(
      "{source}: detected {shape_summary}; slot checks use {format_response_count(diagnostics$n_wrapper_payloads)} standard wrapper-shaped payload{?s} only.",
      wrap = TRUE
    )
  }

  if (length(diagnostics$missing_required) > 0) {
    missing_required <- format_response_slot_counts(
      diagnostics$missing_required_counts[diagnostics$missing_required],
      diagnostics$n_wrapper_payloads,
      label = "standard wrapper payloads"
    )

    cli::cli_alert_warning(
      "{source}: missing required standard wrapper slot ids: {missing_required}. Output behavior is unchanged.",
      wrap = TRUE
    )
  }

  if (
    identical(verbosity, "compact") &&
      diagnostics$n_wrapper_payloads > 0 &&
      length(diagnostics$missing_required) == 0 &&
      length(diagnostics$unknown_wrapper) == 0
  ) {
    cli::cli_alert_info(
      "{source}: standard response slots look OK: required slots were present in all {format_response_count(diagnostics$n_wrapper_payloads)} standard wrapper payload{?s}, and no unexpected wrapper slot ids were found.",
      wrap = TRUE
    )
  }

  if (identical(verbosity, "full") && diagnostics$n_wrapper_payloads > 0) {
    standard_coverage <- format_standard_response_slot_counts(
      diagnostics$missing_required_counts,
      diagnostics$missing_optional_counts,
      diagnostics$n_wrapper_payloads
    )

    cli::cli_alert_info(
      "{source}: standard wrapper slot coverage: {standard_coverage}. Output behavior is unchanged.",
      wrap = TRUE
    )
  }

  if (identical(verbosity, "full") && length(diagnostics$unknown_entry_ids) > 0) {
    unknown_entry_ids <- format_response_slot_examples(
      diagnostics$unknown_entry_ids,
      n = Inf
    )

    cli::cli_alert_info(
      "{source}: unrecognized top-level response ids include: {unknown_entry_ids}. Output behavior is unchanged.",
      wrap = TRUE
    )
  }

  if (length(diagnostics$unknown_wrapper) > 0) {
    unknown <- format_response_slot_examples(
      diagnostics$unknown_wrapper,
      n = if (identical(verbosity, "full")) Inf else 5,
      full_hint = identical(verbosity, "compact")
    )

    if ("responses" %in% diagnostics$unknown_wrapper) {
      cli::cli_alert_warning(
        "{source}: found unexpected wrapper slot ids: {unknown}. The inner slot id {.field responses} is not part of the expected current Testcenter response-slot structure. Output behavior is unchanged.",
        wrap = TRUE
      )
    } else {
      cli::cli_alert_warning(
        "{source}: found unexpected wrapper slot ids: {unknown}. Output behavior is unchanged.",
        wrap = TRUE
      )
    }
  }

  responses
}

format_response_count <- function(x) {
  format(as.integer(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

unique_response_ids <- function(ids) {
  ids <- unlist(ids, use.names = FALSE)

  if (is.null(ids)) {
    return(character())
  }

  unique(as.character(ids))
}

format_response_slot_counts <- function(counts, total, label = "payloads") {
  paste0(
    names(counts),
    " (",
    format_response_count(counts),
    "/",
    format_response_count(total),
    " ",
    label,
    ")",
    collapse = ", "
  )
}

format_standard_response_slot_counts <- function(required_absent_counts,
                                                 optional_absent_counts,
                                                 total) {
  absent_counts <- c(required_absent_counts, optional_absent_counts)
  present_counts <- total - absent_counts

  paste0(
    names(absent_counts),
    " present in ",
    format_response_count(present_counts),
    "/",
    format_response_count(total),
    " standard wrapper payloads",
    collapse = "; "
  )
}

format_response_shape_counts <- function(mixed = 0L, unknown = 0L) {
  counts <- c(
    "mixed response-shaped payloads" = mixed,
    "unrecognized response-shaped payloads" = unknown
  )

  counts <- counts[counts > 0]

  paste0(format_response_count(counts), " ", names(counts), collapse = ", ")
}

format_response_slot_examples <- function(ids, n = 5, full_hint = FALSE) {
  ids <- unique(as.character(ids))
  shown <- if (is.infinite(n)) ids else head(ids, n)
  label <- paste(shown, collapse = ", ")

  if (!is.infinite(n) && length(ids) > n) {
    label <- paste0(label, ", and ", length(ids) - n, " more")
    if (full_hint) {
      label <- paste0(label, " (use diagnostics = \"full\" to list all ids)")
    }
  }

  label
}

response_preparation_progress <- function(label,
                                          done_label,
                                          diagnostics = c("compact", "full", "none")) {
  diagnostics <- match.arg(diagnostics)

  if (diagnostics == "none") {
    return(list(
      type = "custom",
      show_after = 0,
      format = paste0(label, " {cli::pb_percent} | ETA: {cli::pb_eta}"),
      clear = TRUE
    ))
  }

  list(
    type = "custom",
    show_after = 0,
    format = paste0(label, " {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"),
    format_done = paste0(done_label, " in {cli::pb_elapsed}."),
    clear = FALSE
  )
}

announce_empty_nested_response_payloads <- function(responses_raw,
                                                    source = "Response report") {
  if (!"responses" %in% names(responses_raw) || nrow(responses_raw) == 0) {
    return(responses_raw)
  }

  empty_payload <- purrr::map_lgl(
    responses_raw$responses,
    function(x) is.null(x) || length(x) == 0
  )
  n_empty <- sum(empty_payload)

  if (n_empty == 0) {
    return(responses_raw)
  }

  if (n_empty == nrow(responses_raw)) {
    cli::cli_alert_warning(
      "{source}: every response row ({n_empty}) has an empty nested payload; rows are kept and will become {.code responses = NA} after preparation."
    )
  } else {
    cli::cli_alert_info(
      "{source}: kept {n_empty} response row{?s} with empty nested payloads; these rows become {.code responses = NA} after preparation."
    )
  }

  responses_raw
}

announce_missing_response_payloads <- function(responses,
                                               source = "Response data",
                                               diagnostics = c("compact", "full", "none")) {
  diagnostics <- match.arg(diagnostics)

  if (diagnostics == "none") {
    return(responses)
  }

  if (!"responses" %in% names(responses) || nrow(responses) == 0) {
    return(responses)
  }

  n_missing <- sum(is.na(responses$responses))

  if (n_missing == 0) {
    return(responses)
  }

  if (n_missing == nrow(responses)) {
    cli::cli_alert_warning(
      "{source}: every response row ({n_missing}) has a missing payload; rows are kept with {.code responses = NA}, omitted by {.fn code_responses}, and should be completed afterwards with {.fn complete_design}."
    )
  } else {
    cli::cli_alert_info(
      "{source}: kept {n_missing} response row{?s} with missing payloads as {.code responses = NA}; {.fn code_responses} will omit them, and they should be completed afterwards with {.fn complete_design}."
    )
  }

  responses
}

announce_response_unit_filter <- function(n_before,
                                          n_after,
                                          units_filter_off = NULL) {
  if (is.null(units_filter_off) || n_before == n_after) {
    return(invisible(NULL))
  }

  n_removed <- n_before - n_after

  if (n_after == 0) {
    cli::cli_alert_warning(
      "{.arg units_filter_off} removed all {n_before} response row{?s}; returning an empty tibble."
    )
  } else {
    cli::cli_alert_info(
      "{.arg units_filter_off} removed {n_removed} response row{?s}."
    )
  }

  invisible(NULL)
}

announce_failed_response_groups <- function(failed_groups) {
  failed_groups <- failed_groups[!is.na(failed_groups) & failed_groups != ""]

  if (length(failed_groups) == 0) {
    return(invisible(NULL))
  }

  cli::cli_alert_warning(
    "Response reports could not be returned or parsed for {length(failed_groups)} group{?s}: {format_response_group_examples(failed_groups)}."
  )

  invisible(NULL)
}

format_response_group_examples <- function(groups, n = 5) {
  groups <- unique(as.character(groups))
  shown <- head(groups, n)
  label <- paste(shown, collapse = ", ")

  if (length(groups) > n) {
    label <- paste0(label, ", and ", length(groups) - n, " more")
  }

  label
}
