log_normalise_unit_size_key <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_remove("\\.xml$") %>%
    stringr::str_trim()
}

log_extract_loadtimes <- function(playbacks) {
  if (is.null(playbacks) || length(playbacks) == 0 || all(is.na(playbacks))) {
    return(numeric())
  }

  if (!is.data.frame(playbacks) || !"unit_loadtime_i" %in% names(playbacks)) {
    return(numeric())
  }

  loadtimes <- suppressWarnings(as.numeric(playbacks$unit_loadtime_i))
  loadtimes[!is.na(loadtimes)]
}

log_ratio_per_mb <- function(value, size_mb) {
  value <- suppressWarnings(as.numeric(value))
  size_mb <- suppressWarnings(as.numeric(size_mb))
  dplyr::case_when(
    is.na(value) | is.na(size_mb) | size_mb <= 0 ~ NA_real_,
    TRUE ~ value / size_mb
  )
}

log_size_lookup <- function(sizes,
                            size_name_col = "name",
                            size_col = "total_size") {
  checkmate::assert_tibble(sizes)
  assert_cols(sizes, c(size_name_col, size_col), "sizes")

  size_data <- sizes
  if ("type" %in% names(size_data)) {
    size_data <- size_data %>%
      dplyr::filter(.data$type == "Unit")
  }

  size_data %>%
    dplyr::transmute(
      unit_size_key = log_normalise_unit_size_key(.data[[size_name_col]]),
      unit_size_name = as.character(.data[[size_name_col]]),
      unit_size_bytes = suppressWarnings(as.numeric(.data[[size_col]]))
    ) %>%
    dplyr::filter(!is.na(.data$unit_size_key), .data$unit_size_key != "") %>%
    dplyr::group_by(.data$unit_size_key) %>%
    dplyr::summarise(
      unit_size_bytes = dplyr::first(.data$unit_size_bytes[!is.na(.data$unit_size_bytes)]),
      unit_size_mb = .data$unit_size_bytes / 1024^2,
      unit_size_n_matches = dplyr::n(),
      unit_size_name = log_first_non_missing(.data$unit_size_name),
      unit_size_names = log_collapse_values(.data$unit_size_name, max_values = 10L),
      unit_size_conflicting = dplyr::n_distinct(.data$unit_size_bytes, na.rm = TRUE) > 1L,
      .groups = "drop"
    )
}

#' Add unit resource sizes to log-derived unit data
#'
#' @param unit_data Tibble. Unit-level or unit-playback data, for example
#' returned by [estimate_unit_times()] or state-specific log summaries.
#' @param sizes Tibble. Output of [compute_sizes()].
#' @param unit_col Optional name of the unit identifier column in `unit_data`.
#' Defaults to `unit_key` when available, otherwise `unit_alias`.
#' @param size_name_col Name column in `sizes`. Defaults to `name`, matching
#' [compute_sizes()] output.
#' @param size_col Size column in `sizes`. Defaults to `total_size`, matching
#' [compute_sizes()] output.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Joins unit resource sizes from [compute_sizes()] to log-derived unit data.
#' Unit names are matched both with and without a trailing `.xml` extension.
#' The function adds byte and MiB sizes and, when load-time columns are present,
#' observed load-time-per-MiB indicators. These indicators are descriptive
#' load-time ratios and should not be interpreted as download speed.
#'
#' @return A tibble.
#'
#' @export
add_unit_sizes <- function(unit_data,
                           sizes,
                           unit_col = NULL,
                           size_name_col = "name",
                           size_col = "total_size") {
  checkmate::assert_tibble(unit_data)
  checkmate::assert_tibble(sizes)

  if (is.null(unit_col)) {
    unit_col <- dplyr::case_when(
      "unit_key" %in% names(unit_data) ~ "unit_key",
      "unit_alias" %in% names(unit_data) ~ "unit_alias",
      TRUE ~ NA_character_
    )
  }

  checkmate::assert_string(unit_col)
  assert_cols(unit_data, unit_col, "unit_data")

  lookup <- log_size_lookup(sizes, size_name_col = size_name_col, size_col = size_col)

  out <- unit_data %>%
    dplyr::mutate(.unit_size_key = log_normalise_unit_size_key(.data[[unit_col]])) %>%
    dplyr::left_join(lookup, by = c(".unit_size_key" = "unit_size_key")) %>%
    dplyr::mutate(
      unit_size_available = !is.na(.data$unit_size_bytes),
      unit_size_mb = suppressWarnings(as.numeric(.data$unit_size_mb))
    ) %>%
    dplyr::select(-".unit_size_key")

  if ("unit_loadtime" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(
        unit_loadtime_per_mb = log_ratio_per_mb(.data$unit_loadtime, .data$unit_size_mb)
      )
  }

  if ("unit_playbacks" %in% names(out)) {
    loadtime_stats <- purrr::map(out$unit_playbacks, log_extract_loadtimes)
    out <- out %>%
      dplyr::mutate(
        unit_n_valid_loadtimes = purrr::map_int(loadtime_stats, length),
        unit_first_loadtime = purrr::map_dbl(
          loadtime_stats,
          ~ if (length(.x) == 0) NA_real_ else .x[[1]]
        ),
        unit_median_loadtime = purrr::map_dbl(
          loadtime_stats,
          ~ if (length(.x) == 0) NA_real_ else stats::median(.x, na.rm = TRUE)
        ),
        unit_max_loadtime = purrr::map_dbl(
          loadtime_stats,
          ~ if (length(.x) == 0) NA_real_ else max(.x, na.rm = TRUE)
        ),
        unit_first_loadtime_per_mb = log_ratio_per_mb(.data$unit_first_loadtime, .data$unit_size_mb),
        unit_median_loadtime_per_mb = log_ratio_per_mb(.data$unit_median_loadtime, .data$unit_size_mb),
        unit_max_loadtime_per_mb = log_ratio_per_mb(.data$unit_max_loadtime, .data$unit_size_mb)
      )
  }

  out
}

system_check_add_col <- function(data, target, sources) {
  if (target %in% names(data)) {
    return(data)
  }

  source <- sources[sources %in% names(data)][1]
  if (!is.na(source)) {
    data[[target]] <- data[[source]]
  }

  data
}

system_check_normalise_cols <- function(data) {
  data %>%
    system_check_add_col("group_id", c("groupname", "groupName", "group_name")) %>%
    system_check_add_col("login_name", c("loginname", "loginName", "login")) %>%
    system_check_add_col("booklet_id", c("bookletname", "bookletName", "booklet")) %>%
    system_check_add_col("unit_key", c("unitname", "unitName", "unit_alias")) %>%
    system_check_add_col("variable_id", c("id", "variableId", "variableID"))
}

system_check_default_cols <- function(data) {
  intersect(
    c(
      "Name",
      "name",
      "group_id",
      "group",
      "login_name",
      "login",
      "login_code",
      "code",
      "booklet_id",
      "booklet",
      "unit_key",
      "unit_alias"
    ),
    names(data)
  )
}

system_check_network_pattern <- function() {
  paste(
    c(
      "download",
      "upload",
      "downlink",
      "effective",
      "rtt",
      "latency",
      "network",
      "speed",
      "throughput",
      "bandwidth",
      "geschwindigkeit"
    ),
    collapse = "|"
  )
}

system_check_metric_tibble <- function(names, values, pattern = system_check_network_pattern()) {
  names <- as.character(names)
  keep <- !is.na(names) &
    names != "" &
    stringr::str_detect(names, stringr::regex(pattern, ignore_case = TRUE))

  if (!any(keep)) {
    return(tibble::tibble(metric = character(), value = character()))
  }

  tibble::tibble(
    metric = names[keep],
    value = as.character(values[keep])
  ) %>%
    dplyr::filter(!is.na(.data$value), .data$value != "")
}

system_check_wide_metrics <- function(data, check_cols, pattern = system_check_network_pattern()) {
  metric_cols <- setdiff(names(data), check_cols)
  metric_cols <- metric_cols[stringr::str_detect(metric_cols, stringr::regex(pattern, ignore_case = TRUE))]

  if (length(metric_cols) == 0) {
    return(tibble::tibble(system_check_network_metrics = replicate(nrow(data), tibble::tibble(), simplify = FALSE)))
  }

  tibble::tibble(
    system_check_network_metrics = purrr::pmap(
      data[metric_cols],
      function(...) {
        values <- list(...)
        system_check_metric_tibble(metric_cols, values, pattern = pattern)
      }
    )
  )
}

system_check_long_metrics <- function(data, pattern = system_check_network_pattern()) {
  if (!all(c("variable_id", "value") %in% names(data))) {
    return(replicate(nrow(data), tibble::tibble(), simplify = FALSE))
  }

  purrr::map2(
    data$variable_id,
    data$value,
    ~ system_check_metric_tibble(.x, .y, pattern = pattern)
  )
}

system_check_bind_metrics <- function(metrics) {
  metrics <- metrics[purrr::map_lgl(metrics, ~ is.data.frame(.x) && nrow(.x) > 0)]
  if (length(metrics) == 0) {
    return(tibble::tibble(metric = character(), value = character()))
  }

  dplyr::bind_rows(metrics) %>%
    dplyr::distinct()
}

system_check_metric_value <- function(metrics, pattern) {
  if (!is.data.frame(metrics) || nrow(metrics) == 0) {
    return(NA_character_)
  }

  values <- metrics %>%
    dplyr::filter(stringr::str_detect(.data$metric, stringr::regex(pattern, ignore_case = TRUE))) %>%
    dplyr::pull(.data$value)

  log_first_non_missing(values)
}

#' Summarise system check data
#'
#' @param system_checks Tibble. Output of [get_system_checks()] or
#' [read_system_checks()].
#' @param check_cols Optional character vector defining one system-check case.
#' By default, available identifier columns such as `Name`, group/login columns,
#' booklet, and unit columns are used.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Produces one row per system-check case and detects network-related fields
#' conservatively from either wide columns or long `variable_id`/`value` pairs.
#' Network metrics are retained as a list column because system-check exports
#' may vary while these files are evolving.
#'
#' @return A tibble.
#'
#' @export
summarise_system_checks <- function(system_checks, check_cols = NULL) {
  checkmate::assert_tibble(system_checks)
  system_checks <- system_check_normalise_cols(system_checks)

  if (is.null(check_cols)) {
    check_cols <- system_check_default_cols(system_checks)
  }
  checkmate::assert_character(check_cols, null.ok = FALSE)
  if (length(check_cols) > 0) {
    assert_cols(system_checks, check_cols, "system_checks")
  }

  wide_metrics <- system_check_wide_metrics(system_checks, check_cols)
  long_metrics <- system_check_long_metrics(system_checks)

  data <- system_checks %>%
    dplyr::mutate(
      .system_check_row = dplyr::row_number(),
      .network_metrics = purrr::map2(
        wide_metrics$system_check_network_metrics,
        long_metrics,
        ~ system_check_bind_metrics(list(.x, .y))
      )
    )

  if (length(check_cols) > 0) {
    data <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(check_cols)))
  }

  data %>%
    dplyr::summarise(
      n_system_check_rows = dplyr::n(),
      n_response_variables = if ("variable_id" %in% names(data)) {
        log_n_distinct_non_missing(.data$variable_id)
      } else {
        0L
      },
      response_variable_ids = if ("variable_id" %in% names(data)) {
        log_collapse_values(.data$variable_id, max_values = 20L)
      } else {
        NA_character_
      },
      system_check_network_metrics = list(system_check_bind_metrics(.data$.network_metrics)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      n_network_metric_values = purrr::map_int(.data$system_check_network_metrics, nrow),
      has_network_metrics = .data$n_network_metric_values > 0L,
      system_check_download_value = purrr::map_chr(
        .data$system_check_network_metrics,
        system_check_metric_value,
        pattern = "download|downlink|geschwindigkeit"
      ),
      system_check_upload_value = purrr::map_chr(
        .data$system_check_network_metrics,
        system_check_metric_value,
        pattern = "upload"
      ),
      system_check_rtt_value = purrr::map_chr(
        .data$system_check_network_metrics,
        system_check_metric_value,
        pattern = "rtt|latency"
      ),
      system_check_effective_type = purrr::map_chr(
        .data$system_check_network_metrics,
        system_check_metric_value,
        pattern = "effective"
      )
    )
}

#' Add system check summaries to log-derived data
#'
#' @param log_data Tibble. Log-derived session data, for example from
#' [summarise_log_qc()] or [summarise_log_environment()].
#' @param system_check_summary Tibble. Output of [summarise_system_checks()].
#' @param by Optional character vector of join columns. If omitted, the
#' intersection of common session identifier columns is used.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Joins system-check summaries onto log-derived session data. The function
#' only joins by explicit or clearly shared identifier columns and errors when
#' no join key can be found.
#'
#' @return A tibble.
#'
#' @export
add_system_check_summary <- function(log_data,
                                     system_check_summary,
                                     by = NULL) {
  checkmate::assert_tibble(log_data)
  checkmate::assert_tibble(system_check_summary)
  log_data <- system_check_normalise_cols(log_data)
  system_check_summary <- system_check_normalise_cols(system_check_summary)

  if (is.null(by)) {
    by <- intersect(
      c("group_id", "group", "login_name", "login", "login_code", "code", "booklet_id", "Name", "name"),
      intersect(names(log_data), names(system_check_summary))
    )
  }

  checkmate::assert_character(by, min.len = 1)
  assert_cols(log_data, by, "log_data")
  assert_cols(system_check_summary, by, "system_check_summary")

  dplyr::left_join(log_data, system_check_summary, by = by)
}
