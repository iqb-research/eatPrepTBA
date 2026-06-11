#' Code unit responses with coding schemes
#'
#' @param responses Tibble. Response data retrieved from the IQB Testcenter with setting the argument `prepare = FALSE` for [get_responses()] or [read_responses()].
#' @param units Tibble. Unit data retrieved from the IQB Studio after setting the argument `coding_scheme = TRUE` for [get_units()].
#' @param prepare Logical. Whether to unpack the coding results and to add information from the coding schemes.
#' @param by Character. Additional columns as subgroups for the coding (e.g., in case of duplicate unit data for a specific person that could emerge in offline settings).
#' @param codes_manual Tibble (optional). Data frame holding the manual codes. Defaults to `NULL` and does only automatic coding.
#' @param missings Tibble (optional). Provide missing meta data with `code_id`, `status`, `code_score`, and `code_type`. Defaults to `NULL` and uses default scheme.
#' @param parallel Logical. Should the coding be conducted on multiple cores?
#' @param n_cores Integer. Number of the cores to be used for coding (only relevant if `parallel = TRUE`). Will default to number of available system cores - 1.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function automatically codes responses by using the `eatAutoCode` package.
#'
#' @return A tibble.
#' @keywords internal
code_responses_legacy <- function(responses,
                           units,
                           prepare = FALSE,
                           by = NULL,
                           codes_manual = NULL,
                           missings = NULL,
                           parallel = FALSE,
                           n_cores = NULL
) {
  cli_setting()
  # input validation
  responses_cols <- c("unitname", "groupname", "loginname", "code", "bookletname")
  checkmate::assert_tibble(responses)
  if(!(all(responses_cols %in% colnames(responses)))) stop(paste0("'responses' must contain the columns {", paste0(responses_cols, collapse = ", "), "}, but is missing the column(s): {", paste0(setdiff(responses_cols, colnames(responses)), collapse = ", "), "}."))
  units_cols <- c("unit_key", "coding_scheme")
  checkmate::assert_tibble(units)
  if(!(all(units_cols %in% colnames(units)))) stop(paste0("'units' must contain the columns {", paste0(units_cols, collapse = ", "), "}, but is missing the column(s): {", paste0(setdiff(units_cols, colnames(units)), collapse = ", "), "}."))

  codes_manual_cols <- c("groupId", "bookletId", "personId", "variableId",
                         "unitId", "code")
  checkmate::assert_tibble(codes_manual, null.ok = TRUE)
  if(!is.null(codes_manual) && !(all(codes_manual_cols %in% colnames(codes_manual)))) stop(paste0("'codes_manual' must contain the columns {", paste0(codes_manual_cols, collapse = ", "), "}, but is missing the column(s): {", paste0(setdiff(codes_manual_cols, colnames(codes_manual)), collapse = ", "), "}."))
  missings_cols <- c("code_id", "status", "code_score", "code_type")
  checkmate::assert_tibble(missings, null.ok = TRUE)
  if(!is.null(missings) && !(all(missings_cols %in% colnames(missings)))) stop(paste0("'missings' must contain the columns {", paste0(missings_cols, collapse = ", "), "}, but is missing the column(s): {", paste0(setdiff(missings_cols, colnames(missings)), collapse = ", "), "}."))

  checkmate::assert_character(by, null.ok = TRUE)
  checkmate::assert_numeric(n_cores, null.ok = TRUE)
  checkmate::assert_logical(prepare, len = 1)
  checkmate::assert_logical(parallel, len = 1)

  # progressr::handlers("cli")

  # if (parallel) {
  #   cli::cli_h2("Setting up parallel coding")
  #
  #   options <- furrr::furrr_options(seed = TRUE)
  #
  #   if (is.null(n_cores)) {
  #     n_cores <- future::availableCores() - 1
  #   }
  #
  #   cli::cli_text("Using {n_cores} cores")
  #
  #   future::plan(future::multisession, workers = n_cores)
  #
  #   map <- function(.x, .f, ..., .progress = .progress) {
  #     furrr::future_map(.x = .x, .f = .f, ..., .options = options, .progress = .progress)
  #   }
  #   map2 <- function(.x, .y, .f, ..., .progress = .progress) {
  #     furrr::future_map2(.x = .x, .y = .y, .f = .f, ..., .options = options, .progress = .progress)
  #   }
  #   map2_chr <- function(.x, .y, .f, ..., .progress = .progress) {
  #     furrr::future_map2_chr(.x = .x, .y = .y, .f = .f, ..., .options = options, .progress = .progress)
  #   }
  #   pmap <- function(.l, .f, ..., .progress = .progress) {
  #     furrr::future_pmap(.l = .l, .f = .f, ..., .options = options, .progress = .progress)
  #   }
  #
  #
  # } else {
  #   map <- purrr::map
  #   map2 <- purrr::map2
  #   map2_chr <- purrr::map2_chr
  #   pmap <- purrr::pmap
  # }

  if (is.null(missings)) {
    missings <-
      tibble::tribble(
        ~code_id, ~status, ~code_score, ~code_type,
        -96, "NOT_REACHED", 0, "MISSING NOT REACHED",
        -97, "CODING_ERROR", 0, "MISSING CODING IMPOSSIBLE",
        -98, "INVALID", 0, "MISSING INVALID RESPONSE",
        -99, "DISPLAYED", 0, "MISSING BY OMISSION"
      )
  }

  cli::cli_h2("Automatic coding routine")

  start_time <- Sys.time()

  cli::cli_text("Time started: {start_time}")

  cli::cli_h3("Prepare responses")

  responses_prepared <-
    responses %>%
    # Helper to get rid of (for this step) unnecessary stateVariables
    dplyr::mutate(
      response_id = purrr::map_chr(responses, "id")
    ) %>%
    dplyr::filter(response_id == "elementCodes") %>%
    dplyr::select(-response_id) %>%
    dplyr::mutate(
      responses = purrr::map_chr(responses, "content")
    ) %>%
    dplyr::filter(responses != "[]") %>%
    # TODO: Already rename on get_responses()
    dplyr::rename(
      dplyr::any_of(c(
        group_id = "groupname",
        login_name = "loginname",
        login_code = "code",
        booklet_id = "bookletname",
        unit_key = "unitname"
      ))
    )

  coding_schemes <-
    units %>%
    dplyr::filter(
      unit_key %in% responses_prepared$unit_key
    ) %>%
    dplyr::select(unit_key, coding_scheme)

  unit_keys <- coding_schemes$unit_key
  n_units <- length(unique(unit_keys))

  cli::cli_text("Identified {n_units} units that can be coded.")

  # Insert manual codes
  if (!is.null(codes_manual) || prepare) {
    cli::cli_h3("Prepare coding schemes")

    coding_schemes_prepared <-
      coding_schemes %>%
      dplyr::mutate(
        prepared_coding_schemes = purrr::map(coding_scheme,
                                             function(cs) {
                                               cs %>%
                                                 prepare_coding_scheme() %>%
                                                 # ... could otherwise not be unpacked
                                                 dplyr::select(-any_of(c("page",
                                                                         "fragmenting",
                                                                         "valueArrayPos",
                                                                         # TODO: Must urgently be reactivated and integrated!!
                                                                         "variable_alias")))
                                             },
                                             .progress = list(
                                               type ="custom",
                                               show_after = 0,
                                               extra = list(
                                                 unit_keys = pad_ids(unit_keys)
                                               ),
                                               format = "Preparing {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
                                               format_done = "Prepared {cli::pb_total} coding scheme{?s} in {cli::pb_elapsed}.",
                                               clear = FALSE
                                             ))
      ) %>%
      tidyr::unnest(prepared_coding_schemes) %>%
      dplyr::select(unit_key, variable_id, source_type, code_id, code_type, code_score, derive_sources)

    pcs_variables <-
      coding_schemes_prepared %>%
      dplyr::distinct(unit_key, variable_id, source_type)

    pcs_codes <-
      coding_schemes_prepared %>%
      dplyr::distinct(unit_key, variable_id, code_id, code_type, code_score, derive_sources) %>%
      dplyr::filter(!is.na(code_id))
  }

  if (!is.null(codes_manual)) {
    cli::cli_h3("Insert manual codes")

    pcs_variables_insert <-
      pcs_variables %>%
      dplyr::select(-source_type)

    pcs_codes_insert <-
      pcs_codes %>%
      dplyr::mutate(status = "CODING_COMPLETE") %>%
      dplyr::select(-derive_sources, -code_type)

    codes_manual_prepared <-
      codes_manual %>%
      dplyr::select(any_of(c(
        group_id = "groupId",
        booklet_id = "bookletId",
        login_code = "personId",
        variable_id = "variableId",
        unit_key = "unitId",
        code_id = "code"
      ))) %>%
      dplyr::filter(unit_key %in% unit_keys) %>%
      dplyr::mutate(
        code_id = as.integer(code_id)
      ) %>%
      dplyr::left_join(pcs_variables_insert, by = dplyr::join_by("unit_key", "variable_id")) %>%
      dplyr::left_join(pcs_codes_insert, by = dplyr::join_by("unit_key", "variable_id", "code_id")) %>%
      dplyr::left_join(missings %>% dplyr::select(-code_type), by = dplyr::join_by("code_id"), suffix = c("", "_miss")) %>%
      dplyr::mutate(
        status = dplyr::coalesce(status, status_miss),
        code_score = dplyr::coalesce(code_score, code_score_miss)
      ) %>%
      dplyr::select(-status_miss, -code_score_miss)

    codes_manual_nested <-
      codes_manual_prepared %>%
      dplyr::rename(id = variable_id, code = code_id, score = code_score) %>%
      tidyr::nest(
        codes_manual = c(id, code, score, status)
      ) %>%
      dplyr::arrange(unit_key)

    manual_unit_keys <- codes_manual_nested$unit_key

    codes_to_merge <-
      codes_manual_nested %>%
      dplyr::mutate(
        codes_manual = purrr::map(codes_manual, function(x) {
          x %>%
            as.list() %>%
            purrr::list_transpose()
        },
        .progress = list(
          type ="custom",
          show_after = 0,
          extra = list(
            unit_keys = pad_ids(manual_unit_keys)
          ),
          format = "Preparing manual code cases for {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
          format_done = "Prepared {cli::pb_total} manual code case{?s} in {cli::pb_elapsed}.",
          clear = FALSE
        ))
      )

    # TODO: Add filter for unit_keys that are only in coding_schemes, also needs to be arranged
    responses_insert_prepared <-
      responses_prepared %>%
      dplyr::left_join(
        codes_to_merge, by = dplyr::join_by("group_id", "login_code", "booklet_id", "unit_key")
      )

    prep_unit_keys <- responses_insert_prepared$unit_key

    responses_inserted <-
      responses_insert_prepared %>%
      dplyr::mutate(
        responses = purrr::map2_chr(responses, codes_manual, insert_manual_legacy,
                                    .progress = list(
                                      type ="custom",
                                      show_after = 0,
                                      extra = list(
                                        unit_keys = pad_ids(prep_unit_keys)
                                      ),
                                      format = "Inserting manual code cases for {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
                                      format_done = "Inserted {cli::pb_total} manual code case{?s} in {cli::pb_elapsed}.",
                                      clear = FALSE
                                    )
        )
      ) %>%
      dplyr::select(-codes_manual)
  } else {
    responses_inserted <- responses_prepared
  }

  responses_for_coding <-
    responses_inserted %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("group_id", "login_code", "unit_key", by)))) %>%
    # To potentially identify reduplicated cases afterwards!
    dplyr::mutate(code_case = seq_along(unit_key)) %>%
    dplyr::ungroup() %>%
    tidyr::nest(unit_responses = -any_of(c("unit_key", by, "code_case"))) %>%
    # Filter off units without coding scheme
    dplyr::semi_join(coding_schemes, by = dplyr::join_by("unit_key")) %>%
    dplyr::left_join(coding_schemes, by = dplyr::join_by("unit_key")) %>%
    dplyr::arrange(unit_key)

  final_unit_keys <- responses_for_coding$unit_key

  cli::cli_h3("Start coding")

  if (is.null(by)) {
    progress_bar_format <- "Coding unit {.unit-key {cli::pb_extra$final_unit_keys[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"

    progress_bar_format_done <- "Coded {cli::pb_total} unit{?s} in {cli::pb_elapsed}."
  } else {
    progress_bar_format <- "Coding unit {.unit-key {cli::pb_extra$final_unit_keys[cli::pb_current+1]}} for subgroup: {cli::pb_current}/{cli::pb_total} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"

    progress_bar_format_done <- "Coded {cli::pb_extra$n_units} unit{?s} in {cli::pb_total} subgroup{?s} in {cli::pb_elapsed}."
  }

  responses_coded <-
    responses_for_coding %>%
    dplyr::mutate(
      unit_codes = purrr::pmap(
        .l = list(unit_responses, coding_scheme),
        .f = code_unit_legacy,
        .progress = list(
          type ="custom",
          show_after = 0,
          extra = list(
            final_unit_keys = pad_ids(final_unit_keys),
            n_units = n_units
          ),
          format = progress_bar_format,
          format_done = progress_bar_format_done,
          clear = FALSE
        ))
    )

  if (prepare) {
    tryCatch(
      error = function(cnd) {
        cli::cli_alert_danger("Preparation could not be completed. Returning coded data frame.",
                              wrap = TRUE)

        # Default return
        return(responses_coded)
      },
      # TODO: Does not work if no code_ids are available (e.g., only coding errors)
      responses_coded %>%
        dplyr::select(any_of(c("code_case", "unit_key", by, "unit_codes"))) %>%
        tidyr::unnest(unit_codes) %>%
        dplyr::select(any_of(c("code_case", "unit_key", by, "group_id", "login_name", "login_code",
                               "booklet_id", "codes"))) %>%
        tidyr::unnest(codes) %>%
        dplyr::rename(variable_id = id, code_id = code) %>%
        tidyr::unnest(value, keep_empty = TRUE) %>%
        dplyr::left_join(pcs_variables, by = dplyr::join_by("unit_key", "variable_id")) %>%
        dplyr::left_join(pcs_codes, by = dplyr::join_by("unit_key", "variable_id", "code_id")) %>%
        dplyr::select(any_of(c(
          "code_case",
          "unit_key",
          by,
          "variable_id",
          "source_type",
          "group_id",
          "login_code",
          "value",
          "status",
          "code_id",
          "code_type",
          "score"
        ))),
      finally = cli::cli_text("Time finished: {Sys.time()}")
    )
  } else {
    finish_time <- Sys.time()
    cli::cli_text("Time finished: {Sys.time()}")

    return(responses_coded)
  }
}

#' Code the responses of one unit responses with its coding scheme
#'
#' @param unit_responses Character. Response data of one unit retrieved from the IQB Testcenter in JSON format.
#' @param coding_scheme Character. Coding scheme of the unit retrieved from the IQB Studio after setting the argument `coding_scheme = TRUE` for [get_units()].
#'
#' @description
#' This function automatically codes responses of one unit by using the `eatAutoCode` package.
#'
#' @return A tibble.
#'
#' @keywords internal
code_unit_legacy <- function(unit_responses, coding_scheme) {
  # input validation
  checkmate::assert_tibble(unit_responses)
  if(!("responses" %in% colnames(unit_responses))) stop("'unit_responses' must contain the column {responses}.")
  checkmate::assert_character(coding_scheme)

  unit_responses %>%
    # dplyr::mutate(
    #   responses = purrr::map_chr(responses, "content")
    # ) %>%
    # dplyr::filter(responses != "[]") %>%
    dplyr::mutate(
      codes = purrr::map(responses, function(resp) {
        eatAutoCode::code_responses(coding_scheme = coding_scheme,
                                    responses = resp) %>%
          dplyr::mutate(
            value = purrr::map(value, function(x) {
              if (length(x) > 1) {
                x %>% stringr::str_c(collapse = ",")
              } else {
                as.character(x)
              }
            })
          )
      })
    )
}

#' Add manual codes to unit responses
#'
#' @param unit_responses Character. Response data of one unit retrieved from the IQB Testcenter in JSON format.
#' @param unit_codes_manual List. Manual codes to insert into the response data.
#'
#' @description
#' This function automatically codes responses of one unit by using the `eatAutoCode` package.
#'
#' @return A tibble.
#'
#' @keywords internal
insert_manual_legacy <- function(unit_responses, unit_codes_manual) {
  # input validation
  checkmate::assert_character(unit_responses)
  checkmate::assert_list(unit_codes_manual, null.ok = TRUE)

  # Check if unit_codes_manual is NULL or empty, return original unit_responses if so
  if (is.null(unit_codes_manual) || length(unit_codes_manual) == 0) {
    return(unit_responses)
  }

  # Unpack JSON and prepare lookup for manual codes
  unit_responses_unpacked <- jsonlite::parse_json(unit_responses)
  unit_codes_lookup <- purrr::set_names(unit_codes_manual, purrr::map_chr(unit_codes_manual, "id"))

  # Process items in unit_responses_unpacked
  unit_responses_inserted <- purrr::map(unit_responses_unpacked, function(item) {
    id <- item$id
    if (!is.null(unit_codes_lookup[[id]])) {
      purrr::list_modify(item, !!!unit_codes_lookup[[id]])
    } else {
      item
    }
  })

  # Add new items from unit_codes_manual that aren’t in unit_responses
  missing_items <- purrr::keep(unit_codes_manual,
                               function(x) !(x$id %in% purrr::map_chr(unit_responses_unpacked, "id")))
  unit_responses_combined <- c(unit_responses_inserted, missing_items)

  # Convert back to JSON
  jsonlite::toJSON(unit_responses_combined, auto_unbox = TRUE, null = "null") %>% as.character()
}
