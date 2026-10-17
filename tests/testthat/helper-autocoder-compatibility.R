# Independent fixtures for contracts with the real, installed eatAutoCode.
autocoder_contract_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
}

autocoder_contract_variable <- function(id, alias = toupper(id),
                                       type = "BASE", sources = character(),
                                       promote_nr = TRUE) {
  processing <- "TAKE_EMPTY_AS_VALID"
  if (promote_nr) processing <- c(processing, "TAKE_NOT_REACHED_AS_VALUE_CHANGED")
  list(
    id = id, alias = alias, sourceType = type,
    sourceParameters = list(processing = as.list(processing)),
    deriveSources = as.list(sources),
    codes = list(
      list(id = 1L, type = "FULL_CREDIT", score = 1L,
           ruleSets = list(list(rules = list(list(
             method = "NUMERIC_MATCH", parameters = list("-999")
           ))))),
      list(id = 0L, type = "RESIDUAL_AUTO", score = 0L)
    )
  )
}

autocoder_contract_scheme <- function(variables) {
  autocoder_contract_json(list(variableCodings = variables))
}

autocoder_contract_flags <- function(out) {
  # Losing the whole field must fail, rather than being normalized to unknown.
  testthat::expect_true("coding_from_not_reached" %in% names(out))
  flags <- out$coding_from_not_reached
  testthat::expect_length(flags, nrow(out))
  vapply(as.list(flags), function(flag) {
    if (is.null(flag)) return(NA)
    testthat::expect_true(is.logical(flag) && length(flag) == 1L)
    flag
  }, logical(1))
}

autocoder_contract_compare <- function(scheme, input) {
  original <- eatAutoCode::code_responses_array(scheme, input)
  annotated <- annotate_nr_response_payloads(scheme, input)
  actual <- eatAutoCode::code_responses_array(scheme, annotated)
  # Compare every original field, including value, status, code, score and IDs.
  original$coding_from_not_reached <- NULL
  testthat::expect_identical(actual[names(original)], original)
  actual
}

autocoder_contract_fixture <- function(type = "SUM_SCORE", chain = FALSE) {
  variables <- list(
    autocoder_contract_variable("earlier", "Earlier", promote_nr = FALSE),
    autocoder_contract_variable("a"),
    autocoder_contract_variable("b")
  )
  sources <- if (type == "COPY_VALUE") "a" else c("a", "b")
  if (chain) {
    variables[[length(variables) + 1L]] <-
      autocoder_contract_variable("middle", "Middle", "SUM_SCORE", sources)
    sources <- "middle"
  }
  derived <- autocoder_contract_variable("derived", "Derived", type, sources)
  if (type == "SOLVER") {
    aliases <- vapply(variables, `[[`, character(1), "alias")
    ids <- vapply(variables, `[[`, character(1), "id")
    derived$sourceParameters$solverExpression <- paste0(
      "${", aliases[match(sources, ids)], "}", collapse = " + "
    )
  }
  variables[[length(variables) + 1L]] <- derived
  ids <- vapply(variables, `[[`, character(1), "id")
  aliases <- vapply(variables, `[[`, character(1), "alias")
  codes <- minimal_unit_codes()[rep(1L, length(variables)), ]
  codes$variable_id <- aliases
  codes$variable_ref <- ids
  codes$variable_source_type <- vapply(variables, `[[`, character(1), "sourceType")
  codes$variable_level <- c(0L, 0L, 0L, if (chain) c(1L, 2L) else 1L)
  codes$variable_page <- seq_along(variables)
  codes$variable_sources <- lapply(variables, function(variable) {
    refs <- unlist(variable$deriveSources, use.names = FALSE)
    tibble::tibble(variable_source_id = aliases[match(refs, ids)],
                   variable_source_ref = as.character(refs),
                   variable_source_direct = rep(TRUE, length(refs)))
  })
  codes$variable_source_processing <- lapply(variables, function(variable) {
    unlist(variable$sourceParameters$processing, use.names = FALSE)
  })
  codes$variable_codes <- rep(list(tibble::tibble(
    code_id = c(1L, 0L), code_type = c("FULL_CREDIT", "RESIDUAL_AUTO"),
    code_score = c(1L, 0L)
  )), length(variables))
  units <- minimal_units()
  units$coding_scheme <- autocoder_contract_scheme(variables)
  units$unit_codes <- list(codes)
  units$items_list <- list(tibble::tibble(
    variable_id = c("Earlier", "Derived"), variable_ref = c("earlier", "derived"),
    item_id = c("I0", "I1")
  ))
  list(units = units, variables = variables)
}

autocoder_contract_responses <- function(mixed = FALSE, earlier = "DISPLAYED") {
  tibble::tibble(
    group_id = "G", login_name = "L", login_code = "P", booklet_id = "B",
    unit_key = "U1", unit_alias = "U1",
    responses = autocoder_contract_json(list(
      list(id = "earlier", status = earlier, value = NULL),
      list(id = "a", status = if (mixed) "VALUE_CHANGED" else "NOT_REACHED", value = 2L),
      list(id = "b", status = "NOT_REACHED", value = 3L)
    ))
  )
}

autocoder_contract_complete <- function(coded, units, recode, preserve,
                                       include_derived = TRUE) {
  design <- dplyr::mutate(
    dplyr::select(coded, group_id, login_name, login_code, booklet_id,
                  unit_key, unit_alias, variable_id),
    booklet_no = 1L, testlet_no = 1L, unit_booklet_no = 1L
  )
  if (!include_derived) design <- design[design$variable_id %in% c("Earlier", "A", "B"), ]
  suppressMessages(complete_design(
    coded, units, design, recode_omissions_to_not_reached = recode,
    preserve_not_reached = preserve
  ))
}
