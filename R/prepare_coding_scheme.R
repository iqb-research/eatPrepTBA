#' Prepares a readable version of a coding scheme of one unit
#'
#' @description
#' Extends the given coding scheme of a unit to a full data frame that can be filtered for batch checks.
#'
#'
#' @param coding_scheme Coding scheme as prepared by [get_units()] with setting the argument `coding_scheme = TRUE`.
#' @param filter_has_codes Only returns variables that were not deactivated. Defaults to `TRUE`.
#'
#' @return A tibble.
#' @export
prepare_coding_scheme <- function(coding_scheme, filter_has_codes = TRUE) {
  if (is.null(coding_scheme)) {
    return(tibble::tibble())
  }

  scheme_table <-
    coding_scheme %>%
    jsonlite::parse_json() %>%
    purrr::pluck("variableCodings") %>%
    purrr::list_transpose() %>%
    tibble::as_tibble()

  # For legacy reasons
  if (!tibble::has_name(scheme_table, "alias")) {
    scheme_table <-
      scheme_table %>%
      dplyr::mutate(
        alias = id
      )
  }

  scheme_table <-
    scheme_table %>%
    tidyr::unnest(alias, keep_empty = TRUE) %>%
    dplyr::mutate(
      alias = ifelse(is.na(alias), id, alias)
    )

  # Level of variable in dependency tree (makes it easier to search for dependencies)
  source_tree <- prepare_source_tree(coding_scheme)

  prepared_scheme <-
    scheme_table %>%
    dplyr::select(any_of(c(
      variable_id = "alias",
      variable_ref = "id",
      # TODO: This might replace the page identifier for marker items and derived variables
      variable_label = "label",
      variable_source_type = "sourceType",
      variable_source_parameters = "sourceParameters",
      derive_sources = "deriveSources",
      variable_processing = "processing",
      variable_fragmenting = "fragmenting",
      variable_general_instruction = "manualInstruction",
      variable_code_model = "codeModel",
      # TODO: This is the page identifier, but it is still buggy
      variable_page_ref = "page",
      codes = "codes"
    ))) %>%
    dplyr::filter(dplyr::if_any(c("variable_source_type"), function(x) {
      if (filter_has_codes) {
        x != "BASE_NO_VALUE"
      } else TRUE
    })) %>%
    dplyr::left_join(source_tree, by = dplyr::join_by("variable_ref")) %>%
    dplyr::mutate(
      variable_source_parameters = purrr::map(variable_source_parameters, function(source_parameters) {
        tibble::tibble(
          variable_source_processing = list(source_parameters$processing),
          variable_source_solver_expression = source_parameters$solverExpression,
        )
      }),
      codes = purrr::map(codes, prepare_codes)
    ) %>%
    tidyr::unnest(c(codes, variable_source_parameters), keep_empty = TRUE) %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("variable_fragmenting")),
                    list_to_character),
      dplyr::across(dplyr::any_of(c("variable_page_ref")),
                    list_to_integer)
    )

  if (tibble::has_name(prepared_scheme, "rule_sets")) {
    prepared_rule_sets <-
      prepared_scheme %>%
      dplyr::mutate(
        rule_sets = purrr::map(rule_sets, prepare_rule_sets)
      ) %>%
      tidyr::unnest(rule_sets, keep_empty = TRUE) %>%
      dplyr::mutate(
        # ruleOperatorAnd and ruleSetOpertorAnd will be recoded
        dplyr::across(dplyr::any_of(c("rule_set_operator", "rule_operator")),
                      function(x) ifelse(x, "AND", "OR")),
      )

    if (tibble::has_name(prepared_rule_sets, "rules")) {
      prepared_rule_sets %>%
        dplyr::mutate(
          rules = purrr::map(rules, prepare_rules)
        ) %>%
        tidyr::unnest(rules, keep_empty = TRUE) %>%
        dplyr::mutate(
          dplyr::across(dplyr::any_of(c("rule_parameter")),
                        list_to_character)
        )
    } else {
      prepared_rule_sets
    }
  } else {
    prepared_scheme
  }
}

prepare_codes <- function(codes) {
  codes %>%
    purrr::list_transpose() %>%
    tibble::as_tibble() %>%
    dplyr::select(any_of(c(
      code_id = "id",
      code_type = "type",
      code_label = "label",
      code_score = "score",
      code_manual_instruction = "manualInstruction",
      rule_set_operator = "ruleSetOperatorAnd",
      rule_sets = "ruleSets"
    ))
    )
}

prepare_rule_sets <- function(rule_sets) {
  if (!is.null(rule_sets)) {
    if (is.null(names(rule_sets)) & length(rule_sets) > 0) {
      rule_sets %>%
        purrr::imap(function(x, i) {
          x %>%
            tibble::as_tibble() %>%
            dplyr::mutate(
              rule_set_no = i,
              dplyr::across(dplyr::any_of(c("valueArrayPos")),
                            list_to_character)
            )
        }) %>%
        # purrr::list_transpose() %>%
        # tibble::as_tibble() %>%
        dplyr::bind_rows() %>%
        dplyr::rename(dplyr::any_of(c(
          rule_operator = "ruleOperatorAnd",
          rule_set_array_position = "valueArrayPos"
        )))
    }
  } else {
    if (!is.null(rule_sets$rules)) {
      tibble::tibble()
    }
  }
}

list_to_character <- function(x) {
  purrr::map_chr(x, function(x) {
    if (is.null(x)) {
      return(NA_character_)
    }

    as.character(x)
  })
}

list_to_integer <- function(x) {
  purrr::map_int(x, function(x) {
    if (is.null(x)) {
      return(NA_integer_)
    }

    as.integer(x)
  })
}

coerce_list <- function(x) {
  if (is.list(x)) x else list(x)
}

prepare_rules <- function(rules) {
  if (is.null(rules) || length(rules) == 0) {
    prepared_rules <-
      tibble::tibble()
  } else if (is.null(names(rules)) & length(rules) >= 1) {
    prepared_rules <-
      rules %>%
      purrr::list_transpose() %>%
      tibble::as_tibble()
  } else {
    prepared_rules <-
      rules %>%
      tibble::as_tibble()
  }

  prepared_rules <-
    prepared_rules %>%
    dplyr::rename(dplyr::any_of(c(
      rule_method = "method",
      rule_fragment_position = "fragment"
    ))) %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("rule_fragment_position")),
                    list_to_character)
    )

  if (tibble::has_name(rules, "parameters")) {
    prepared_rules %>%
      dplyr::rename("rule_parameter" = "parameters")
  } else {
    prepared_rules
  }
}

