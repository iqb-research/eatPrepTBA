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
  if (
    is.null(coding_scheme) ||
    length(coding_scheme) != 1 ||
    is.na(coding_scheme) ||
    coding_scheme %in% c("", "NA", "null")
  ) {
    return(tibble::tibble())
  }

  variable_codings <-
    coding_scheme %>%
    jsonlite::parse_json() %>%
    purrr::pluck("variableCodings")

  if (is.null(variable_codings) || length(variable_codings) == 0) {
    return(tibble::tibble())
  }

  scheme_table <- records_to_tibble(variable_codings)

  # For legacy reasons
  if (!tibble::has_name(scheme_table, "alias")) {
    scheme_table$alias <- as.list(scheme_table$id)
  }

  scheme_table <-
    scheme_table %>%
    tidyr::unnest(alias, keep_empty = TRUE) %>%
    dplyr::mutate(
      id = list_to_character(id),
      alias = dplyr::coalesce(list_to_character(alias), id)
    )

  # Level of variable in dependency tree (makes it easier to search for dependencies)
  source_tree <-
    prepare_source_tree(coding_scheme) %>%
    complete_schema(source_tree_schema(), keep_extra = FALSE)

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
    complete_schema(variable_schema(), keep_extra = FALSE) %>%
    dplyr::filter(
      if (filter_has_codes) {
        is.na(variable_source_type) | variable_source_type != "BASE_NO_VALUE"
      } else {
        TRUE
      }
    ) %>%
    dplyr::left_join(source_tree, by = dplyr::join_by("variable_ref")) %>%
    complete_schema(combine_schemas(variable_schema(), source_tree_schema()), keep_extra = FALSE) %>%
    dplyr::mutate(
      variable_source_parameters = purrr::map(variable_source_parameters, prepare_source_parameters),
      codes = purrr::map(codes, prepare_codes)
    ) %>%
    tidyr::unnest(c(codes, variable_source_parameters), keep_empty = TRUE) %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("variable_fragmenting")),
                    list_to_character),
      dplyr::across(dplyr::any_of(c("variable_page_ref")),
                    list_to_integer)
    )

  prepared_scheme %>%
    dplyr::mutate(
      rule_sets = purrr::map(rule_sets, prepare_rule_sets)
    ) %>%
    tidyr::unnest(rule_sets, keep_empty = TRUE) %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("rule_set_operator", "rule_operator")),
                    operator_to_character),
      rules = purrr::map(rules, prepare_rules)
    ) %>%
    tidyr::unnest(rules, keep_empty = TRUE) %>%
    normalize_scheme()
}

prepare_source_parameters <- function(source_parameters) {
  if (is.null(source_parameters) || length(source_parameters) == 0) {
    return(empty_schema(source_parameter_schema()))
  }

  tibble::tibble(
    variable_source_processing = list(purrr::pluck(source_parameters, "processing")),
    variable_source_solver_expression = scalar_to_character(
      purrr::pluck(source_parameters, "solverExpression", .default = NA_character_)
    )
  ) %>%
    complete_schema(source_parameter_schema(), keep_extra = FALSE)
}

prepare_codes <- function(codes) {
  records_to_tibble(codes) %>%
    dplyr::select(any_of(c(
      code_id = "id",
      code_type = "type",
      code_label = "label",
      code_score = "score",
      code_manual_instruction = "manualInstruction",
      rule_set_operator = "ruleSetOperatorAnd",
      rule_sets = "ruleSets"
    ))) %>%
    complete_schema(code_schema(), keep_extra = FALSE) %>%
    dplyr::mutate(
      rule_set_operator = operator_to_character(rule_set_operator)
    )
}

prepare_rule_sets <- function(rule_sets) {
  records_to_tibble(rule_sets) %>%
    dplyr::mutate(
      rule_set_no = dplyr::row_number()
    ) %>%
    dplyr::rename(dplyr::any_of(c(
      rule_operator = "ruleOperatorAnd",
      rule_set_array_position = "valueArrayPos"
    ))) %>%
    complete_schema(rule_set_schema(), keep_extra = FALSE) %>%
    dplyr::mutate(
      rule_operator = operator_to_character(rule_operator),
      rule_set_array_position = list_to_character(rule_set_array_position)
    )
}

prepare_rules <- function(rules) {
  records_to_tibble(rules) %>%
    dplyr::rename(dplyr::any_of(c(
      rule_method = "method",
      rule_fragment_position = "fragment",
      rule_parameter = "parameters"
    ))) %>%
    complete_schema(rule_schema(), keep_extra = FALSE) %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("rule_fragment_position", "rule_parameter")),
                    list_to_character)
    )
}

normalize_scheme <- function(tbl) {
  tbl %>%
    complete_schema(final_scheme_schema(), keep_extra = FALSE) %>%
    dplyr::mutate(
      variable_code_model = list_to_character(variable_code_model),
      dplyr::across(dplyr::any_of(c("rule_set_operator", "rule_operator")),
                    operator_to_character)
    )
}

records_to_tibble <- function(records) {
  records <- as_record_list(records)

  if (length(records) == 0) {
    return(tibble::tibble())
  }

  records %>%
    purrr::map(record_to_tibble) %>%
    dplyr::bind_rows()
}

as_record_list <- function(records) {
  if (is.null(records) || length(records) == 0) {
    return(list())
  }

  if (!is.list(records)) {
    if (length(records) == 1 && is.na(records)) {
      return(list())
    }

    return(list(records))
  }

  if (is_record(records)) {
    return(list(records))
  }

  records
}

is_record <- function(x) {
  is.list(x) &&
    !is.null(names(x)) &&
    any(nzchar(names(x)))
}

record_to_tibble <- function(record) {
  record %>%
    purrr::map(function(value) {
      if (is.null(value) || is.list(value) || length(value) != 1) {
        list(value)
      } else {
        value
      }
    }) %>%
    tibble::as_tibble(.rows = 1)
}

complete_schema <- function(tbl, schema, keep_extra = TRUE) {
  tbl <- tibble::as_tibble(tbl)
  missing <- setdiff(names(schema), names(tbl))

  for (col in missing) {
    tbl[[col]] <- missing_schema_column(schema[[col]], nrow(tbl))
  }

  for (col in intersect(names(schema), names(tbl))) {
    tbl[[col]] <- coerce_schema_column(tbl[[col]], schema[[col]])
  }

  if (keep_extra) {
    tbl %>%
      dplyr::select(dplyr::all_of(names(schema)), dplyr::everything())
  } else {
    tbl %>%
      dplyr::select(dplyr::all_of(names(schema)))
  }
}

empty_schema <- function(schema) {
  complete_schema(tibble::tibble(), schema, keep_extra = FALSE)
}

combine_schemas <- function(...) {
  schemas <- list(...)
  combined <- list()

  for (schema in schemas) {
    for (col in names(schema)) {
      combined[[col]] <- schema[[col]]
    }
  }

  combined
}

missing_schema_column <- function(prototype, n) {
  if (is.list(prototype)) {
    return(rep(list(NULL), n))
  }

  if (is.integer(prototype)) {
    return(rep(NA_integer_, n))
  }

  if (is.double(prototype)) {
    return(rep(NA_real_, n))
  }

  if (is.logical(prototype)) {
    return(rep(NA, n))
  }

  rep(NA_character_, n)
}

coerce_schema_column <- function(x, prototype) {
  if (is.list(prototype)) {
    if (is.list(x)) {
      return(x)
    }

    return(as.list(x))
  }

  if (is.integer(prototype)) {
    return(list_to_integer(x))
  }

  if (is.double(prototype)) {
    return(list_to_double(x))
  }

  if (is.logical(prototype)) {
    return(list_to_logical(x))
  }

  list_to_character(x)
}

source_tree_schema <- function() {
  list(
    variable_ref = character(),
    variable_level = integer(),
    variable_sources = list()
  )
}

source_parameter_schema <- function() {
  list(
    variable_source_processing = list(),
    variable_source_solver_expression = character()
  )
}

variable_schema <- function() {
  list(
    variable_id = character(),
    variable_ref = character(),
    variable_label = character(),
    variable_source_type = character(),
    variable_source_parameters = list(),
    derive_sources = list(),
    variable_processing = list(),
    variable_fragmenting = character(),
    variable_general_instruction = character(),
    variable_code_model = list(),
    variable_page_ref = integer(),
    codes = list()
  )
}

code_schema <- function() {
  list(
    code_id = integer(),
    code_type = character(),
    code_label = character(),
    code_score = numeric(),
    code_manual_instruction = character(),
    rule_set_operator = character(),
    rule_sets = list()
  )
}

rule_set_schema <- function() {
  list(
    rule_set_no = integer(),
    rule_set_array_position = character(),
    rule_operator = character(),
    rules = list()
  )
}

rule_schema <- function() {
  list(
    rule_fragment_position = character(),
    rule_method = character(),
    rule_parameter = character()
  )
}

final_scheme_schema <- function() {
  c(
    list(
      variable_id = character(),
      variable_ref = character(),
      variable_label = character(),
      variable_source_type = character()
    ),
    source_parameter_schema(),
    list(
      derive_sources = list(),
      variable_processing = list(),
      variable_fragmenting = character(),
      variable_general_instruction = character(),
      variable_code_model = character(),
      variable_page_ref = integer(),
      code_id = integer(),
      code_type = character(),
      code_label = character(),
      code_score = numeric(),
      code_manual_instruction = character(),
      rule_set_no = integer(),
      rule_set_operator = character(),
      rule_set_array_position = character(),
      rule_operator = character(),
      rule_fragment_position = character(),
      rule_method = character(),
      rule_parameter = character(),
      variable_level = integer(),
      variable_sources = list()
    )
  )
}

list_to_character <- function(x) {
  if (is.list(x)) {
    return(purrr::map_chr(x, scalar_to_character))
  }

  as.character(x)
}

list_to_integer <- function(x) {
  if (is.list(x)) {
    return(purrr::map_int(x, scalar_to_integer))
  }

  suppressWarnings(as.integer(x))
}

list_to_double <- function(x) {
  if (is.list(x)) {
    return(purrr::map_dbl(x, scalar_to_double))
  }

  suppressWarnings(as.double(x))
}

list_to_logical <- function(x) {
  if (is.list(x)) {
    return(purrr::map_lgl(x, scalar_to_logical))
  }

  suppressWarnings(as.logical(x))
}

operator_to_character <- function(x) {
  if (is.list(x)) {
    return(purrr::map_chr(x, scalar_to_operator))
  }

  purrr::map_chr(x, scalar_to_operator)
}

scalar_to_character <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  if (is.list(x) && length(x) == 1 && !is.list(x[[1]])) {
    return(scalar_to_character(x[[1]]))
  }

  if (is.atomic(x)) {
    if (length(x) == 1 && is.na(x)) {
      return(NA_character_)
    }

    return(paste(as.character(x), collapse = ","))
  }

  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
}

scalar_to_integer <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_integer_)
  }

  if (is.list(x)) {
    if (length(x) == 1) {
      return(scalar_to_integer(x[[1]]))
    }

    return(NA_integer_)
  }

  suppressWarnings(as.integer(x[[1]]))
}

scalar_to_double <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_real_)
  }

  if (is.list(x)) {
    if (length(x) == 1) {
      return(scalar_to_double(x[[1]]))
    }

    return(NA_real_)
  }

  suppressWarnings(as.double(x[[1]]))
}

scalar_to_logical <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA)
  }

  if (is.list(x)) {
    if (length(x) == 1) {
      return(scalar_to_logical(x[[1]]))
    }

    return(NA)
  }

  suppressWarnings(as.logical(x[[1]]))
}

scalar_to_operator <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  if (is.list(x)) {
    if (length(x) == 1) {
      return(scalar_to_operator(x[[1]]))
    }

    return(NA_character_)
  }

  if (is.logical(x[[1]])) {
    if (is.na(x[[1]])) {
      return(NA_character_)
    }

    return(ifelse(x[[1]], "AND", "OR"))
  }

  value <- scalar_to_character(x[[1]])

  if (is.na(value)) {
    return(NA_character_)
  }

  value_upper <- toupper(trimws(value))

  if (value_upper %in% c("AND", "OR")) {
    return(value_upper)
  }

  if (value_upper %in% c("TRUE", "T", "1")) {
    return("AND")
  }

  if (value_upper %in% c("FALSE", "F", "0")) {
    return("OR")
  }

  value
}

coerce_list <- function(x) {
  if (is.list(x)) x else list(x)
}
