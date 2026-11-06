# Read explicit variable declarations, not arbitrary variableId references in
# feedback, display settings, or media. StarS variableInfo lives inside the unit
# definition; it is distinct from Studio's separately stored VariableInfo table.
# https://iqb-specifications.github.io/stars-unit-definition/
design_variables_stars <- function(definition, unit_key) {
  if (is.null(definition) || length(definition) == 0L ||
      (length(definition) == 1L && is.na(definition))) return(character())
  checkmate::assert_string(definition)

  # Other players may use non-JSON definitions. Only inspect a definition that
  # identifies itself as StarS; malformed StarS JSON must not silently disappear.
  if (!grepl('"id"\\s*:\\s*"stars-unit-definition"', definition, perl = TRUE)) {
    return(character())
  }
  definition <- tryCatch(jsonlite::parse_json(definition), error = function(e) {
    cli::cli_abort("Invalid StarS unit definition for {.field {unit_key}}.", parent = e)
  })
  if (!is.list(definition) || !identical(definition$id, "stars-unit-definition")) {
    return(character())
  }

  id <- function(value) {
    if (is.null(value)) return(character())
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
      cli::cli_abort("Invalid variable ID in StarS unit definition for {.field {unit_key}}.")
    }
    value[nzchar(trimws(value))]
  }
  parameters <- definition$interactionParameters
  info <- definition$variableInfo
  meta <- definition$closingMetaButtons
  if ((!is.null(parameters) && !is.list(parameters)) ||
      (!is.null(info) && !is.list(info)) ||
      (!is.null(meta) && !is.list(meta))) {
    cli::cli_abort("Invalid variable declarations in StarS unit definition for {.field {unit_key}}.")
  }

  info_ids <- unlist(lapply(info, function(variable) {
    if (!is.list(variable)) {
      cli::cli_abort("Invalid variableInfo entry in StarS unit definition for {.field {unit_key}}.")
    }
    id(variable$variableId)
  }), use.names = FALSE)

  unique(c(
    id(parameters$variableId), info_ids,
    id(meta$variableIdMetaSelection), id(meta$variableIdMetaOutcome)
  ))
}
