# Keep provenance on the JSON record so aliases, subforms, and overlays retain
# the auto-coder's response identity. Process only payloads needing annotation.
annotate_nr_response_payloads <- function(coding_scheme, unit_responses,
                                         manual_provenance_prepared = FALSE) {
  field <- function(x, name) {
    if (!is.list(x)) return(NULL)
    positions <- which(names(x) == name)
    if (length(positions)) x[[positions[length(positions)]]] else NULL
  }
  scheme <- tryCatch(jsonlite::parse_json(coding_scheme), error = function(e) NULL)
  variables <- field(scheme, "variableCodings")
  has_nr_rule <- is.list(variables) && any(vapply(variables, function(x) {
    "TAKE_NOT_REACHED_AS_VALUE_CHANGED" %in% field(field(x, "sourceParameters"), "processing")
  }, logical(1)))

  columns <- c("responses", "geometry_variables")
  if (!manual_provenance_prepared) columns <- c(columns, "manual")
  batches <- replacements <- list()
  for (column in intersect(columns, names(unit_responses))) {
    values <- unit_responses[[column]]
    if (is.factor(values)) values <- as.character(values)
    # Optional columns can be entirely logical NA. Other non-text input is
    # left to the coder, preserving its existing input handling.
    if (!is.character(values)) next
    selected <- which(!is.na(values) &
      (has_nr_rule | grepl("coding_from_not_reached", values, fixed = TRUE)))
    if (!length(selected)) next
    unique_payloads <- unique(values[selected])
    batches[[column]] <- list(values = unname(as.list(unique_payloads)),
                             inheritStatus = column != "responses")
    replacements[[column]] <- list(values = values, selected = selected,
                                  index = match(values[selected], unique_payloads))
  }
  if (!length(batches)) return(unit_responses)

  # JSON.parse/stringify have the same number and duplicate-key semantics as
  # the coder. A batch avoids serializing each response field through R's S4
  # JSON methods; identical payloads are processed once per column and unit.
  annotated <- nr_provenance_context()$call("annotateNrPayloads", coding_scheme, batches)
  for (column in names(replacements)) {
    replacement <- replacements[[column]]
    replacement$values[replacement$selected] <-
      unlist(annotated[[column]], use.names = FALSE)[replacement$index]
    unit_responses[[column]] <- replacement$values
  }
  unit_responses
}

# Reuse one private context across units. V8 is also used by eatAutoCode; do
# not modify that package's context or depend on its internal R objects.
nr_provenance_context <- local({
  context <- NULL
  function() {
    if (is.null(context)) {
      created <- V8::v8()
      created$source(system.file("js", "nr-provenance.js", package = "eatPrepTBA",
                                 mustWork = TRUE))
      context <<- created
    }
    context
  }
})
