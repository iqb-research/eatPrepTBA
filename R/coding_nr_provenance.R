# Carry the response's NR provenance through the auto-coder on the JSON record
# itself. The coder preserves extra record fields while mapping refs to aliases,
# grouping subforms, and creating derived responses; joining on person/variable
# identifiers afterwards would lose that identity.
annotate_nr_response_payloads <- function(coding_scheme, unit_responses,
                                         manual_provenance_prepared = FALSE) {
  # JSON.parse uses the last occurrence of a repeated object key. Use the
  # same interpretation, including for the reserved marker we recompute.
  field <- function(x, name) {
    if (!is.list(x)) return(NULL)
    positions <- which(names(x) == name)
    if (length(positions)) x[[utils::tail(positions, 1L)]] else NULL
  }
  scheme <- tryCatch(jsonlite::parse_json(coding_scheme), error = function(e) NULL)
  variables <- field(scheme, "variableCodings")
  if (!is.list(variables) || !is.null(names(variables)) ||
      !all(vapply(variables, is.list, logical(1)))) {
    variables <- list()
  }

  scalar_string <- function(x) {
    if (is.character(x) && length(x) == 1L && !is.na(x)) x else NA_character_
  }
  ids <- vapply(variables, function(x) scalar_string(field(x, "id")), character(1))
  aliases <- vapply(variables, function(x) scalar_string(field(x, "alias")), character(1))
  source_types <- vapply(variables, function(x) scalar_string(field(x, "sourceType")), character(1))
  transforms_nr <- vapply(variables, function(x) {
    "TAKE_NOT_REACHED_AS_VALUE_CHANGED" %in% field(field(x, "sourceParameters"), "processing")
  }, logical(1))

  # The auto-coder resolves aliases before refs. A derived alias that reuses
  # the id of its own BASE source is the one exception: that input belongs to
  # the BASE source and must remain available for deriving the result.
  alias_usable <- !is.na(aliases) & nzchar(aliases)
  for (i in which(alias_usable & !is.na(source_types) &
                  !source_types %in% c("BASE", "BASE_NO_VALUE"))) {
    base <- which(!is.na(ids) & ids == aliases[i] & source_types == "BASE")
    if (length(base) == 1L && aliases[i] %in% field(variables[[i]], "deriveSources")) {
      alias_usable[i] <- FALSE
    }
  }

  resolve <- function(raw_id) {
    if (is.na(raw_id)) return(NA_integer_)
    candidate <- which(alias_usable & aliases == raw_id)
    if (length(candidate) == 0L) candidate <- which(!is.na(ids) & ids == raw_id)
    if (length(candidate) != 1L || is.na(ids[candidate]) ||
        sum(ids == ids[candidate], na.rm = TRUE) != 1L) return(NA_integer_)
    candidate
  }

  annotate <- function(payload, inherit_status = FALSE) {
    if (!is.character(payload) || length(payload) != 1L || is.na(payload)) return(payload)
    # Ordinary units do not need another JSON parse. Still reset an embedded
    # provenance marker instead of trusting data supplied in the payload.
    if (!any(transforms_nr) && !grepl("coding_from_not_reached", payload, fixed = TRUE)) {
      return(payload)
    }
    records <- tryCatch(jsonlite::parse_json(payload), error = function(e) NULL)
    if (!is.list(records) || !is.null(names(records)) ||
        !all(vapply(records, is.list, logical(1)))) return(payload)
    positions <- vapply(records, function(x) resolve(scalar_string(field(x, "id"))), integer(1))

    # Repeated responses resolving to the same variable in the same subform
    # have no unambiguous provenance. Keep the coder's existing handling.
    subforms <- vapply(records, function(x) {
      subform <- scalar_string(field(x, "subform"))
      if (is.na(subform)) "" else subform
    }, character(1))
    keys <- data.frame(position = positions, subform = subforms)
    ambiguous <- duplicated(keys) | duplicated(keys, fromLast = TRUE)

    for (i in seq_along(records)) {
      records[[i]] <- records[[i]][names(records[[i]]) != "coding_from_not_reached"]
      # An overlay without a status inherits the original record's status
      # and its freshly computed provenance. Explicit null is different:
      # it overwrites the original status and must receive an unknown flag.
      if (inherit_status && !"status" %in% names(records[[i]])) next
      position <- positions[i]
      status <- scalar_string(field(records[[i]], "status"))
      flag <- NA
      if (!is.na(position) && !ambiguous[i] && !is.na(source_types[position])) {
        if (source_types[position] != "BASE" || !transforms_nr[position]) {
          flag <- FALSE
        } else if (!is.na(status)) {
          flag <- identical(status, "NOT_REACHED")
        }
      }
      records[[i]]$coding_from_not_reached <- flag
    }
    # Seventeen significant digits preserve the double values that JSON.parse
    # supplies to the coder. jsonlite's digits = NA rounds to fifteen digits
    # and can change exact numeric matching in a coding rule.
    as.character(jsonlite::toJSON(records, auto_unbox = TRUE, null = "null",
                                 na = "null", digits = 17))
  }

  columns <- c("responses", "geometry_variables")
  if (!manual_provenance_prepared) columns <- c(columns, "manual")
  for (column in intersect(columns, names(unit_responses))) {
    # Geometry and manual overlays are merged into the raw response before
    # coding. Only manual payloads prepared by code_responses() already carry
    # the status provenance from before its promotion of manual missing codes.
    unit_responses[[column]] <- vapply(unit_responses[[column]], annotate, character(1),
                                      inherit_status = column != "responses",
                                      USE.NAMES = FALSE)
  }
  unit_responses
}
