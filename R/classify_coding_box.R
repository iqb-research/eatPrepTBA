# Item-matrix semantics pinned to iqb-berlin/coding-box 39468e5a28a24dc9ec860aee46daf8d4ed5c8682.
# This translates its cell resolver to the complete variable-level long table;
# it never constructs a matrix or invents rows for missing-by-design items.
classify_coding_box <- function(data, metadata, profile,
                                recode_omissions_to_not_reached = FALSE,
                                not_reached_scope = "unit",
                                identifiers = c("group_id", "login_name", "login_code")) {
  checkmate::assert_tibble(data)
  checkmate::assert_tibble(metadata)
  checkmate::assert_tibble(profile)
  checkmate::assert_flag(recode_omissions_to_not_reached)
  not_reached_scope <- match.arg(not_reached_scope, c("unit", "testlet", "booklet"))
  if (recode_omissions_to_not_reached && not_reached_scope == "unit") {
    cli::cli_abort("The coding_box policy permits trailing-omission recoding only with testlet or booklet scope.")
  }
  assert_cols(data, c("booklet_id", "testlet_no", "unit_booklet_no", "unit_key",
                      "variable_id", "code_id", "code_score", "code_type",
                      "code_status", "box_position", "box_included"), "data")
  assert_cols(metadata, c("unit_key", "variable_id"), "metadata")
  assert_cols(profile, c("code_type", "code_id", "code_score"), "profile")
  identifiers <- intersect(identifiers, names(data))
  if (!length(identifiers)) cli::cli_abort("No person identifier is available for the coding_box policy.")
  checkmate::assert_logical(data$box_included, len = nrow(data))
  checkmate::assert_numeric(data$box_position, len = nrow(data))
  checkmate::assert_numeric(data$code_id)
  checkmate::assert_numeric(data$code_score)

  missing_types <- c(mir = "MISSING_INVALID_RESPONSE", mci = "MISSING_CODING_IMPOSSIBLE",
                     mbi_mbo = "MISSING_BY_OMISSION", mnr = "MISSING_NOT_REACHED",
                     mbd = "MISSING_BY_DESIGN")
  selected <- match(unname(missing_types), profile$code_type)
  if (anyNA(selected)) cli::cli_abort("The coding_box policy requires invalid, coding-impossible, omission, not-reached, and missing-by-design profile entries.")
  missing_profile <- profile[selected, c("code_type", "code_id", "code_score")]
  if (anyNA(missing_profile$code_id) || any(!is.finite(missing_profile$code_id)) ||
      any(missing_profile$code_id >= 0) || anyDuplicated(missing_profile$code_id) ||
      any(missing_profile$code_id %in% c(-1, -2, -3, -4, -111))) {
    cli::cli_abort("The coding_box missing profile needs distinct negative IDs excluding reserved technical codes -1, -2, -3, -4, and -111.")
  }
  if (any(!is.na(missing_profile$code_score) & !is.finite(missing_profile$code_score))) {
    cli::cli_abort("Missing-profile scores must be finite numbers or NA.")
  }

  index <- dplyr::left_join(
    data[c("unit_key", "variable_id")], metadata,
    by = c("unit_key", "variable_id"), relationship = "many-to-one"
  )
  n <- nrow(data)
  source_type <- if ("variable_source_type" %in% names(index)) as.character(index$variable_source_type) else
    if ("sourceType" %in% names(index)) as.character(index$sourceType) else rep(NA_character_, n)
  source_ids <- if ("source_ids" %in% names(index)) index$source_ids else rep(list(character()), n)
  basis <- !is.na(source_type) & grepl("^(BASE|BASIS)", source_type)
  if ("variable_level" %in% names(index)) {
    basis <- basis | (is.na(source_type) & !is.na(index$variable_level) & index$variable_level == 0)
  }
  derived <- !basis & (!is.na(source_type) |
                        vapply(source_ids, length, integer(1)) > 0L)
  included <- data$box_included %in% TRUE & !is.na(data$box_position)
  direct_item <- if ("box_is_item" %in% names(data)) data$box_is_item %in% TRUE else included
  present <- if ("response_present" %in% names(data)) data$response_present %in% TRUE else rep(TRUE, n)
  numeric_result <- present & (!is.na(data$code_id) | !is.na(data$code_score))
  state <- rep("error", n)
  cell_id <- data$code_id
  cell_score <- data$code_score
  activity <- rep(TRUE, n)
  candidate <- omission <- rep(FALSE, n)
  failure <- rep(NA_character_, n)
  reasons <- rep(NA_character_, n)

  set_missing <- function(rows, category) {
    if (!length(rows)) return(invisible(NULL))
    p <- match(category, names(missing_types))
    state[rows] <<- category
    cell_id[rows] <<- missing_profile$code_id[[p]]
    cell_score[rows] <<- missing_profile$code_score[[p]]
    activity[rows] <<- !category %in% c("mnr", "mbd")
    candidate[rows] <<- FALSE
    omission[rows] <<- category == "mbi_mbo"
    failure[rows] <<- NA_character_
    invisible(NULL)
  }
  score_equal <- function(left, right) {
    (is.na(left) && is.na(right)) || (!is.na(left) && !is.na(right) && left == right)
  }
  for (i in seq_len(n)) {
    if (numeric_result[[i]]) {
      if (data$code_id[[i]] %in% c(-3, -4)) {
        set_missing(i, if (data$code_id[[i]] == -3) "mir" else "mci")
        next
      }
      p <- match(data$code_id[[i]], missing_profile$code_id)
      if (!is.na(p) && score_equal(data$code_score[[i]], missing_profile$code_score[[p]])) {
        set_missing(i, names(missing_types)[[p]])
      } else if ((!is.na(data$code_id[[i]]) && data$code_id[[i]] >= 0) ||
                 (is.na(data$code_id[[i]]) && !is.na(data$code_score[[i]]))) {
        state[[i]] <- "valid"
      }
      next
    }
    status <- as.character(data$code_status[[i]])
    if (present[[i]] && status %in% "INVALID") {
      set_missing(i, "mir")
    } else if (present[[i]] && status %in% "CODING_ERROR") {
      set_missing(i, "mci")
    } else if (present[[i]] && status %in% c("UNSET", "DISPLAYED", "PARTLY_DISPLAYED")) {
      set_missing(i, "mbi_mbo")
    } else if (!present[[i]] || status %in% "NOT_REACHED") {
      state[[i]] <- "mnr"
      cell_id[[i]] <- cell_score[[i]] <- NA_real_
      candidate[[i]] <- TRUE
      activity[[i]] <- FALSE
    } else {
      failure[[i]] <- "unresolved-status"
    }
  }

  person_keys <- data[c(identifiers, "booklet_id")]
  person_keys$booklet_id <- toupper(person_keys$booklet_id)
  if ("booklet_no" %in% names(data)) person_keys$booklet_no <- data$booklet_no
  scope_keys <- person_keys
  if (not_reached_scope != "booklet") scope_keys$testlet_no <- data$testlet_no
  if (not_reached_scope == "unit") {
    scope_keys$unit_booklet_no <- data$unit_booklet_no
    scope_keys$unit_key <- data$unit_key
    if ("unit_alias" %in% names(data)) scope_keys$unit_alias <- data$unit_alias
  }
  scopes <- dplyr::group_indices(dplyr::group_by(scope_keys, dplyr::across(dplyr::everything())))
  # The reference's design.units.order is a global booklet position. Equivalent
  # R designs may restart unit_booklet_no per testlet, so translate the expected
  # testlet/unit sequence to that global ordinal before sorting or grouping.
  unit_rank <- as.integer(dplyr::dense_rank(tibble::tibble(
    testlet_no = ifelse(is.na(data$testlet_no), Inf, data$testlet_no),
    unit_booklet_no = data$unit_booklet_no
  )))
  for (rows in split(which(included), scopes[included])) {
    rows <- rows[order(unit_rank[rows], data$box_position[rows], decreasing = TRUE)]
    position <- length(rows)
    later_activity <- FALSE
    # Traverse groups from latest to earliest. Equal positions see only activity
    # at strictly later positions, including activity from derived results.
    cursor <- 1L
    while (cursor <= position) {
      end <- cursor
      while (end < position &&
             unit_rank[[rows[end + 1L]]] == unit_rank[[rows[cursor]]] &&
             data$box_position[[rows[end + 1L]]] == data$box_position[[rows[cursor]]]) {
        end <- end + 1L
      }
      at_position <- rows[cursor:end]
      for (i in at_position) {
        if (candidate[[i]]) {
          set_missing(i, if (later_activity) "mbi_mbo" else "mnr")
        } else if (omission[[i]] && recode_omissions_to_not_reached && !later_activity) {
          set_missing(i, "mnr")
        }
      }
      later_activity <- later_activity || any(activity[at_position])
      cursor <- end + 1L
    }
  }

  occurrence_keys <- person_keys
  occurrence_keys$testlet_no <- data$testlet_no
  occurrence_keys$unit_booklet_no <- data$unit_booklet_no
  occurrence_keys$unit_key <- data$unit_key
  if ("unit_alias" %in% names(data)) occurrence_keys$unit_alias <- data$unit_alias
  occurrences <- dplyr::group_indices(dplyr::group_by(occurrence_keys, dplyr::across(dplyr::everything())))
  partial_types <- c("SUM_CODE", "SUM_SCORE", "CONCAT_CODE")
  for (rows in split(seq_len(n), occurrences)) {
    ids <- data$variable_id[rows]
    if (anyDuplicated(ids)) cli::cli_abort("Duplicate variable occurrences in coding_box input.")
    resolve_sources <- function(i, path = integer()) {
      if (i %in% path) {
        return(list(state = "error", failure = "derived-cycle", valid = FALSE,
                    omitted = FALSE, ineligible = TRUE))
      }
      sources <- source_ids[[i]]
      if (!length(sources)) {
        category <- if (included[[i]] || present[[i]]) state[[i]] else "error"
        return(list(state = category, failure = if (category == "error") "derived-source-unresolved" else NA_character_,
                    valid = category == "valid", omitted = category == "mbi_mbo",
                    ineligible = !category %in% c("valid", "mbi_mbo")))
      }
      children <- lapply(sources, function(id) {
        j <- rows[match(id, ids)]
        if (is.na(j)) {
          return(list(state = "error", failure = "derived-source-unresolved", valid = FALSE,
                      omitted = FALSE, ineligible = TRUE))
        }
        if (!numeric_result[[j]] && length(source_ids[[j]]) > 0L) {
          return(resolve_sources(j, c(path, i)))
        }
        category <- if (included[[j]] || present[[j]]) state[[j]] else "error"
        list(state = category,
             failure = if (category == "error") "derived-source-unresolved" else failure[[j]],
             valid = category == "valid", omitted = category == "mbi_mbo",
             ineligible = !category %in% c("valid", "mbi_mbo"))
      })
      states <- vapply(children, `[[`, character(1), "state")
      failures <- vapply(children, `[[`, character(1), "failure")
      category <- coding_box_aggregate_states(states)
      why <- if (any(failures %in% "derived-cycle")) "derived-cycle" else
        if (any(states == "error")) "derived-source-unresolved" else
          if (category == "error" && any(states == "mbd")) "derived-design-conflict" else
            if (category == "error") "derived-source-unresolved" else NA_character_
      list(state = if (!is.na(why)) "error" else category, failure = why,
           valid = any(vapply(children, `[[`, logical(1), "valid")),
           omitted = any(vapply(children, `[[`, logical(1), "omitted")),
           ineligible = any(vapply(children, `[[`, logical(1), "ineligible")))
    }
    for (i in rows[derived[rows] & !numeric_result[rows] & included[rows] & direct_item[rows]]) {
      resolved <- resolve_sources(i)
      if (source_type[[i]] %in% partial_types && resolved$state == "valid" &&
          resolved$valid && (resolved$omitted || resolved$ineligible) && is.na(resolved$failure)) {
        set_missing(i, "mir")
      } else if (resolved$state %in% names(missing_types)) {
        set_missing(i, resolved$state)
      } else {
        state[[i]] <- "error"
        cell_id[[i]] <- cell_score[[i]] <- NA_real_
        failure[[i]] <- if (resolved$state == "valid") "derived-result-missing" else resolved$failure
        activity[[i]] <- TRUE
        omission[[i]] <- candidate[[i]] <- FALSE
      }
    }
  }

  result <- data
  resolved_missing <- included & state %in% names(missing_types)
  for (category in names(missing_types)) {
    rows <- which(resolved_missing & state == category)
    result$code_type[rows] <- unname(missing_types[[category]])
    result$code_id[rows] <- cell_id[rows]
    result$code_score[rows] <- cell_score[rows]
  }
  unresolved <- included & state == "error" & !numeric_result
  # An unknown technical state is not a classified missing. Preserve technical
  # metadata, but do not export an analytical category supplied by another policy.
  result$code_type[unresolved] <- NA_character_
  result$code_id[unresolved] <- cell_id[unresolved]
  result$code_score[unresolved] <- cell_score[unresolved]
  failure[included & state == "valid" & is.na(cell_id)] <- "missing-code"
  failure[included & state == "valid" & is.na(cell_score)] <- "missing-score"
  failure[included & state == "error" & cell_id %in% c(-1, -2, -111)] <- "invalid-code"
  failure[included & state == "error" & !is.na(cell_id) &
            !cell_id %in% c(-1, -2, -111)] <- "unrecognized-negative-code"
  failure[!included] <- "excluded-variable"
  reasons[!included] <- "order"
  reasons[included & !is.na(failure)] <- ifelse(
    grepl("^derived-", failure[included & !is.na(failure)]), "sources", "order")
  list(data = result, reasons = reasons, basis = basis, diagnostics = failure)
}

# Equivalent to the box pair table: errors dominate; missing-by-design only
# combines with itself; coding-impossible then valid then invalid then NR then O.
coding_box_aggregate_states <- function(states) {
  if (!length(states) || anyNA(states) || any(states == "error")) return("error")
  if (any(states == "mbd")) return(if (all(states == "mbd")) "mbd" else "error")
  for (category in c("mci", "valid", "mir", "mnr", "mbi_mbo")) {
    if (any(states == category)) return(category)
  }
  "error"
}
