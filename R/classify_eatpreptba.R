# The standard long-format policy uses partial presentation relationships and
# sources with priority over automatically generated derived activity.
classify_eatpreptba <- function(data, metadata, profile, input_profile = profile,
                                precedence = NULL,
                                recode_omissions_to_not_reached = FALSE,
                                not_reached_scope = "testlet",
                                recode_existing_not_reached = FALSE,
                                derived_not_reached = "recode",
                                identifiers = c("group_id", "login_name", "login_code"),
                                progress = FALSE) {
  checkmate::assert_tibble(data)
  checkmate::assert_tibble(metadata)
  checkmate::assert_tibble(profile)
  checkmate::assert_data_frame(input_profile)
  checkmate::assert_flag(recode_omissions_to_not_reached)
  checkmate::assert_flag(recode_existing_not_reached)
  checkmate::assert_flag(progress)
  progress_session <- missing_progress_session_start(progress)
  on.exit(missing_progress_session_done(progress_session), add = TRUE)
  not_reached_scope <- match.arg(not_reached_scope, c("unit", "testlet", "booklet"))
  derived_not_reached <- match.arg(derived_not_reached, c("recode", "preserve"))
  assert_cols(data, c("booklet_id", "testlet_no", "unit_booklet_no", "unit_key",
                      "variable_id", "value", "code_status", "code_type", "code_id", "code_score"), "data")
  assert_cols(metadata, c("unit_key", "variable_id", "basis_sources", "sources_known"), "metadata")
  assert_cols(profile, c("code_type", "code_id", "code_score"), "profile")
  assert_cols(input_profile, c("code_type", "code_id"), "input_profile")
  identifiers <- intersect(identifiers, names(data))
  if (!length(identifiers)) cli::cli_abort("No person identifier is available for the eatPrepTBA policy.")
  checkmate::assert_numeric(data$code_id)
  checkmate::assert_numeric(data$code_score)
  n <- nrow(data)
  # Keep source lists on the small static table. The person-expanded table
  # needs only integer references, not a repeated copy of every metadata field.
  metadata_keys <- metadata[c("unit_key", "variable_id")]
  metadata_keys$.metadata_row <- seq_len(nrow(metadata))
  metadata_row <- dplyr::left_join(data[c("unit_key", "variable_id")], metadata_keys,
                                  by = c("unit_key", "variable_id"), relationship = "many-to-one")$.metadata_row
  static_source_type <- if ("variable_source_type" %in% names(metadata)) as.character(metadata$variable_source_type) else rep(NA_character_, nrow(metadata))
  static_basis <- !is.na(static_source_type) & grepl("^(BASE|BASIS)", static_source_type)
  if ("variable_level" %in% names(metadata)) {
    static_basis <- static_basis | (is.na(static_source_type) & !is.na(metadata$variable_level) & metadata$variable_level == 0)
  }
  source_type <- static_source_type[metadata_row]
  basis <- static_basis[metadata_row] %in% TRUE
  source_ids <- if ("source_ids" %in% names(metadata)) metadata$source_ids else metadata$basis_sources
  source_ids[static_basis] <- rep(list(character()), sum(static_basis))
  source_count <- lengths(source_ids)[metadata_row]
  included <- basis | if ("analysis_included" %in% names(data)) data$analysis_included %in% TRUE else rep(TRUE, n)
  present <- if ("response_present" %in% names(data)) data$response_present %in% TRUE else rep(TRUE, n)
  has_value <- if (is.list(data$value)) vapply(data$value, function(value) {
    length(value) > 0L && any(!is.na(value))
  }, logical(1), USE.NAMES = FALSE) else !is.na(data$value[seq_len(n)])
  numeric_result <- !is.na(data$code_id) | !is.na(data$code_score)
  omission <- "MISSING_BY_OMISSION"
  nr <- "MISSING_NOT_REACHED"
  invalid <- "MISSING_INVALID_RESPONSE"
  process_types <- c("MISSING_CODING_IMPOSSIBLE", "NO_CODING", "INTENDED_INCOMPLETE",
                      "CODING_INCOMPLETE", "DERIVE_PENDING", "CODE_SELECTION_PENDING")
  protected_process_types <- setdiff(process_types, "DERIVE_PENDING")
  original_types <- as.character(data$code_type)
  raw_types <- original_types
  decoder <- unique(input_profile[c("code_type", "code_id")])
  decoder <- decoder[!is.na(decoder$code_id) & decoder$code_id < 0, , drop = FALSE]
  duplicate <- duplicated(decoder$code_id) | duplicated(decoder$code_id, fromLast = TRUE)
  ambiguous_input_ids <- decoder$code_id[duplicate]
  decoder <- decoder[!duplicate, , drop = FALSE]
  from_id <- is.na(raw_types) & data$code_id %in% decoder$code_id
  raw_types[from_id] <- decoder$code_type[match(data$code_id[from_id], decoder$code_id)]
  unknown_negative <- is.na(raw_types) & !is.na(data$code_id) & data$code_id < 0
  # Zero is a valid numeric result when no negative missing ID or explicit
  # analytical missing type identifies it. A missing's score never proves work.
  untyped_valid <- is.na(raw_types) &
    ((!is.na(data$code_id) & data$code_id >= 0) |
       (is.na(data$code_id) & !is.na(data$code_score)))
  status_map <- c(
    UNSET = omission, DISPLAYED = omission, PARTLY_DISPLAYED = omission,
    NOT_REACHED = nr, INVALID = invalid, DERIVE_ERROR = invalid,
    CODING_ERROR = "MISSING_CODING_IMPOSSIBLE", NO_CODING = "NO_CODING",
    INTENDED_INCOMPLETE = "INTENDED_INCOMPLETE", CODING_INCOMPLETE = "CODING_INCOMPLETE",
    DERIVE_PENDING = "DERIVE_PENDING", CODE_SELECTION_PENDING = "CODE_SELECTION_PENDING"
  )
  mapped_status <- unname(status_map[as.character(data$code_status)])
  from_status <- is.na(raw_types) & !untyped_valid & !unknown_negative & !is.na(mapped_status)
  raw_types[from_status] <- mapped_status[from_status]
  valid_type <- !is.na(raw_types) & !startsWith(dplyr::coalesce(raw_types, ""), "MISSING_") &
    !raw_types %in% c(process_types, omission, nr, invalid)
  valid <- valid_type | untyped_valid
  unknown_empty <- basis & is.na(raw_types) & !numeric_result & !has_value &
    (!present | is.na(data$code_status))
  candidate <- included & ((basis & raw_types %in% nr &
                              (recode_existing_not_reached | !numeric_result)) |
                             (basis & recode_omissions_to_not_reached & raw_types %in% omission) |
                             unknown_empty)
  normal_activity <- included & (valid | raw_types %in% invalid |
                                  (has_value & !raw_types %in% c(omission, nr, "MISSING_BY_DESIGN")) |
                                  (!recode_omissions_to_not_reached & raw_types %in% omission))

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
  group_ids <- dplyr::group_indices(dplyr::group_by(scope_keys, dplyr::across(dplyr::everything())))
  scope_rows <- split(seq_len(n), group_ids)
  occurrence_keys <- person_keys
  for (column in intersect(c("testlet_no", "unit_booklet_no", "unit_key", "unit_alias"), names(data))) {
    occurrence_keys[[column]] <- data[[column]]
  }
  occurrences <- dplyr::group_indices(dplyr::group_by(occurrence_keys, dplyr::across(dplyr::everything())))
  occurrence_rows <- split(seq_len(n), occurrences)
  basis_rows <- rep(list(integer()), n)
  local_before <- vector("list", length(occurrence_rows))
  local_sources <- vector("list", length(occurrence_rows))
  static_cache <- new.env(parent = emptyenv())
  static_fields <- intersect(c("booklet_id", "testlet_no", "unit_booklet_no", "unit_key", "unit_alias"), names(data))
  occurrence_starts <- vapply(occurrence_rows, `[[`, integer(1), 1L)
  static_keys <- data[occurrence_starts, static_fields, drop = FALSE]
  static_keys$booklet_id <- toupper(static_keys$booklet_id)
  static_ids <- dplyr::group_indices(dplyr::group_by(static_keys, dplyr::across(dplyr::everything())))
  unit_rank <- as.integer(dplyr::dense_rank(tibble::tibble(
    testlet_no = ifelse(is.na(data$testlet_no), Inf, data$testlet_no),
    unit_booklet_no = data$unit_booklet_no
  )))
  if (is.null(precedence)) precedence <- attr(data, "design_precedence")
  # Standard precedence has the same occurrence keys throughout. Match it in
  # one join; retain the original matching loop for partial/custom key sets.
  indexed_precedence <- length(precedence) > 0L && all(vapply(precedence,
    function(entry) identical(nrow(entry$keys), 1L) && setequal(names(entry$keys), static_fields) &&
      all(vapply(static_fields, function(column) identical(class(entry$keys[[column]]),
                                                          class(static_keys[[column]])), logical(1))), logical(1)))
  if (indexed_precedence) {
    precedence_keys <- dplyr::bind_rows(lapply(precedence, `[[`, "keys"))
    precedence_keys$booklet_id <- toupper(precedence_keys$booklet_id)
    precedence_keys$.precedence_row <- seq_along(precedence)
    precedence_keys <- dplyr::distinct(precedence_keys, dplyr::across(dplyr::all_of(static_fields)), .keep_all = TRUE)
    precedence_rows <- dplyr::left_join(static_keys, precedence_keys, by = static_fields,
                                       relationship = "many-to-one")$.precedence_row
  }
  progress_id <- missing_progress_start("Prepare source relationships", length(occurrence_rows), progress)
  on.exit(missing_progress_done(progress_id), add = TRUE)
  for (k in seq_along(occurrence_rows)) {
    rows <- occurrence_rows[[k]]
    ids <- data$variable_id[rows]
    if (anyDuplicated(ids)) cli::cli_abort("Duplicate variable occurrences in eatPrepTBA input.")
    cache_key <- as.character(static_ids[[k]])
    cache_entries <- if (exists(cache_key, envir = static_cache, inherits = FALSE))
      get(cache_key, envir = static_cache, inherits = FALSE) else list()
    cached_at <- which(vapply(cache_entries, function(entry) identical(entry$ids, ids), logical(1)))
    cached <- length(cached_at) > 0L
    cached_order <- if (cached) cache_entries[[cached_at[[1L]]]] else NULL
    before <- if (cached) cached_order$before else
      matrix(FALSE, length(rows), length(rows))
    known_order <- FALSE
    if (!cached && length(precedence)) {
      entries <- if (indexed_precedence) precedence[stats::na.omit(precedence_rows[[k]])] else precedence
      for (entry in entries) {
        fields <- intersect(names(entry$keys), names(data))
        matches <- vapply(fields, function(column) {
          a <- entry$keys[[column]][[1L]]
          b <- data[[column]][[rows[[1L]]]]
          if (column == "booklet_id") { a <- toupper(a); b <- toupper(b) }
          (is.na(a) && is.na(b)) || (!is.na(a) && !is.na(b) && a == b)
        }, logical(1))
        if (!length(matches) || !all(matches)) next
        at <- match(ids, entry$variable_ids)
        if (anyNA(at)) cli::cli_abort("Static precedence does not cover every variable occurrence.")
        before <- entry$before[at, at, drop = FALSE]
        before[is.na(before)] <- FALSE
        known_order <- TRUE
        break
      }
    }
    if (!cached && !known_order) {
      position_column <- intersect(c("position_group", "box_position"), names(data))
      if (length(position_column)) {
        positions <- data[[position_column[[1L]]]][rows]
        before <- outer(positions, positions, `<`)
        before[is.na(before)] <- FALSE
      }
    }
    if (!cached) {
      source_indices <- lapply(source_ids[metadata_row[rows]], match, table = ids)
      basis_indices <- rep(list(integer()), length(rows))
      for (at in which(!basis[rows])) {
        i <- rows[[at]]
        m <- metadata_row[[i]]
        if (is.na(m) || !isTRUE(metadata$sources_known[[m]])) next
        sources <- metadata$basis_sources[[m]]
        matched <- match(sources, ids)
        if (length(sources) && !anyNA(sources) && !anyNA(matched) && all(basis[rows[matched]])) {
          basis_indices[[at]] <- matched
        }
      }
      cached_order <- list(ids = ids, before = before, sources = source_indices, bases = basis_indices)
      assign(cache_key, c(cache_entries, list(cached_order)), envir = static_cache)
    }
    local_before[[k]] <- before
    local_sources[[k]] <- cached_order$sources
    basis_rows[rows] <- lapply(cached_order$bases, function(at) rows[at])
    # All sources needed by an item participate, including ambiguous shared
    # sources. Such a source can still establish work in its unit without a
    # fabricated intra-unit position.
    if (any(!included[rows])) {
      pending <- which(if ("box_is_item" %in% names(data)) data$box_is_item[rows] %in% TRUE else included[rows])
      visited <- rep(FALSE, length(rows))
      while (length(pending)) {
        visited[pending] <- TRUE
        children <- unlist(cached_order$sources[pending], use.names = FALSE)
        pending <- unique(children[!is.na(children) & !visited[children]])
      }
      included[rows[visited]] <- TRUE
    }
    missing_progress_update(progress_id)
  }
  missing_progress_done(progress_id)
  normal_activity <- normal_activity | (included & !basis & !normal_activity &
                                        (valid | raw_types %in% invalid |
                                           (has_value & !raw_types %in% c(omission, nr, "MISSING_BY_DESIGN")) |
                                           (!recode_omissions_to_not_reached & raw_types %in% omission)))
  candidate <- included & ((basis & raw_types %in% nr &
                              (recode_existing_not_reached | !numeric_result)) |
                             (basis & recode_omissions_to_not_reached & raw_types %in% omission) |
                             unknown_empty)
  potential <- rep(FALSE, n)
  for (i in which(included & !basis &
                   ((!numeric_result & raw_types %in% invalid) |
                      (derived_not_reached == "recode" & numeric_result & (valid | raw_types %in% invalid))))) {
    sources <- basis_rows[[i]]
    missing_sources <- raw_types[sources] %in% nr | unknown_empty[sources] |
      (recode_omissions_to_not_reached & raw_types[sources] %in% omission)
    potential[[i]] <- length(sources) > 0L && all(missing_sources)
  }

  evaluate_order <- function(activity) {
    before_work <- tail <- rep(FALSE, n)
    order_progress <- missing_progress_start("Find last reached positions", length(scope_rows), progress)
    on.exit(missing_progress_done(order_progress), add = TRUE)
    for (rows in scope_rows) {
      anchors <- rows[activity[rows]]
      if (!length(anchors)) {
        tail[rows] <- TRUE
        missing_progress_update(order_progress)
        next
      }
      last_unit <- max(unit_rank[anchors])
      before_work[rows[unit_rank[rows] < last_unit]] <- TRUE
      tail[rows[unit_rank[rows] > last_unit]] <- TRUE
      locals <- rows[unit_rank[rows] == last_unit]
      for (k in unique(occurrences[locals])) {
        current <- occurrence_rows[[k]]
        local_rows <- intersect(locals, current)
        local_anchors <- intersect(anchors, current)
        if (!length(local_anchors)) next
        at <- match(local_rows, current)
        anchor_at <- match(local_anchors, current)
        relation <- local_before[[k]]
        before_work[local_rows] <- rowSums(relation[at, anchor_at, drop = FALSE]) > 0L
        tail[local_rows] <- colSums(relation[anchor_at, at, drop = FALSE]) == length(anchor_at)
      }
      before_work[anchors] <- TRUE
      missing_progress_update(order_progress)
    }
    list(before = before_work, tail = tail)
  }
  classify_candidates <- function(evidence) {
    types <- raw_types
    types[candidate & evidence$before] <- omission
    types[candidate & evidence$tail] <- nr
    types
  }
  # Evidence is restored monotonically. A derived result is withheld only
  # while its complete basis-source tree supports the proposed NR override.
  # This prevents an automatically generated zero from proving its own reach.
  withheld <- potential
  repeat {
    evidence <- evaluate_order(normal_activity & !withheld)
    types <- classify_candidates(evidence)
    release <- rep(FALSE, n)
    for (i in which(withheld)) {
      sources <- basis_rows[[i]]
      release[[i]] <- !all(types[sources] %in% nr & evidence$tail[sources])
    }
    if (!any(release)) break
    withheld[release] <- FALSE
  }
  overridden <- withheld
  types[overridden] <- nr
  # An omission on an item is constrained by its sources, rather than by the
  # artificial insertion point of its derived variable in the display order.
  for (i in which(included & !basis & raw_types %in% omission & recode_omissions_to_not_reached)) {
    sources <- basis_rows[[i]]
    if (length(sources) && all(types[sources] %in% nr & evidence$tail[sources])) {
      types[[i]] <- nr
      overridden[[i]] <- TRUE
    }
  }
  # Existing derived NR is likewise protected unless explicitly requested.
  # Correction then requires the complete sources to precede later work.
  for (i in which(included & !basis & numeric_result & raw_types %in% nr & recode_existing_not_reached)) {
    sources <- basis_rows[[i]]
    if (length(sources) && all(evidence$before[sources])) types[[i]] <- omission
  }
  reasons <- diagnostics <- rep(NA_character_, n)
  ambiguous <- candidate & !evidence$before & !evidence$tail
  reasons[ambiguous] <- "order"
  diagnostics[ambiguous] <- "ambiguous-position"
  reasons[!included] <- "order"
  diagnostics[!included] <- "excluded-variable"
  numeric_ids <- data$code_id
  numeric_scores <- data$code_score
  p <- match(types, profile$code_type)
  # Basis missings use the output schema. Existing derived coding results are
  # protected; all source-supported NR overrides use the complete schema.
  apply_profile <- !is.na(p) & included &
    (basis | overridden | (!basis & types %in% c(omission, nr, "MISSING_BY_DESIGN")) |
       (!basis & is.na(reasons) & candidate &
                            (is.na(raw_types) | types != raw_types)))
  numeric_ids[apply_profile] <- profile$code_id[p[apply_profile]]
  numeric_scores[apply_profile] <- profile$code_score[p[apply_profile]]

  missing_states <- c(MISSING_INVALID_RESPONSE = "mir", MISSING_CODING_IMPOSSIBLE = "mci",
                      MISSING_BY_OMISSION = "mbi_mbo", MISSING_NOT_REACHED = "mnr",
                      MISSING_BY_DESIGN = "mbd")
  states <- unname(missing_states[types])
  states[is.na(states) & valid] <- "valid"
  states[is.na(states)] <- "error"
  progress_id <- missing_progress_start("Classify derived missings", length(occurrence_rows), progress)
  for (k in seq_along(occurrence_rows)) {
    rows <- occurrence_rows[[k]]
    source_indices <- local_sources[[k]]
    resolve_sources <- function(at, path = integer()) {
      if (at %in% path) return(list(state = "error", failure = "derived-cycle", valid = FALSE, missing = TRUE))
      i <- rows[[at]]
      sources <- source_indices[[at]]
      if (!length(sources)) {
        return(list(state = states[[i]], failure = if (states[[i]] == "error") "derived-source-unresolved" else NA_character_,
                    valid = states[[i]] == "valid", missing = !states[[i]] %in% c("valid", "error")))
      }
      children <- lapply(sources, function(child) {
        j <- rows[child]
        if (is.na(j)) return(list(state = "error", failure = "derived-source-unresolved", valid = FALSE, missing = TRUE))
        if (!basis[[j]] && !numeric_result[[j]] &&
            !raw_types[[j]] %in% protected_process_types && isTRUE(source_count[[j]] > 0L)) {
          return(resolve_sources(child, c(path, at)))
        }
        list(state = states[[j]], failure = if (states[[j]] == "error") "derived-source-unresolved" else NA_character_,
             valid = states[[j]] == "valid", missing = !states[[j]] %in% c("valid", "error"))
      })
      source_states <- vapply(children, `[[`, character(1), "state")
      failures <- vapply(children, `[[`, character(1), "failure")
      category <- coding_box_aggregate_states(source_states)
      failure <- if (any(failures %in% "derived-cycle")) "derived-cycle" else
        if (any(source_states == "error")) "derived-source-unresolved" else
          if (category == "error" && any(source_states == "mbd")) "derived-design-conflict" else
            if (category == "error") "derived-source-unresolved" else NA_character_
      list(state = if (is.na(failure)) category else "error", failure = failure,
           valid = any(vapply(children, `[[`, logical(1), "valid")),
           missing = any(vapply(children, `[[`, logical(1), "missing")))
    }
    for (at in which(!basis[rows] & included[rows] & !numeric_result[rows] &
                   !raw_types[rows] %in% protected_process_types)) {
      i <- rows[[at]]
      resolved <- resolve_sources(at)
      category <- resolved$state
      if (source_type[[i]] %in% c("SUM_CODE", "SUM_SCORE", "CONCAT_CODE") &&
          category == "valid" && resolved$valid && resolved$missing && is.na(resolved$failure)) {
        category <- "mir"
      }
      target_type <- names(missing_states)[match(category, missing_states)]
      if (!is.na(target_type)) {
        types[[i]] <- target_type
        p <- match(target_type, profile$code_type)
        if (is.na(p)) cli::cli_abort("No output-profile entry for a classified derived missing.")
        numeric_ids[[i]] <- profile$code_id[[p]]
        numeric_scores[[i]] <- profile$code_score[[p]]
        states[[i]] <- category
        reasons[[i]] <- diagnostics[[i]] <- NA_character_
      } else {
        types[[i]] <- NA_character_
        numeric_ids[[i]] <- numeric_scores[[i]] <- NA_real_
        reasons[[i]] <- "sources"
        diagnostics[[i]] <- if (category == "valid") "derived-result-missing" else resolved$failure
      }
    }
    missing_progress_update(progress_id)
  }
  missing_progress_done(progress_id)
  # Source information that is insufficient to justify a change must not erase
  # an existing result, including a positive or invalid derived result.
  for (i in which(included & !basis & numeric_result & (valid | raw_types %in% invalid) & !overridden)) {
    if (derived_not_reached == "recode" && !length(basis_rows[[i]])) {
      reasons[[i]] <- "sources"
      diagnostics[[i]] <- "incomplete-basis-sources"
    }
  }
  unknown <- included & is.na(types) & !valid & is.na(diagnostics)
  diagnostics[unknown] <- "unresolved-status"
  diagnostics[included & unknown_negative] <- ifelse(
    data$code_id[included & unknown_negative] %in% ambiguous_input_ids,
    "ambiguous-input-code", "unrecognized-negative-code")
  result <- data
  result$code_type[included] <- types[included]
  result$code_id[included] <- numeric_ids[included]
  result$code_score[included] <- numeric_scores[included]
  list(data = result, reasons = reasons, basis = basis, diagnostics = diagnostics)
}
