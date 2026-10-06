# Evaluate only established relationships. Matrices are cached per static unit
# occurrence; person-specific evidence then needs only the last worked-on unit.
missing_order_evidence <- function(keys, result, metadata, basis, reached,
                                    occurrence_rows, group_ids,
                                    use_variable_names_for_recoding) {
  static_fields <- c("booklet_id", "testlet_no", "unit_booklet_no", "unit_key", "unit_alias")
  static_keys <- keys[static_fields]
  static_ids <- dplyr::group_indices(dplyr::group_by(
    static_keys, dplyr::across(dplyr::everything())))
  occurrences <- dplyr::distinct(static_keys)
  if (anyDuplicated(occurrences[c("booklet_id", "testlet_no", "unit_booklet_no")])) {
    cli::cli_abort("Conflicting unit occurrences: each booklet/testlet/unit position must identify one unit and alias.")
  }
  cache <- vector("list", if (length(static_ids)) max(static_ids) else 0L)
  for (rows in occurrence_rows) {
    id <- static_ids[rows[1L]]
    if (!is.null(cache[[id]])) next
    bases <- rows[basis[rows]]
    source <- if ("order_source" %in% names(result)) result$order_source[bases] else NULL
    cache[[id]] <- list(
      ids = result$variable_id[bases],
      precedes = design_order_precedence(
        metadata[bases, , drop = FALSE], result$variable_order[bases],
        order_source = source,
        use_variable_names_for_recoding = use_variable_names_for_recoding
      )
    )
  }
  before <- tail <- rep(FALSE, nrow(result))
  for (rows in split(seq_len(nrow(result)), group_ids)) {
    bases <- rows[basis[rows]]
    anchors <- rows[reached[rows]]
    if (!length(anchors)) {
      tail[bases] <- TRUE
      next
    }
    last_unit <- max(keys$unit_booklet_no[anchors])
    before[bases[keys$unit_booklet_no[bases] < last_unit]] <- TRUE
    tail[bases[keys$unit_booklet_no[bases] > last_unit]] <- TRUE
    local <- bases[keys$unit_booklet_no[bases] == last_unit]
    local_anchors <- anchors[keys$unit_booklet_no[anchors] == last_unit]
    evidence <- cache[[static_ids[local[1L]]]]
    index <- match(result$variable_id[local], evidence$ids)
    anchor_index <- match(result$variable_id[local_anchors], evidence$ids)
    before[local] <- (rowSums(evidence$precedes[, anchor_index, drop = FALSE]) > 0)[index]
    tail[local] <- (colSums(evidence$precedes[anchor_index, , drop = FALSE]) == length(anchor_index))[index]
    # A source that is itself worked on is at the reached boundary, even if
    # other variables at its physical position cannot be ordered against it.
    before[anchors] <- TRUE
  }
  list(before = before, tail = tail)
}
