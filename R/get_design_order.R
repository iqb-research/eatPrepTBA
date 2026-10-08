#' Determine static variable and item positions in a booklet design
#'
#' @param design Data frame containing `booklet_id`, `testlet_no`,
#'   `unit_booklet_no`, `unit_key`, and `unit_alias`. Person identifiers and
#'   assignment-specific `booklet_no` are ignored.
#' @param units Unit data, optionally prepared with [add_coding_scheme()].
#' @param overwrite Logical. Rebuild an existing `unit_codes` column.
#' @param order_overrides Optional data frame with `unit_key`, `variable_id`,
#'   and `local_order`. An override must contain every active basis variable
#'   of a unit occurrence. Optional `booklet_id`, `testlet_no`, and
#'   `unit_booklet_no` restrict its scope; missing scope values are wildcards.
#' @param item_selection Optional data frame with `unit_key`, `variable_id`,
#'   and optionally `item_id`. Only selected variables receive an `item_order`;
#'   variable positions remain those of the complete design.
#' @param order_method Internal ordering policy: `"vomd"` uses Studio item
#'   list positions and source dependencies, `"structure"` uses unit positions,
#'   and `"hybrid"` supplements physical positions with compatible VOMD
#'   relationships. Complete overrides take precedence over either source.
#' @param use_variable_names_for_recoding Whether names may resolve otherwise
#'   unknown relationships. Names always provide deterministic display ranks.
#'
#' @details
#' Positions are computed once per booklet, not from participants' responses.
#' All active coding variables are included, even when `design` contains only
#' a variable subset. Booklet identifiers are normalized to uppercase.
#' VOMD uses `item_no`, the original item-list position, rather than an optional
#' Studio `item_order` property. Unique hidden sources share their owning item's
#' group; shared or unassigned sources remain unlocated within the unit.
#' Structure mode uses pages and numeric section/element paths. Hybrid mode
#' retains those relations and supplements them with compatible item positions.
#' Contradictory lower-priority relationships are discarded with one warning per
#' unit and recorded in the `order_conflicts` attribute. Names never reverse
#' known positions, even when enabled. Derived presentation anchors come from
#' their basis sources; their own item-list positions are kept separately.
#' Derived variables follow their sources in the deterministic display order.
#'
#' `variable_order` is a unique, consecutive integer within each booklet.
#' It is a display order, not evidence that every relationship is known.
#' `position_group` records local groups where known, and the static
#' `design_precedence` attribute preserves partial relationships per occurrence.
#' Names supply analytical relationships only when explicitly enabled.
#' Item indices use the complete Studio item universe within each booklet;
#' selecting items masks their indices without renumbering the others.
#' No item identifiers are inferred from variable names.
#'
#' @return A static tibble keyed by `booklet_id`, `testlet_no`,
#'   `unit_booklet_no`, `unit_key`, `unit_alias`, and `variable_id`, with
#'   variable metadata, `variable_order`, `order_group`, `order_source`,
#'   `item_id`, `item_order`, and `item_order_source`.
#' @keywords internal
#' @noRd
get_design_order <- function(design, units, overwrite = FALSE,
                             order_overrides = NULL, item_selection = NULL,
                             order_method = c("vomd", "structure", "hybrid"),
                             use_variable_names_for_recoding = FALSE,
                             metadata = NULL, progress = FALSE) {
  order_method <- match.arg(order_method)
  checkmate::assert_flag(use_variable_names_for_recoding)
  progress_session <- missing_progress_session_start(progress)
  on.exit(missing_progress_session_done(progress_session), add = TRUE)
  occurrence_keys <- c("booklet_id", "testlet_no", "unit_booklet_no",
                       "unit_key", "unit_alias")
  assert_cols(design, occurrence_keys, "design")
  checkmate::assert_flag(overwrite)
  occurrences <- tibble::as_tibble(design[occurrence_keys])
  occurrences$booklet_id <- stringr::str_to_upper(as.character(occurrences$booklet_id))
  occurrences$unit_key <- as.character(occurrences$unit_key)
  occurrences$unit_alias <- as.character(occurrences$unit_alias)
  design_order_assert_keys(occurrences, c("booklet_id", "unit_key"), "design")
  for (column in c("testlet_no", "unit_booklet_no")) {
    checkmate::assert_numeric(occurrences[[column]], any.missing = column == "testlet_no",
                             lower = 0, finite = TRUE, .var.name = paste0("design$", column))
  }
  occurrences <- dplyr::distinct(occurrences)
  slots <- occurrences[c("booklet_id", "testlet_no", "unit_booklet_no")]
  if (nrow(dplyr::distinct(slots)) != nrow(slots)) {
    cli::cli_abort("Conflicting unit occurrences in {.arg design}: each booklet/testlet/unit position must identify one unit and alias.")
  }
  occurrences <- dplyr::arrange(occurrences, .data$booklet_id, .data$testlet_no,
                                .data$unit_booklet_no, .data$unit_key, .data$unit_alias)
  units <- design_order_units_for_keys(units, occurrences$unit_key)
  if (is.null(metadata) || overwrite) {
    metadata <- design_order_metadata(units, overwrite = overwrite, progress = progress)
  }
  metadata <- design_order_vomd_metadata(metadata, units)
  unresolved_items <- attr(metadata, "vomd_unresolved")
  unknown_units <- setdiff(occurrences$unit_key, metadata$unit_key)
  if (length(unknown_units)) {
    cli::cli_abort("Unknown or empty units in {.arg design}: {unknown_units}.")
  }
  if ("variable_id" %in% names(design)) {
    requested <- dplyr::distinct(tibble::as_tibble(design[c("unit_key", "variable_id")]))
    design_order_assert_keys(requested, c("unit_key", "variable_id"), "design")
    unknown <- dplyr::anti_join(requested, metadata, by = c("unit_key", "variable_id"))
    if (nrow(unknown)) {
      cli::cli_abort("Unknown variables in {.arg design}: {paste(unknown$unit_key, unknown$variable_id, sep = ' / ')}.")
    }
  }
  overrides <- design_order_validate_overrides(order_overrides, occurrences, metadata)
  tables <- vector("list", nrow(occurrences))
  precedence <- vector("list", nrow(occurrences))
  conflicts <- vector("list", nrow(occurrences))
  template_of <- integer(nrow(occurrences))
  group_offsets <- integer(nrow(occurrences))
  template_count <- 0L
  offset <- 0L
  previous_booklet <- NULL
  metadata_indices <- split(seq_len(nrow(metadata)), metadata$unit_key)
  position_cache <- new.env(parent = emptyenv())
  ordering_progress <- missing_progress_start("Ordering units", nrow(occurrences), progress)
  for (i in seq_len(nrow(occurrences))) {
    occurrence <- occurrences[i, , drop = FALSE]
    key <- occurrence$unit_key[[1L]]
    # An unmodified unit has the same local order in every booklet. Overrides
    # remain occurrence-specific and are deliberately not reused.
    if (is.null(overrides[[i]]) && exists(key, envir = position_cache, inherits = FALSE)) {
      template <- get(key, envir = position_cache, inherits = FALSE)
      current <- tables[[template]]
    } else {
      current <- metadata[metadata_indices[[key]], , drop = FALSE]
      current <- design_order_position_unit(current, overrides[[i]], order_method,
                                            use_variable_names_for_recoding)
      template_count <- template_count + 1L
      template <- template_count
      tables[[template]] <- current
      conflicts[[template]] <- attr(current, "order_conflicts", exact = TRUE)
      if (is.null(overrides[[i]])) assign(key, template, envir = position_cache)
    }
    template_of[[i]] <- template
    precedence[[i]] <- list(keys = occurrence,
                            variable_ids = current$variable_id,
                            before = attr(current, "before"))
    if (!identical(previous_booklet, occurrence$booklet_id[[1L]])) offset <- 0L
    group_offsets[[i]] <- offset
    offset <- max(c(offset, current$order_group + offset), na.rm = TRUE)
    previous_booklet <- occurrence$booklet_id[[1L]]
    missing_progress_update(ordering_progress)
  }
  missing_progress_done(ordering_progress)
  assembly_progress <- missing_progress_start("Assembling booklet positions", enabled = progress)
  if (!length(tables)) {
    out <- dplyr::bind_cols(occurrences, metadata[0, setdiff(names(metadata), "unit_key")])
    out$variable_order <- integer()
    out$order_group <- integer()
    out$order_source <- character()
    out$position_group <- numeric()
    out$position_source <- character()
    out$analysis_included <- logical()
    out$box_position <- numeric()
    out$box_included <- logical()
    out$box_is_item <- logical()
  } else {
    # Materialize repeated occurrences once from the cached local templates,
    # rather than constructing a separate data frame for every occurrence.
    tables <- tables[seq_len(template_count)]
    sizes <- vapply(tables, nrow, integer(1))
    starts <- c(1L, utils::head(cumsum(sizes), -1L) + 1L)
    occurrence_sizes <- sizes[template_of]
    rows <- sequence(occurrence_sizes, from = starts[template_of])
    local <- dplyr::bind_rows(tables)
    keys <- occurrences[rep.int(seq_len(nrow(occurrences)), occurrence_sizes), , drop = FALSE]
    out <- dplyr::bind_cols(keys, local[rows, setdiff(names(local), "unit_key")])
    # Local ranks include derivations; occurrence offsets are static per booklet.
    out$variable_order <- sequence(rle(out$booklet_id)$lengths)
    out$order_group <- out$order_group + rep.int(group_offsets, occurrence_sizes)
  }
  missing_progress_done(assembly_progress)
  item_progress <- missing_progress_start("Ordering items", enabled = progress)
  out <- design_order_items(out, item_selection)
  missing_progress_done(item_progress)
  attr(out, "design_precedence") <- precedence
  attr(out, "vomd_unresolved") <- unresolved_items
  conflicts <- dplyr::distinct(dplyr::bind_rows(design_order_empty_conflicts(), conflicts[seq_len(template_count)]))
  attr(out, "order_conflicts") <- conflicts
  design_order_warn_conflicts(conflicts)
  out
}

# Scope preparation to complete units, including every active variable and
# source. An unrelated workspace unit must not affect this design's metadata.
design_order_units_for_keys <- function(units, unit_keys) {
  assert_cols(units, "unit_key", "units")
  used <- as.character(units$unit_key) %in% unique(as.character(unit_keys))
  units[used, , drop = FALSE]
}

# Prepare one row per active variable, preserving original source references.
# This helper also accepts its own prepared result to avoid repeated work.
design_order_metadata <- function(units, overwrite = FALSE, progress = FALSE) {
  checkmate::assert_flag(overwrite)
  assert_cols(units, "unit_key", "units")
  if (!nrow(units)) return(design_order_empty_metadata())
  prepared_columns <- c("unit_key", "variable_id", "variable_ref", "variable_source_type",
                        "variable_level", "variable_page", "variable_section",
                        "variable_element", "variable_page_always_visible",
                        "source_ids", "basis_sources", "sources_known")
  if (all(prepared_columns %in% names(units)) && !overwrite) {
    prepared <- dplyr::distinct(tibble::as_tibble(units[prepared_columns]))
    design_order_assert_keys(prepared, c("unit_key", "variable_id", "variable_ref"), "units")
    if (anyDuplicated(prepared[c("unit_key", "variable_id")])) {
      cli::cli_abort("Conflicting prepared variable metadata in {.arg units}.")
    }
    return(prepared)
  }
  if (overwrite || !"unit_codes" %in% names(units)) {
    units <- add_coding_scheme(units, filter_has_codes = TRUE, overwrite = overwrite,
                              progress = progress)
  }
  assert_cols(units, "unit_codes", "units")
  rows <- purrr::map2(units$unit_key, units$unit_codes, function(key, codes) {
    if (is.null(codes) || !nrow(codes)) return(NULL)
    assert_cols(codes, "variable_id", "unit_codes")
    codes <- tibble::as_tibble(codes)
    codes$unit_key <- rep(as.character(key), nrow(codes))
    codes
  })
  raw <- dplyr::bind_rows(rows)
  if (!nrow(raw)) {
    return(design_order_empty_metadata())
  }
  design_order_assert_keys(raw, c("unit_key", "variable_id"), "unit_codes")
  raw$unit_key <- as.character(raw$unit_key)
  raw$variable_id <- as.character(raw$variable_id)
  defaults <- list(variable_ref = NA_character_, variable_source_type = NA_character_,
                   variable_level = NA_integer_, variable_page = NA_real_,
                   variable_section = NA_integer_, variable_element = NA_integer_,
                   variable_page_always_visible = NA)
  for (column in names(defaults)) {
    if (!column %in% names(raw)) raw[[column]] <- rep(defaults[[column]], nrow(raw))
  }
  raw <- raw[is.na(raw$variable_source_type) | raw$variable_source_type != "BASE_NO_VALUE", , drop = FALSE]
  if (!nrow(raw)) return(design_order_empty_metadata())
  raw$variable_ref <- dplyr::coalesce(as.character(raw$variable_ref), raw$variable_id)
  out <- design_order_metadata_merge(raw, progress = progress)
  out$source_ids <- vector("list", nrow(out))
  out$basis_sources <- vector("list", nrow(out))
  out$sources_known <- rep(FALSE, nrow(out))
  for (key in unique(out$unit_key)) {
    index <- which(out$unit_key == key)
    graph <- out[index, , drop = FALSE]
    if (anyDuplicated(graph$variable_ref)) {
      cli::cli_abort("Multiple variable aliases for the same original reference in unit {key}; source dependencies are ambiguous.")
    }
    ids <- graph$variable_id
    dependencies <- lapply(graph$.source_refs, function(refs) {
      alias <- startsWith(refs, ".alias:")
      result <- ids[match(refs, graph$variable_ref)]
      result[alias] <- ids[match(substring(refs[alias], 8L), ids)]
      unique(result)
    })
    is_base <- design_order_is_base(graph)
    state <- integer(length(ids))
    leaves <- vector("list", length(ids))
    known <- rep(FALSE, length(ids))
    visit <- function(i) {
      if (state[[i]] == 2L) return(list(ids = leaves[[i]], known = known[[i]]))
      if (state[[i]] == 1L) return(list(ids = character(), known = FALSE))
      state[[i]] <<- 1L
      if (is_base[[i]]) {
        result <- list(ids = ids[[i]], known = TRUE)
      } else {
        deps <- dependencies[[i]]
        children <- lapply(match(deps[!is.na(deps)], ids), visit)
        child_ids <- unique(as.character(unlist(lapply(children, `[[`, "ids"), use.names = FALSE)))
        result <- list(ids = child_ids,
                       known = length(deps) > 0L && !anyNA(deps) &&
                         length(child_ids) > 0L && all(vapply(children, `[[`, logical(1), "known")))
      }
      leaves[[i]] <<- result$ids
      known[[i]] <<- result$known
      state[[i]] <<- 2L
      result
    }
    for (i in seq_along(ids)) visit(i)
    out$source_ids[index] <- dependencies
    out$basis_sources[index] <- leaves
    out$sources_known[index] <- known
  }
  out[prepared_columns]
}

design_order_empty_metadata <- function() {
  tibble::tibble(unit_key = character(), variable_id = character(),
                 variable_ref = character(), variable_source_type = character(),
                 variable_level = integer(), variable_page = numeric(),
                 variable_section = list(), variable_element = list(),
                 variable_page_always_visible = logical(), source_ids = list(),
                 basis_sources = list(), sources_known = logical())
}

# VOMD list positions are item positions, not a total order of all variables.
# Resolve original read-only references before aliases, without guessing names.
design_order_vomd_metadata <- function(metadata, units) {
  metadata$item_id <- rep(NA_character_, nrow(metadata))
  metadata$item_position <- rep(NA_real_, nrow(metadata))
  metadata$box_position <- rep(NA_real_, nrow(metadata))
  metadata$box_included <- rep(FALSE, nrow(metadata))
  metadata$box_is_item <- rep(FALSE, nrow(metadata))
  metadata$analysis_included <- rep(FALSE, nrow(metadata))
  unresolved_schema <- tibble::tibble(unit_key = character(), item_id = character(),
                                      variable_id = character(), variable_ref = character(),
                                      item_position = numeric())
  attr(metadata, "vomd_unresolved") <- unresolved_schema
  column <- intersect(c("items_list", "item_metadata"), names(units))
  if (!length(column)) return(metadata)
  mapping <- vector("list", nrow(units))
  unresolved <- vector("list", nrow(units))
  for (i in seq_len(nrow(units))) {
    items <- units[[column[[1L]]]][[i]]
    if (is.null(items) || !nrow(items)) next
    assert_cols(items, c("variable_id", "item_id"), column[[1L]])
    key <- as.character(units$unit_key[[i]])
    current <- metadata[metadata$unit_key == key, , drop = FALSE]
    reference <- if ("variable_ref" %in% names(items)) as.character(items$variable_ref) else rep(NA_character_, nrow(items))
    aliases <- as.character(items$variable_id)
    resolved <- match(reference, current$variable_ref)
    valid_reference <- !is.na(reference) & nzchar(trimws(reference))
    resolved[!valid_reference] <- NA_integer_
    fallback <- is.na(resolved)
    resolved[fallback] <- match(aliases[fallback], current$variable_id)
    fallback <- is.na(resolved)
    resolved[fallback] <- match(aliases[fallback], current$variable_ref)
    positions <- if ("item_no" %in% names(items)) suppressWarnings(as.numeric(items$item_no)) else seq_len(nrow(items))
    missing <- is.na(resolved) & !is.na(items$item_id) & nzchar(trimws(as.character(items$item_id)))
    unresolved[[i]] <- tibble::tibble(unit_key = rep(key, sum(missing)),
                                      item_id = as.character(items$item_id[missing]),
                                      variable_id = aliases[missing], variable_ref = reference[missing],
                                      item_position = as.numeric(positions[missing]))
    valid <- !is.na(resolved) & !is.na(items$item_id) & nzchar(trimws(as.character(items$item_id)))
    if (any(valid & (is.na(positions) | !is.finite(positions) | positions < 0))) {
      cli::cli_abort("Invalid VOMD {.field item_no} in unit {key}; item-list positions must be finite and non-negative.")
    }
    mapping[[i]] <- tibble::tibble(unit_key = key,
                                   variable_id = current$variable_id[resolved[valid]],
                                   item_id = as.character(items$item_id[valid]),
                                   item_position = as.numeric(positions[valid]))
  }
  mapping <- dplyr::distinct(dplyr::bind_rows(mapping))
  attr(metadata, "vomd_unresolved") <- dplyr::distinct(dplyr::bind_rows(unresolved_schema, unresolved))
  if (!nrow(mapping)) return(metadata)
  if (anyDuplicated(mapping[c("unit_key", "variable_id")])) {
    cli::cli_abort("Conflicting VOMD item identifiers or positions for one variable. Subset {.arg units} to the relevant versions first.")
  }
  index <- match(paste(metadata$unit_key, metadata$variable_id, sep = "\r"),
                 paste(mapping$unit_key, mapping$variable_id, sep = "\r"))
  metadata$item_id <- mapping$item_id[index]
  metadata$item_position <- mapping$item_position[index]
  metadata$box_is_item <- !is.na(metadata$item_position)
  for (key in unique(metadata$unit_key)) {
    indices <- which(metadata$unit_key == key)
    current <- metadata[indices, , drop = FALSE]
    anchors <- rep(list(numeric()), nrow(current))
    items <- which(current$box_is_item)
    # Directly mapped sources retain their own item anchor. Shared hidden
    # sources cannot be assigned the position of an arbitrarily chosen owner.
    visit <- function(j, anchor, visited = integer()) {
      if (j %in% visited) return(invisible(NULL))
      if (current$box_is_item[[j]]) {
        anchor <- current$item_position[[j]]
      } else {
        anchors[[j]] <<- unique(c(anchors[[j]], anchor))
      }
      children <- match(current$source_ids[[j]], current$variable_id)
      for (child in children[!is.na(children)]) visit(child, anchor, c(visited, j))
      invisible(NULL)
    }
    for (j in items) visit(j, current$item_position[[j]])
    unique_source <- lengths(anchors) == 1L
    position <- current$item_position
    hidden <- which(!current$box_is_item & unique_source)
    position[hidden] <- vapply(anchors[hidden], `[[`, numeric(1), 1L)
    included <- current$box_is_item | unique_source
    metadata$box_position[indices] <- position
    metadata$box_included[indices] <- included
    metadata$analysis_included[indices] <- included
  }
  metadata
}

# Expand a partial basis order to variable presentation positions. A derived
# variable is anchored at its latest source; its calculation/display index is
# never itself proof that it follows those sources physically.
design_order_expand_precedence <- function(metadata, basis_before) {
  basis <- which(design_order_is_base(metadata))
  if (length(basis) == nrow(metadata)) return(basis_before)
  derived <- setdiff(seq_len(nrow(metadata)), basis)
  leaves <- lapply(derived, function(i) {
    if (!metadata$sources_known[[i]]) return(integer())
    at <- match(metadata$basis_sources[[i]], metadata$variable_id[basis])
    if (anyNA(at)) integer() else unique(at)
  })
  before <- matrix(FALSE, nrow(metadata), nrow(metadata))
  before[basis, basis] <- basis_before
  # First locate every target relative to the bases. Then a derived variable
  # precedes a target only if each of its sources precedes that target. This
  # avoids a per-pair R callback for the overwhelmingly common base/base case.
  for (j in which(lengths(leaves) > 0L)) {
    before[basis, derived[[j]]] <- rowSums(basis_before[, leaves[[j]], drop = FALSE]) > 0L
  }
  for (i in which(lengths(leaves) > 0L)) {
    before[derived[[i]], ] <- colSums(before[basis[leaves[[i]]], , drop = FALSE]) == length(leaves[[i]])
  }
  diag(before) <- FALSE
  before
}

# A topological sort also detects cycles without constructing all paths.
design_order_topological <- function(before, priority = seq_len(nrow(before))) {
  n <- nrow(before)
  pending <- rep(TRUE, n)
  predecessors <- colSums(before)
  emitted <- integer(n)
  count <- 0L
  while (count < n) {
    available <- which(pending & predecessors == 0L)
    if (!length(available)) break
    i <- available[[which.min(priority[available])]]
    count <- count + 1L
    emitted[[count]] <- i
    pending[[i]] <- FALSE
    predecessors <- predecessors - before[i, ]
  }
  emitted[seq_len(count)]
}

# Build a DAG's closure a row at a time. In a dense sequential order the first
# successor already contains the whole remaining suffix; do not re-union it
# once for every descendant or allocate an n-by-n outer product at every node.
design_order_transitive <- function(before, unit_key, topological = NULL) {
  if (is.null(topological)) topological <- design_order_topological(before)
  if (length(topological) != nrow(before)) {
    cli::cli_abort("Conflicting trusted ordering metadata in unit {unit_key}.")
  }
  result <- matrix(FALSE, nrow(before), ncol(before))
  for (i in rev(topological)) {
    pending <- topological[before[i, topological]]
    while (length(pending)) {
      next_node <- pending[[1L]]
      result[i, ] <- result[i, ] | result[next_node, ]
      result[i, next_node] <- TRUE
      pending <- pending[!result[i, pending]]
    }
  }
  result
}

# Iterative Tarjan traversal: large units must not exhaust R's recursion stack.
design_order_components <- function(before) {
  n <- nrow(before)
  adjacent <- lapply(seq_len(n), function(i) which(before[i, ]))
  index <- low <- component <- parent <- integer(n)
  cursor <- rep(1L, n)
  on_stack <- rep(FALSE, n)
  stack <- integer(n)
  depth <- clock <- components <- 0L
  for (root in seq_len(n)) {
    if (index[[root]]) next
    current <- root
    repeat {
      if (!index[[current]]) {
        clock <- clock + 1L
        index[[current]] <- low[[current]] <- clock
        depth <- depth + 1L
        stack[[depth]] <- current
        on_stack[[current]] <- TRUE
      }
      children <- adjacent[[current]]
      if (cursor[[current]] <= length(children)) {
        child <- children[[cursor[[current]]]]
        cursor[[current]] <- cursor[[current]] + 1L
        if (!index[[child]]) {
          parent[[child]] <- current
          current <- child
        } else if (on_stack[[child]]) {
          low[[current]] <- min(low[[current]], index[[child]])
        }
        next
      }
      if (low[[current]] == index[[current]]) {
        components <- components + 1L
        repeat {
          child <- stack[[depth]]
          depth <- depth - 1L
          on_stack[[child]] <- FALSE
          component[[child]] <- components
          if (child == current) break
        }
      }
      predecessor <- parent[[current]]
      if (!predecessor) break
      low[[predecessor]] <- min(low[[predecessor]], low[[current]])
      current <- predecessor
    }
  }
  component
}

design_order_empty_conflicts <- function() {
  tibble::tibble(unit_key = character(), before_variable_id = character(),
    after_variable_id = character(), discarded_source = character(),
    retained_source = character(), reason = character())
}

# Apply one complete priority tier at once. When several lower-priority edges
# jointly form a cycle, drop ALL edges from that tier within the component.
# Greedily accepting them one by one would make results depend on row order.
design_order_add_priority <- function(higher, lower, metadata, source, retained) {
  lower <- lower & !higher
  conflicts <- design_order_empty_conflicts()
  if (!any(lower)) return(list(before = higher, added = lower, conflicts = conflicts))
  combined <- higher | lower
  sorted <- design_order_topological(combined)
  if (length(sorted) != nrow(combined)) {
    components <- design_order_components(combined)
    rejected <- lower & outer(components, components, `==`)
    pairs <- which(rejected, arr.ind = TRUE)
    conflicts <- tibble::tibble(
      unit_key = rep(metadata$unit_key[[1L]], nrow(pairs)),
      before_variable_id = metadata$variable_id[pairs[, 1L]],
      after_variable_id = metadata$variable_id[pairs[, 2L]],
      discarded_source = rep(source, nrow(pairs)),
      retained_source = rep(retained, nrow(pairs)),
      reason = ifelse(higher[cbind(pairs[, 2L], pairs[, 1L])],
        "contradicts_higher_priority", "indirect_cycle"))
    lower[rejected] <- FALSE
    combined <- higher | lower
    sorted <- design_order_topological(combined)
  }
  list(before = design_order_transitive(combined, metadata$unit_key[[1L]], sorted),
    added = lower, conflicts = conflicts)
}

design_order_warn_conflicts <- function(conflicts) {
  for (key in unique(conflicts$unit_key)) {
    current <- conflicts[conflicts$unit_key == key, , drop = FALSE]
    examples <- paste0(current$before_variable_id, " -> ", current$after_variable_id)
    examples <- paste(utils::head(examples, 3L), collapse = ", ")
    discarded <- paste(unique(current$discarded_source), collapse = "/")
    retained <- paste(unique(current$retained_source), collapse = "/")
    cli::cli_warn(c(
      "Order conflict in unit {.val {key}}: ignored {nrow(current)} lower-priority {discarded} relationship{?s} ({examples}).",
      "i" = "Retained {retained} relationships; unresolved pairs are not used for NR recoding. Inspect {.code attr(result, 'order_conflicts')}."
    ), class = "eatPrepTBA_order_conflict")
  }
  invisible(NULL)
}

design_order_position_unit <- function(metadata, override, method, use_names) {
  is_base <- design_order_is_base(metadata)
  base <- which(is_base)
  n <- nrow(metadata)
  raw <- metadata
  basis <- raw[base, , drop = FALSE]
  conflicts <- design_order_empty_conflicts()
  group <- raw$box_position
  source <- ifelse(raw$box_is_item, "vomd_item",
                    ifelse(raw$box_included, "vomd_source", "unit_only"))
  if (!is.null(override)) {
    out <- design_order_unit(raw, override)
    ranks <- override$local_order[match(raw$variable_id[base], override$variable_id)]
    before <- design_order_expand_precedence(raw, outer(ranks, ranks, `<`))
    group <- out$order_group[match(raw$variable_id, out$variable_id)]
    source[] <- "override"
  } else {
    if (method == "vomd") {
      # Box presentation positions intentionally remain independent of pages.
      before <- outer(group, group, `<`)
      before[is.na(before)] <- FALSE
      basis_before <- before[base, base, drop = FALSE]
    } else {
      # Resolve competing evidence only among bases. Combining a derived item's
      # own VOMD anchor with its source anchor creates artificial contradictions.
      basis_before <- design_order_precedence(basis, seq_along(base))
      source <- ifelse(is_base, "unit_only", "derived_sources")
      located <- is.finite(basis$variable_page) & basis$variable_page >= 0 &
        !basis$variable_page_always_visible %in% TRUE
      source[base[located]] <- "structure"
      if (method == "hybrid") {
        vomd <- outer(basis$box_position, basis$box_position, `<`)
        vomd[is.na(vomd)] <- FALSE
        merged <- design_order_add_priority(basis_before, vomd, basis, "vomd", "structure")
        basis_before <- merged$before
        conflicts <- dplyr::bind_rows(conflicts, merged$conflicts)
        supplemented <- base[rowSums(merged$added) + colSums(merged$added) > 0L]
        source[supplemented] <- ifelse(source[supplemented] == "structure", "structure_vomd", "vomd")
      }
    }
    if (use_names && length(base)) {
      naming <- design_order_natural_rank(raw$variable_id[base])
      merged <- design_order_add_priority(basis_before, outer(naming, naming, `<`),
        basis, "naming", if (method == "structure") "structure" else if (method == "hybrid") "structure/vomd" else "vomd")
      basis_before <- merged$before
      conflicts <- dplyr::bind_rows(conflicts, merged$conflicts)
      named_ids <- base[rowSums(merged$added) + colSums(merged$added) > 0L]
      source[named_ids] <- paste0(source[named_ids], "_naming_trusted")
      if (method == "vomd") {
        # Preserve explicit VOMD semantics, including mapped derivations. New
        # name relations are accepted only if they also fit these anchors.
        expanded <- design_order_expand_precedence(raw, merged$added)
        mapped <- design_order_add_priority(before, expanded, raw, "naming", "vomd")
        before <- mapped$before
        conflicts <- dplyr::bind_rows(conflicts, mapped$conflicts)
      }
    }
    if (method != "vomd") {
      before <- design_order_expand_precedence(raw, basis_before)
      group <- rep(NA_real_, n)
      known <- rowSums(basis_before) + colSums(basis_before) > 0L | located
      if (method == "hybrid") known <- known | basis$box_included
      group[base[known]] <- colSums(basis_before)[known] + 1
      for (i in which(!is_base)) {
        leaves <- match(raw$basis_sources[[i]], raw$variable_id)
        if (raw$sources_known[[i]] && length(leaves) && !anyNA(leaves) && !anyNA(group[leaves])) {
          group[[i]] <- max(group[leaves])
        } else if (!raw$sources_known[[i]]) {
          source[[i]] <- "derived_unresolved"
        }
      }
    }
    if (method == "structure") {
      # Keep structural display provenance and equal-position groups stable.
      out <- design_order_unit(raw)
      group <- out$order_group[match(raw$variable_id, out$variable_id)]
      structural_source <- out$order_source[match(raw$variable_id, out$variable_id)]
      structural_source[grepl("naming_trusted", source, fixed = TRUE)] <- source[grepl("naming_trusted", source, fixed = TRUE)]
      source <- structural_source
      group[base[!located]] <- NA_real_
      if (use_names) group[base] <- colSums(basis_before) + 1
      out$order_source <- source[match(out$variable_id, raw$variable_id)]
    } else {
      out <- NULL
    }
    # Dense ranks are deterministic and dependency-safe, while groups and the
    # separate relation matrix retain the actual uncertainty.
    if (is.null(out)) {
      pending <- which(!is_base)
      emitted <- integer()
      natural <- design_order_natural_rank(raw$variable_id)
      # Unknown pages are interleaved by name for display, never pushed to the
      # end by an artificial Inf location. Analytical evidence stays separate.
      priority <- if (method == "vomd") group else rep(0, n)
      priority[is.na(priority)] <- Inf
      display_rank <- order(order(priority, natural))
      local <- design_order_topological(before[base, base, drop = FALSE], display_rank[base])
      dependencies <- lapply(raw$source_ids, match, table = raw$variable_id)
      for (j in base[local]) {
        emitted <- c(emitted, j)
        # Calculation order is a separate display convention: insert every
        # newly available derivation immediately after its final source.
        repeat {
          ready <- pending[vapply(pending, function(k) {
            deps <- dependencies[[k]]
            raw$sources_known[[k]] && length(deps) > 0L &&
              !anyNA(deps) && all(deps %in% emitted)
          }, logical(1))]
          if (!length(ready)) break
          k <- ready[[which.min(display_rank[ready])]]
          emitted <- c(emitted, k)
          pending <- pending[pending != k]
        }
      }
      if (length(pending)) emitted <- c(emitted, pending[order(priority[pending], natural[pending])])
      out <- raw[emitted, , drop = FALSE]
      out$variable_order <- seq_along(emitted)
      out$order_group <- as.integer(group[emitted])
      out$order_source <- source[emitted]
    }
  }
  index <- match(out$variable_id, raw$variable_id)
  out$position_group <- as.numeric(group[index])
  out$position_source <- source[index]
  out$analysis_included <- raw$analysis_included[index]
  attr(out, "before") <- before[index, index, drop = FALSE]
  attr(out, "order_conflicts") <- conflicts
  out
}

design_order_is_base <- function(metadata) {
  startsWith(dplyr::coalesce(metadata$variable_source_type, ""), "BASE") |
    (is.na(metadata$variable_source_type) & !is.na(metadata$variable_level) & metadata$variable_level == 0L)
}

design_order_assert_keys <- function(data, columns, argument) {
  for (column in columns) {
    values <- as.character(data[[column]])
    if (anyNA(values) || any(!nzchar(trimws(values)))) {
      cli::cli_abort("{.arg {argument}} has missing or empty {.field {column}} keys.")
    }
  }
}

design_order_natural_rank <- function(ids) {
  stable <- sort(unique(ids), method = "radix")
  stable <- stable[stringr::str_order(stable, numeric = TRUE, locale = "en")]
  match(ids, stable)
}

# Only numeric array paths carry physical order; element IDs are not paths.
design_order_path <- function(path) {
  if (!is.numeric(path) || !length(path) || anyNA(path) ||
      any(!is.finite(path) | path < 0 | path != floor(path))) return(NA_character_)
  paste(sprintf("%020.0f", path), collapse = "/")
}

# Compare basis variables within one unit occurrence. A display rank alone is
# not evidence of physical order; only a complete explicit override makes it so.
design_order_precedence <- function(metadata, variable_order, order_source = NULL,
                                     use_variable_names_for_recoding = FALSE) {
  checkmate::assert_flag(use_variable_names_for_recoding)
  assert_cols(metadata, c("unit_key", "variable_id"), "metadata")
  n <- nrow(metadata)
  if (!is.null(order_source)) {
    checkmate::assert_character(order_source, len = n, any.missing = TRUE)
  }
  if (any(order_source %in% "override")) {
    if (!all(order_source %in% "override")) {
      cli::cli_abort("An explicit order override must cover every basis variable of a unit occurrence. Supply a complete {.arg order_overrides} table.")
    }
    checkmate::assert_integerish(variable_order, len = n, any.missing = FALSE,
                                 lower = 1)
    if (anyDuplicated(variable_order)) {
      cli::cli_abort("An explicit order override must give each basis variable a unique position.")
    }
    return(outer(variable_order, variable_order, `<`))
  }

  page <- if ("variable_page" %in% names(metadata)) metadata$variable_page else rep(NA_real_, n)
  page[!is.finite(page) | page < 0] <- NA_real_
  path_rank <- function(column) {
    paths <- if (column %in% names(metadata)) {
      vapply(metadata[[column]], design_order_path, character(1))
    } else {
      rep(NA_character_, n)
    }
    match(paths, sort(unique(paths[!is.na(paths)]), method = "radix"))
  }
  section <- path_rank("variable_section")
  element <- path_rank("variable_element")
  same_page <- outer(page, page, `==`)
  same_section <- outer(section, section, `==`)
  before <- outer(page, page, `<`) |
    (same_page & outer(section, section, `<`)) |
    (same_page & same_section & outer(element, element, `<`))
  before[is.na(before)] <- FALSE

  # Permanently visible supplementary pages have no position in the sequential
  # page flow. Their layout must not imply reachability, even within that page.
  # VOMD item order and explicit overrides remain independent sources of order.
  always_visible <- if ("variable_page_always_visible" %in% names(metadata)) {
    metadata$variable_page_always_visible %in% TRUE
  } else rep(FALSE, n)
  before[always_visible, ] <- FALSE
  before[, always_visible] <- FALSE

  if (use_variable_names_for_recoding) {
    naming <- design_order_natural_rank(metadata$variable_id)
    merged <- design_order_add_priority(before, outer(naming, naming, `<`),
      metadata, "naming", "structure")
    design_order_warn_conflicts(merged$conflicts)
    return(merged$before)
  }
  before
}

# Preserve every known physical relation while using names only to choose among
# the currently available variables. Unknown pages therefore need not come last.
design_order_display_order <- function(metadata) {
  before <- design_order_precedence(metadata, seq_len(nrow(metadata)))
  naming <- design_order_natural_rank(metadata$variable_id)
  pending <- seq_len(nrow(metadata))
  predecessors <- colSums(before)
  emitted <- integer()
  while (length(pending)) {
    available <- pending[predecessors[pending] == 0L]
    if (!length(available)) {
      cli::cli_abort("Conflicting physical ordering metadata in a unit.")
    }
    next_variable <- available[which.min(naming[available])]
    emitted <- c(emitted, next_variable)
    predecessors <- predecessors - before[next_variable, ]
    pending <- pending[pending != next_variable]
  }
  emitted
}

design_order_unit <- function(metadata, override = NULL) {
  base <- which(design_order_is_base(metadata))
  derived <- setdiff(seq_len(nrow(metadata)), base)
  if (!is.null(override)) {
    base <- base[order(override$local_order[match(metadata$variable_id[base], override$variable_id)])]
    groups <- seq_along(base)
    provenance <- rep("override", length(base))
  } else {
    basis <- metadata[base, , drop = FALSE]
    page <- basis$variable_page
    page[!is.finite(page) | page < 0] <- NA_real_
    section <- vapply(basis$variable_section, design_order_path, character(1))
    element <- vapply(basis$variable_element, design_order_path, character(1))
    # Partly missing paths cannot locate their unknown siblings in that parent.
    for (indices in split(seq_along(base), ifelse(is.na(page), "?", as.character(page)))) {
      if (anyNA(section[indices])) section[indices] <- NA_character_
    }
    section[is.na(page)] <- NA_character_
    parents <- paste(page, section, sep = "|")
    for (indices in split(seq_along(base), parents)) {
      if (anyNA(element[indices])) element[indices] <- NA_character_
    }
    element[is.na(section)] <- NA_character_
    provenance <- ifelse(is.na(page), "naming", ifelse(is.na(section), "page_naming",
                          ifelse(is.na(element), "page_section_naming", "page_section_element")))
    local <- design_order_display_order(basis)
    physical <- paste(page, section, element, sep = "|")
    groups <- match(physical[local], unique(physical[local]))
    provenance <- provenance[local]
    base <- base[local]
  }
  group_by_id <- integer(nrow(metadata))
  group_by_id[base] <- groups
  source <- rep("derived_sources", nrow(metadata))
  source[base] <- provenance
  emitted <- integer()
  pending <- derived[metadata$sources_known[derived]]
  for (i in base) {
    emitted <- c(emitted, i)
    repeat {
      eligible <- pending[vapply(pending, function(j) {
        length(metadata$source_ids[[j]]) > 0L &&
          all(metadata$source_ids[[j]] %in% metadata$variable_id[emitted])
      }, logical(1))]
      if (!length(eligible)) break
      j <- eligible[which.min(design_order_natural_rank(metadata$variable_id[eligible]))]
      group_by_id[[j]] <- max(group_by_id[match(metadata$basis_sources[[j]], metadata$variable_id)])
      emitted <- c(emitted, j)
      pending <- setdiff(pending, j)
    }
  }
  unresolved <- setdiff(derived, emitted)
  if (length(unresolved)) {
    unresolved <- unresolved[order(design_order_natural_rank(metadata$variable_id[unresolved]))]
    group_by_id[unresolved] <- if (length(groups)) max(groups) + 1L else 1L
    source[unresolved] <- "derived_unresolved"
    emitted <- c(emitted, unresolved)
  }
  out <- metadata[emitted, , drop = FALSE]
  out$variable_order <- seq_along(emitted)
  out$order_group <- as.integer(group_by_id[emitted])
  out$order_source <- source[emitted]
  out
}

design_order_validate_overrides <- function(overrides, occurrences, metadata) {
  result <- vector("list", nrow(occurrences))
  if (is.null(overrides)) return(result)
  assert_cols(overrides, c("unit_key", "variable_id", "local_order"), "order_overrides")
  overrides <- tibble::as_tibble(overrides)
  design_order_assert_keys(overrides, c("unit_key", "variable_id"), "order_overrides")
  checkmate::assert_numeric(overrides$local_order, any.missing = FALSE, finite = TRUE,
                           .var.name = "order_overrides$local_order")
  if ("booklet_id" %in% names(overrides)) overrides$booklet_id <- stringr::str_to_upper(overrides$booklet_id)
  used <- rep(FALSE, nrow(overrides))
  scopes <- intersect(c("booklet_id", "testlet_no", "unit_booklet_no"), names(overrides))
  for (i in seq_len(nrow(occurrences))) {
    occurrence <- occurrences[i, , drop = FALSE]
    matches <- overrides$unit_key == occurrence$unit_key
    for (scope in scopes) matches <- matches & (is.na(overrides[[scope]]) | overrides[[scope]] == occurrence[[scope]])
    selected <- which(matches %in% TRUE)
    if (!length(selected)) next
    current <- overrides[selected, , drop = FALSE]
    ids <- metadata$variable_id[metadata$unit_key == occurrence$unit_key & design_order_is_base(metadata)]
    if (anyDuplicated(current$variable_id) || anyDuplicated(current$local_order)) {
      cli::cli_abort("Duplicate or overlapping {.arg order_overrides} for unit {occurrence$unit_key}.")
    }
    if (!setequal(current$variable_id, ids)) {
      cli::cli_abort("An override must provide the full basis-variable order of unit {occurrence$unit_key}; derived or unknown variables are not allowed.")
    }
    used[selected] <- TRUE
    result[[i]] <- current
  }
  if (any(!used)) cli::cli_abort("Some {.arg order_overrides} do not match a design unit occurrence.")
  result
}

design_order_items <- function(order, selection = NULL) {
  selected <- rep(TRUE, nrow(order))
  order$item_order_source <- rep("unavailable", nrow(order))
  order$item_order_source[!is.na(order$item_id)] <- "studio_metadata"
  if (!is.null(selection)) {
    assert_cols(selection, c("unit_key", "variable_id"), "item_selection")
    selection <- tibble::as_tibble(selection[intersect(c("unit_key", "variable_id", "item_id"), names(selection))])
    design_order_assert_keys(selection, c("unit_key", "variable_id"), "item_selection")
    if (anyDuplicated(selection[c("unit_key", "variable_id")])) {
      cli::cli_abort("Duplicate variable keys in {.arg item_selection}.")
    }
    unknown <- dplyr::anti_join(selection, order, by = c("unit_key", "variable_id"))
    if (nrow(unknown)) cli::cli_abort("Unknown variables in {.arg item_selection}.")
    selected_keys <- dplyr::mutate(selection, .selected = TRUE)
    if ("item_id" %in% names(selected_keys)) names(selected_keys)[names(selected_keys) == "item_id"] <- ".selected_item_id"
    order <- dplyr::left_join(order, selected_keys,
                              by = c("unit_key", "variable_id"), relationship = "many-to-one")
    selected <- order$.selected %in% TRUE
    order$item_order_source <- ifelse(selected, "selection", "not_selected")
    if (".selected_item_id" %in% names(order)) {
      order$item_id[selected] <- as.character(order$.selected_item_id[selected])
    }
  }
  order$item_order <- rep(NA_integer_, nrow(order))
  # Rank all booklets together. Repeatedly scanning and trimming the full
  # variable table for every booklet becomes quadratic in large designs.
  valid_item <- !is.na(order$item_id) & nzchar(trimws(order$item_id))
  index <- which(valid_item | (!is.null(selection) & selected))
  if (length(index)) {
    keys <- order[index, c("booklet_id", "testlet_no", "unit_booklet_no", "unit_key", "unit_alias", "item_id", "variable_id")]
    # An item is represented by one chosen variable. Different variables mapped
    # to the same Studio item do not silently define an aggregation rule.
    mapped <- keys[selected[index] & valid_item[index], , drop = FALSE]
    item_keys <- mapped[setdiff(names(mapped), "variable_id")]
    if (nrow(dplyr::distinct(item_keys)) != nrow(item_keys)) {
      cli::cli_abort("Several variables map to the same item in a unit occurrence. Choose one variable per item using {.arg item_selection}.")
    }
    positions <- order[index, c("testlet_no", "unit_booklet_no", "item_position", "variable_order")]
    local <- do.call(base::order, c(list(match(keys$booklet_id, unique(keys$booklet_id))),
                                    positions, list(na.last = TRUE)))
    unique_keys <- dplyr::distinct(keys[local, , drop = FALSE])
    unique_keys$.item_order <- sequence(rle(unique_keys$booklet_id)$lengths)
    ordered <- dplyr::left_join(keys, unique_keys, by = names(keys), relationship = "many-to-one")
    order$item_order[index[selected[index]]] <- ordered$.item_order[selected[index]]
  }
  dplyr::select(order, -dplyr::any_of(c(".selected", ".selected_item_id")))
}
