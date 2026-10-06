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
#'
#' @details
#' Positions are computed once per booklet, not from participants' responses.
#' All active coding variables are included, even when `design` contains only
#' a variable subset. Booklet identifiers are normalized to uppercase.
#' Basis variables follow page and available numeric section/element paths,
#' with natural variable-name order as a deterministic fallback. Derived
#' variables follow their latest source in dependency order. Unknown or cyclic
#' dependencies are placed in a diagnostic unresolved block.
#' Variables with unknown pages are inserted using names while respecting known
#' physical relationships, rather than being placed at the end of the unit.
#'
#' `variable_order` is a unique, consecutive integer within each booklet.
#' It is a display order, not evidence that every relationship is known.
#' `order_group` retains equal or incomplete physical positions; it can be
#' nonmonotone when unknown-page variables are inserted among known positions.
#' `order_source` records the ordering information used. [recode_missings()]
#' uses names analytically only with `use_variable_names_for_recoding = TRUE`;
#' otherwise it uses known physical relationships and explicit overrides.
#' Item positions use Studio metadata and are consecutive within each booklet.
#' No item identifiers are inferred from variable names.
#'
#' @return A static tibble keyed by `booklet_id`, `testlet_no`,
#'   `unit_booklet_no`, `unit_key`, `unit_alias`, and `variable_id`, with
#'   variable metadata, `variable_order`, `order_group`, `order_source`,
#'   `item_id`, `item_order`, and `item_order_source`.
#' @export
get_design_order <- function(design, units, overwrite = FALSE,
                             order_overrides = NULL, item_selection = NULL) {
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
  if (anyDuplicated(slots)) {
    cli::cli_abort("Conflicting unit occurrences in {.arg design}: each booklet/testlet/unit position must identify one unit and alias.")
  }
  occurrences <- dplyr::arrange(occurrences, .data$booklet_id, .data$testlet_no,
                                .data$unit_booklet_no, .data$unit_key, .data$unit_alias)
  metadata <- design_order_metadata(units, overwrite = overwrite)
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
  for (i in seq_len(nrow(occurrences))) {
    occurrence <- occurrences[i, , drop = FALSE]
    current <- metadata[metadata$unit_key == occurrence$unit_key, , drop = FALSE]
    current <- design_order_unit(current, overrides[[i]])
    keys <- occurrence[rep(1L, nrow(current)), , drop = FALSE]
    tables[[i]] <- dplyr::bind_cols(keys, current[setdiff(names(current), "unit_key")])
  }
  if (!length(tables)) {
    out <- dplyr::bind_cols(occurrences, metadata[0, setdiff(names(metadata), "unit_key")])
    out$variable_order <- integer()
    out$order_group <- integer()
    out$order_source <- character()
  } else {
    out <- dplyr::bind_rows(tables)
    # Local ranks include derivations; occurrence offsets are static per booklet.
    out <- out %>%
      dplyr::group_by(.data$booklet_id) %>%
      dplyr::mutate(variable_order = seq_len(dplyr::n())) %>%
      dplyr::ungroup()
    offsets <- 0L
    previous <- NULL
    cursor <- 1L
    for (current in tables) {
      if (!identical(previous, current$booklet_id[[1L]])) offsets <- 0L
      index <- seq.int(cursor, length.out = nrow(current))
      out$order_group[index] <- current$order_group + offsets
      offsets <- max(out$order_group[index])
      previous <- current$booklet_id[[1L]]
      cursor <- cursor + nrow(current)
    }
  }
  if (any(c("items_list", "item_metadata") %in% names(units))) {
    out <- add_item_id(out, units)
  } else {
    out$item_id <- rep(NA_character_, nrow(out))
  }
  design_order_items(out, item_selection)
}

# Prepare one row per active variable, preserving original source references.
# This helper also accepts its own prepared result to avoid repeated work.
design_order_metadata <- function(units, overwrite = FALSE) {
  checkmate::assert_flag(overwrite)
  assert_cols(units, "unit_key", "units")
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
    units <- add_coding_scheme(units, filter_has_codes = TRUE, overwrite = overwrite)
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
  groups <- dplyr::group_split(dplyr::group_by(raw, .data$unit_key, .data$variable_id))
  merged <- purrr::map(groups, function(group) {
    scalar <- function(column, unknown = NA) {
      values <- unique(group[[column]][!is.na(group[[column]])])
      if (length(values) == 1L) values[[1L]] else unknown
    }
    for (column in c("variable_ref", "variable_source_type", "variable_level")) {
      if (length(unique(stats::na.omit(group[[column]]))) > 1L) {
        cli::cli_abort("Conflicting {.field {column}} for unit/variable {group$unit_key[[1L]]} / {group$variable_id[[1L]]}.")
      }
    }
    refs <- character()
    if ("derive_sources" %in% names(group)) {
      refs <- as.character(unlist(group$derive_sources, use.names = FALSE))
    }
    if (!length(refs) && "variable_sources" %in% names(group)) {
      sources <- dplyr::bind_rows(group$variable_sources)
      if (nrow(sources)) {
        if ("variable_source_direct" %in% names(sources) && any(sources$variable_source_direct %in% TRUE)) {
          sources <- sources[sources$variable_source_direct %in% TRUE, , drop = FALSE]
        }
        if ("variable_source_ref" %in% names(sources)) {
          refs <- as.character(sources$variable_source_ref)
        } else if ("variable_source_id" %in% names(sources)) {
          # Alias-only legacy metadata is resolved explicitly in the unit graph.
          refs <- paste0(".alias:", as.character(sources$variable_source_id))
        }
      }
    }
    refs <- unique(refs[!is.na(refs) & nzchar(refs) & refs != ".alias:NA"])
    path <- function(column) {
      values <- unique(lapply(seq_len(nrow(group)), function(i) group[[column]][[i]]))
      values <- Filter(function(value) length(value) && !all(is.na(value)), values)
      if (length(values) == 1L) values[[1L]] else NA_integer_
    }
    tibble::tibble(unit_key = group$unit_key[[1L]], variable_id = group$variable_id[[1L]],
                   variable_ref = as.character(scalar("variable_ref", NA_character_)),
                   variable_source_type = as.character(scalar("variable_source_type", NA_character_)),
                   variable_level = as.integer(scalar("variable_level", NA_integer_)),
                   variable_page = suppressWarnings(as.numeric(scalar("variable_page", NA_real_))),
                   variable_section = list(path("variable_section")),
                   variable_element = list(path("variable_element")),
                   variable_page_always_visible = as.logical(scalar("variable_page_always_visible")),
                   .source_refs = list(refs))
  })
  out <- dplyr::bind_rows(merged)
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

  if (use_variable_names_for_recoding) {
    naming <- design_order_natural_rank(metadata$variable_id)
    named_before <- outer(naming, naming, `<`)
    conflict <- which(before & !named_before, arr.ind = TRUE)
    if (nrow(conflict)) {
      first <- conflict[1L, 1L]
      second <- conflict[1L, 2L]
      cli::cli_abort(c(
        "Variable-name order conflicts with physical metadata in unit {.val {metadata$unit_key[[first]]}}: {.val {metadata$variable_id[[first]]}} precedes {.val {metadata$variable_id[[second]]}} physically but follows it by name.",
        "i" = "Resolve the conflict with a complete {.arg order_overrides} table instead of confirming variable-name order."
      ))
    }
    return(named_before)
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
  for (booklet in unique(order$booklet_id)) {
    valid_item <- !is.na(order$item_id) & nzchar(trimws(order$item_id))
    index <- which(order$booklet_id == booklet & selected &
                     (valid_item | !is.null(selection)))
    if (!length(index)) next
    keys <- order[index, c("testlet_no", "unit_booklet_no", "unit_key", "unit_alias", "item_id", "variable_id")]
    # An item is represented by one chosen variable. Different variables mapped
    # to the same Studio item do not silently define an aggregation rule.
    mapped <- keys[!is.na(keys$item_id) & nzchar(trimws(keys$item_id)), , drop = FALSE]
    item_keys <- mapped[setdiff(names(mapped), "variable_id")]
    if (anyDuplicated(item_keys)) {
      cli::cli_abort("Several variables map to the same item in a unit occurrence. Choose one variable per item using {.arg item_selection}.")
    }
    unique_keys <- dplyr::distinct(keys)
    unique_keys$.item_order <- seq_len(nrow(unique_keys))
    ordered <- dplyr::left_join(keys, unique_keys, by = names(keys), relationship = "many-to-one")
    order$item_order[index] <- ordered$.item_order
  }
  dplyr::select(order, -dplyr::any_of(c(".selected", ".selected_item_id")))
}
