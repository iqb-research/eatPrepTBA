#' Summarise item-level missings per person
#'
#' @param data Data frame returned by [complete_design()] and enriched with
#'   [add_item_id()]. Requires `unit_key`, `item_id`, `code_type`, and all
#'   `identifiers`. Unmapped variables have a missing or blank `item_id`.
#' @param identifiers Character vector of columns that jointly identify a
#'   person. All specified columns must exist and contain non-missing,
#'   non-blank values. Include an occasion identifier when summarising separate
#'   administrations of the same items.
#' @param used_only Logical. Keep only people with `id_used = TRUE`? Defaults
#'   to `FALSE`, retaining all people in `data`. If present, `id_used` must be
#'   logical, non-missing, and constant within each person; it is returned in
#'   the result. It is required when `used_only = TRUE`.
#'
#' @description
#' Counts the final missing classifications on item level and divides each
#' count by the number of items assigned to that person in the completed
#' design. Uses `code_type`; neither scores nor technical statuses are used
#' to infer or change classifications.
#'
#' @details
#' An item is identified by `unit_key` and `item_id` together. Variables
#' without an item mapping are excluded from item counts. More than one row
#' for the same person and item is an error, even if the rows are identical.
#' Resolve duplicate mappings or repeated administrations before summarising.
#'
#' The denominator `n_items` includes all mapped items in the person's design,
#' including missing responses and unresolved coding results. Combinations
#' absent from the design are not added. Explicit `MISSING_BY_DESIGN` rows are
#' excluded from both the denominator and missing counts. Retain all design
#' rows before calling this function; response-only data underestimate the
#' denominator and missings.
#'
#' All people in the input are retained by default, including those without
#' mapped items. People without eligible items receive zero counts and
#' `NA_real_` proportions. Empty
#' input returns zero rows with the standard result columns. Person order
#' follows first appearance in the input. Existing grouping is ignored.
#'
#' @return An ungrouped tibble (also a data frame) with one row per person,
#'   the identifiers, optional `id_used`, and `n_items`. Counts have prefix
#'   `n_`; proportions have prefix `prop_` and range from 0 to 1:
#'   * `missing`: total of all `MISSING_*` types except `MISSING_BY_DESIGN`.
#'   * `not_reached`, `by_omission`, `invalid_response`, `coding_impossible`:
#'     the four standard missing types, always included even when absent.
#'   * Additional observed missing types use their full type as a suffix,
#'     converted to lowercase and made syntactically valid with [make.names()],
#'     e.g. `MISSING_TIMEOUT` gives `n_missing_timeout` and
#'     `prop_missing_timeout`. Ambiguous generated names are an error.
#'   * `no_coding`, `coding_incomplete`, `derive_pending`,
#'     `intended_incomplete`: unresolved coding states, always included and
#'     excluded from the `missing` total.
#'   * `unclassified`: missing or blank `code_type`, also excluded from the
#'     `missing` total. Other non-missing types contribute to `n_items` only.
#'
#' @seealso [complete_design()], [add_item_id()]
#' @export
#' @examples
#' data <- tibble::tibble(
#'   person = c("P1", "P1", "P1", "P2"),
#'   unit_key = "U1", item_id = c("01", "02", NA, "01"),
#'   code_type = c("FULL_CREDIT", "MISSING_NOT_REACHED",
#'                 "MISSING_NOT_REACHED", "MISSING_BY_OMISSION"),
#'   id_used = TRUE
#' )
#' summarise_missings(data, identifiers = "person")
summarise_missings <- function(data,
                               identifiers = c("group_id", "login_name", "login_code"),
                               used_only = FALSE) {
  checkmate::assert_character(identifiers, min.len = 1L, any.missing = FALSE,
                              unique = TRUE, min.chars = 1L)
  checkmate::assert_flag(used_only)
  assert_cols(data, c(identifiers, "unit_key", "code_type"), "data")
  if (!"item_id" %in% names(data)) {
    cli::cli_abort("{.arg data} must contain {.field item_id}. Run {.fun add_item_id} first.")
  }
  if (any(identifiers %in% c("unit_key", "item_id", "code_type", "id_used"))) {
    cli::cli_abort("{.arg identifiers} must contain person keys, not item or coding fields.")
  }
  for (key in identifiers) {
    values <- data[[key]]
    if (!is.atomic(values) || !is.null(dim(values)) || anyNA(values) ||
        any(!nzchar(trimws(as.character(values))))) {
      cli::cli_abort("Person identifier {.field {key}} must contain non-missing, non-blank atomic values.")
    }
  }
  for (key in c("unit_key", "item_id", "code_type")) {
    if (!is.character(data[[key]]) && !is.factor(data[[key]])) {
      cli::cli_abort("{.field {key}} must be a character or factor column.")
    }
  }
  has_used <- "id_used" %in% names(data)
  if (used_only && !has_used) {
    cli::cli_abort("{.arg used_only} requires {.field id_used} from {.fun complete_design}.")
  }
  if (has_used) checkmate::assert_logical(data$id_used, any.missing = FALSE)

  # Work with narrow columns; group people once and tabulate counts by index.
  person_cols <- c(identifiers, if (has_used) "id_used")
  data <- dplyr::ungroup(data) %>%
    dplyr::select(dplyr::all_of(c(person_cols, "unit_key", "item_id", "code_type")))
  grouped <- dplyr::group_by(data, dplyr::across(dplyr::all_of(identifiers)),
                            .drop = TRUE)
  person_index <- dplyr::group_indices(grouped)
  n_people <- dplyr::n_groups(grouped)
  first_rows <- match(seq_len(n_people), person_index)
  out <- tibble::as_tibble(data[first_rows, person_cols, drop = FALSE])
  if (has_used && any(data$id_used != out$id_used[person_index])) {
    cli::cli_abort("{.field id_used} must be constant within each person.")
  }

  mapped <- !is.na(data$item_id) & nzchar(trimws(as.character(data$item_id)))
  if (used_only) mapped <- mapped & data$id_used
  items <- data[mapped, , drop = FALSE]
  if (anyNA(items$unit_key) || any(!nzchar(trimws(as.character(items$unit_key))))) {
    cli::cli_abort("Mapped items must have a non-missing, non-blank {.field unit_key}.")
  }
  if (anyDuplicated(items[c(identifiers, "unit_key", "item_id")])) {
    cli::cli_abort(c(
      "Multiple rows for the same person and item ({.field unit_key} / {.field item_id}).",
      "i" = "Resolve duplicate mappings or repeated administrations before summarising."
    ))
  }
  types <- as.character(items$code_type)
  eligible <- !types %in% "MISSING_BY_DESIGN"
  types <- types[eligible]
  item_people <- person_index[mapped][eligible]
  is_missing <- startsWith(types, "MISSING_") %in% TRUE
  standard_types <- c(not_reached = "MISSING_NOT_REACHED",
                       by_omission = "MISSING_BY_OMISSION",
                       invalid_response = "MISSING_INVALID_RESPONSE",
                       coding_impossible = "MISSING_CODING_IMPOSSIBLE")
  extra_types <- sort(setdiff(unique(types[is_missing]), standard_types),
                      method = "radix")
  names(extra_types) <- make.names(tolower(extra_types))
  counted_types <- c(standard_types, extra_types,
                     no_coding = "NO_CODING", coding_incomplete = "CODING_INCOMPLETE",
                     derive_pending = "DERIVE_PENDING", intended_incomplete = "INTENDED_INCOMPLETE")
  suffixes <- c("missing", names(counted_types), "unclassified")
  result_names <- c("n_items", paste0("n_", suffixes), paste0("prop_", suffixes))
  if (anyDuplicated(result_names) || any(identifiers %in% result_names)) {
    cli::cli_abort("Person identifiers or missing types produce conflicting summary column names.")
  }

  out$n_items <- tabulate(item_people, nbins = n_people)
  denominator <- out$n_items
  denominator[denominator == 0L] <- NA_integer_
  counts <- list(missing = tabulate(item_people[is_missing], nbins = n_people))
  for (name in names(counted_types)) {
    counts[[name]] <- tabulate(item_people[types %in% counted_types[[name]]],
                               nbins = n_people)
  }
  unclassified <- is.na(types) | !nzchar(trimws(types))
  counts$unclassified <- tabulate(item_people[unclassified], nbins = n_people)
  for (name in names(counts)) {
    out[[paste0("n_", name)]] <- counts[[name]]
    out[[paste0("prop_", name)]] <- counts[[name]] / denominator
  }
  out <- out[order(first_rows), , drop = FALSE]
  if (used_only) out <- out[out$id_used, , drop = FALSE]
  out
}
