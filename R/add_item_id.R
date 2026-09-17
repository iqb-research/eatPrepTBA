#' Add item identifiers to variable-level data
#'
#' @param data Data frame containing `unit_key` and `variable_id`.
#' @param units Data frame returned by [get_units()] with `metadata = TRUE`.
#'   Uses its `items_list` list-column, or `item_metadata` after [add_metadata()].
#' @param overwrite Logical. Replace an existing `item_id` column? Defaults to
#'   `FALSE`, which raises an error if the column already exists.
#'
#' @description
#' Adds the item identifier stored in Studio metadata by matching `unit_key`
#' and `variable_id`. No API requests or coding operations are performed.
#' The player definition alone does not contain this metadata mapping.
#'
#' @details
#' Missing or empty keys and item identifiers are ignored in the lookup.
#' Unmatched rows receive `NA_character_`; no item identifiers are invented.
#' Repeated identical mappings are collapsed. Multiple distinct item identifiers
#' for the same pair of keys anywhere in `units` cause an error, including
#' conflicts between workspaces. Subset `units` to the relevant versions first.
#' When both metadata columns exist, `items_list` is used.
#'
#' @return `data` with an `item_id` character column. Row count and order are
#'   preserved. With `overwrite = TRUE`, unmatched rows replace old IDs with `NA`.
#' @export
#' @examples
#' units <- tibble::tibble(
#'   unit_key = "U1",
#'   items_list = list(tibble::tibble(variable_id = "V1", item_id = "I1"))
#' )
#' responses <- tibble::tibble(unit_key = c("U1", "U1"),
#'                             variable_id = c("V1", "V2"))
#' add_item_id(responses, units)
add_item_id <- function(data, units, overwrite = FALSE) {
  assert_cols(data, c("unit_key", "variable_id"), "data")
  assert_cols(units, "unit_key", "units")
  checkmate::assert_flag(overwrite)

  if ("item_id" %in% names(data) && !overwrite) {
    cli::cli_abort("{.arg data} already contains {.field item_id}. Use {.code overwrite = TRUE} to replace it.")
  }

  metadata_column <- intersect(c("items_list", "item_metadata"), names(units))
  if (length(metadata_column) == 0L) {
    cli::cli_abort("{.arg units} must contain {.field items_list} or {.field item_metadata}. Retrieve units with {.code get_units(..., metadata = TRUE)}.")
  }
  metadata_column <- metadata_column[[1L]]
  checkmate::assert_list(units[[metadata_column]])

  empty_mapping <- tibble::tibble(
    unit_key = character(), variable_id = character(), item_id = character()
  )
  mapping <- purrr::map2(units$unit_key, units[[metadata_column]], function(key, items) {
    if (is.null(items)) return(empty_mapping)
    assert_cols(items, c("variable_id", "item_id"), metadata_column)
    tibble::tibble(
      unit_key = rep(as.character(key), nrow(items)),
      variable_id = as.character(items$variable_id),
      item_id = as.character(items$item_id)
    )
  }) %>%
    dplyr::bind_rows(empty_mapping) %>%
    dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.x) & nzchar(trimws(.x)))) %>%
    dplyr::distinct()

  conflicts <- mapping %>%
    dplyr::count(unit_key, variable_id) %>%
    dplyr::filter(.data$n > 1L)
  if (nrow(conflicts) > 0L) {
    keys <- paste0(conflicts$unit_key, " / ", conflicts$variable_id)
    cli::cli_abort("Multiple {.field item_id} values for {.field unit_key / variable_id}: {keys}.")
  }

  data %>%
    dplyr::select(-dplyr::any_of("item_id")) %>%
    dplyr::left_join(mapping, by = c("unit_key", "variable_id"),
                     na_matches = "never", relationship = "many-to-one")
}
