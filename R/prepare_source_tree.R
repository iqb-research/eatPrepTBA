#' Flexibly adds a full version of the variable dependency tree
#'
#' @description
#' Extends the given coding scheme of a unit by its dependency tree. Please note that the tree only comprises variable `variable_ref`, but not `variable_id`.
#'
#' @param coding_scheme Coding scheme as prepared by [get_units()] with setting the argument `coding_scheme = TRUE`.
#'
#' @return A tibble.
#' @keywords internal
prepare_source_tree <- function(coding_scheme) {
  sources_nest <-
    coding_scheme %>%
    eatAutoCode::get_dependency_tree() %>%
    tibble::as_tibble() %>%
    dplyr::select(
      variable_ref = "id",
      variable_level = "level",
      variable_sources = "sources"
    )

  sources_unnest <-
    sources_nest %>%
    tidyr::unnest(variable_sources, keep_empty = TRUE)

  check_dependencies <- sources_unnest$variable_sources %>% purrr::compact()

  if (length(check_dependencies) > 0) {
    sources <-
      sources_nest %>%
      dplyr::mutate(
        variable_sources = purrr::map(variable_sources, function(variable_sources) {
          fill_source_tree(dependencies = sources_unnest,
                           current_sources = variable_sources)
        })
      ) %>%
      dplyr::rename(
        variable_source_ref = "variable_sources"
      ) %>%
      tidyr::unnest(variable_source_ref, keep_empty = TRUE) %>%
      dplyr::left_join(
        sources_nest %>%
          dplyr::select(
            variable_source_ref = "variable_ref",
            variable_source_level = "variable_level"),
        by = dplyr::join_by("variable_source_ref")
      ) %>%
      tidyr::nest(
        variable_sources = c("variable_source_ref", "variable_source_level")
      )
  } else {
    sources_nest %>%
      dplyr::mutate(
        variable_sources = list(tibble::tibble())
      )
  }
}

fill_source_tree <- function(dependencies,
                             current_sources,
                             previous_sources = list()) {
  new_sources <- current_sources

  if (length(current_sources) > 0) {
    add_sources <-
      dependencies %>%
      dplyr::filter(variable_ref %in% unlist(current_sources),
                    variable_level != 0,
                    ! variable_sources %in% unlist(current_sources)) %>%
      dplyr::pull(variable_sources) %>%
      purrr::reduce(union, .init = c())

    if (length(add_sources) > 0 && length(setdiff(add_sources, current_sources)) > 0) {
      new_sources <- union(current_sources, add_sources) %>% unique()

      new_sources <- fill_source_tree(dependencies,
                                      current_sources = new_sources,
                                      previous_sources = current_sources)
    }
  }
  c(new_sources)
}

# Next step:
# prep <-
# coding_scheme %>%
#   prepare_coding_scheme()
#
# test <-
#   prep %>%
#   dplyr::distinct(variable_source_ref = variable_ref, variable_source_id = variable_id)
#
# prep %>%
#   dplyr::distinct(variable_id, variable_ref, source_type, variable_level, variable_sources) %>%
#   tidyr::unnest(variable_sources, keep_empty = TRUE) %>%
#   dplyr::left_join(test)
#
# cs_merge <-
#   units_cs %>%
#   dplyr::distinct(ws_id, unit_id, unit_key,
#                   variable_source_ref = variable_ref, variable_source_id = variable_id)
#
# units_cs %>%
#   dplyr::distinct(ws_id, unit_id, unit_key,
#                   variable_ref, variable_id, variable_sources) %>%
#   tidyr::unnest(variable_sources, keep_empty = TRUE) %>%
#   dplyr::left_join(cs_merge,
#                    by = dplyr::join_by("ws_id", "unit_id", "unit_key",
#                                        "variable_source_ref"))
