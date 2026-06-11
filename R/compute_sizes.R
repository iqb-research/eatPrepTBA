#' Computes resource sizes for Testcenter instances
#'
#' @param data Tibble. Must be a tibble retrieved with `list_files()` with `dependencies = TRUE`.
#'
#' @return The object with estimated sizes of units, booklets, and testtakers file and adds a column `total_size` (in Bytes). Note that this only works for objects retrieved from Testcenter 15.6.0 and higher.
#'
#' @export
compute_sizes <- function(data) {
  cli_setting()

  all_sizes <-
    data %>%
    dplyr::filter(type %in% c("Unit", "Booklet")) %>%
    dplyr::mutate(
      dependencies = purrr::map(dependencies, function(x) {
        x %>%
          purrr::list_transpose() %>%
          tibble::as_tibble()
      }
      )) %>%
    tidyr::unnest(dependencies) %>%
    dplyr::left_join(data %>% dplyr::select(object_name = "name", object_size = "size")) %>%
    dplyr::group_by(type, name, relationship_type) %>%
    dplyr::distinct(object_name, object_size) %>%
    dplyr::summarise(
      total_size = sum(object_size)
    )

  unit_sizes <-
    all_sizes %>%
    dplyr::filter(type == "Unit" & relationship_type == "isDefinedBy") %>%
    # Only unit definitions are accounted for by the algorithm (as of 03/2025)
    dplyr::summarise(
      total_size = sum(total_size)
    )

  booklet_sizes <-
    all_sizes %>%
    dplyr::filter(type == "Booklet") %>%
    dplyr::summarise(
      total_size = sum(total_size) / 1024 ^2
    )

  data %>%
    dplyr::left_join(
      dplyr::bind_rows(
        unit_sizes,
        booklet_sizes
      )
    ) %>%
    dplyr::mutate(
      total_size = ifelse(type == "Testtakers", size, total_size)
    )
}
