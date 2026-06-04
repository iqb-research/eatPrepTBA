#' Reads a booklet XML
#'
#' @param booklet_xml Must be a booklet XML document.
#'
#' @details
#' Please note that this currently only works if you have a structure of `Units > Testlet > Unit` or `Units > Unit` in your booklet XML. Further nesting is currently not supported. Moreover, it only extracts the unit order.
#'
#' @return A tibble.
#'
#' @export

# booklet_xml <- xml2::read_xml("D:/data/THB001_1.xml")
read_booklet <- function(booklet_xml) {
  # Metadata
  booklet_metadata <-
    booklet_xml %>%
    rvest::html_elements("Metadata") %>%
    purrr::map(function(metadata) {
      metadata_children <- metadata %>% rvest::html_children()
      metadata_values <- metadata_children %>% rvest::html_text2()
      names(metadata_values) <- metadata_children %>% rvest::html_name()
      metadata_values %>% as.list() %>% tibble::as_tibble()
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(
      booklet_id = Id,
      booklet_label = Label
    )

  booklet_units <-
    booklet_xml %>%
    rvest::html_elements("Units") %>%
    rvest::html_children() %>% #.[[1]] -> node
    purrr::map(function(node) {
      node_name <- rvest::html_name(node)

      if (node_name == "Testlet") {
        testlet_attrs <-
          node %>%
          rvest::html_attrs() %>%
          dplyr::bind_rows()

        testlet_units <-
          node %>%
          rvest::html_elements("Unit") %>%
          rvest::html_attrs() %>%
          dplyr::bind_rows()

        testlet_attrs %>%
          dplyr::mutate(
            testlet_units = list(testlet_units)
          )
      } else if (node_name == "Unit") {
        testlet_units <-
          node %>%
          rvest::html_attrs() %>%
          dplyr::bind_rows()

        tibble::tibble(
          testlet_id = NA_character_,
          testlet_label = NA_character_,
          testlet_units = list(testlet_units)
        )
      }
    })

  booklet_metadata %>%
    tidyr::crossing(
      booklet_units
    ) %>%
    tidyr::unnest(
      dplyr::any_of("booklet_units")
    ) %>%
    dplyr::rename(
      dplyr::any_of(c(
        "testlet_id" = "id",
        "testlet_label" = "label"
      ))
    ) %>%
    tidyr::unnest(
      dplyr::any_of("testlet_units")
    ) %>%
    tidyr::nest(
      testlet_units = -dplyr::any_of(c("booklet_id", "booklet_label", "testlet_id", "testlet_label"))
    ) %>%
    dplyr::mutate(
      testlet_no = seq_along(testlet_id)
    ) %>%
    tidyr::unnest(
      dplyr::any_of("testlet_units")
    ) %>%
    dplyr::rename(
      dplyr::any_of(c(
        "unit_key" = "id",
        "unit_label" = "label",
        "unit_labelshort" = "labelshort",
        "unit_alias" = "alias"
      ))
    ) %>%
    dplyr::mutate(
      unit_booklet_no = seq_along(unit_key)
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("testlet_id")))) %>%
    dplyr::mutate(
      unit_testlet_no = seq_along(unit_key)
    ) %>%
    dplyr::ungroup()
}
