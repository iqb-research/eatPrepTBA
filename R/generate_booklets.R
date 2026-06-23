#' Generates booklet XMLs from booklet, testlet, and unit information
#'
#' @param booklets Must be a tibble with the columns `booklet_id`, `booklet_label`, and `booklet_units`.Optionally, the columns `booklet_description` (character) and `booklet_configuration` (list) can be added. The (list) column `booklet_units` is a nested tibble with columns `testlet_id`, `testlet_label`, and `units`. Optionally, it can contain the columns `testlet_restrictions` and `testlets`. Finally, the (list) column `units` is again a nested tibble with columns `unit_key`, `unit_alias`, `unit_label`, and `unit_labelshort`. The optional `testlets` column can contain nested tibbles with the same structure as `booklet_units`.
#' @param app_version Version of the target Testcenter instance. Defaults to `"16.0.0"`.
#' @param login Target Testcenter instance. If it is available, the `app_version` will be overwritten.
#'
#' @description
#' Please note that the function currently only works for units that are nested within
#'
#' @return A booklet XML.
#' @export
generate_booklets <- function(
    booklets,
    app_version = "16.0.2",
    login = NULL
) {

  cli_setting()
  # input validation
  booklets_cols <- c("booklet_id", "booklet_label", "booklet_units")
  checkmate::assert_tibble(booklets)
  assert_cols(booklets, booklets_cols, "booklets")
  checkmate::assert_character(app_version, len = 1)
  checkmate::assert_class(login, "LoginTestcenter", null.ok = TRUE)

  # function
  if (!is.null(login)) {
    app_version <- login@app_version
  }

  missing_cols <-
    tibble::tibble(
      booklet_description = list(NULL),
      booklet_configuration = list(list(NULL)),
      booklet_units = list(NULL)
    )

  cols_to_add <- setdiff(names(missing_cols), names(booklets))

  if (length(cols_to_add) > 0) {
    booklets <-
      booklets %>%
      dplyr::bind_cols(
        missing_cols %>% dplyr::select(dplyr::all_of(cols_to_add))
      )
  }

  booklet_ids <- booklets$booklet_id

  booklets %>%
    dplyr::mutate(
      booklet_metadata = purrr::pmap(
        list(booklet_id, booklet_label, booklet_description),
        prepare_booklet_metadata,
        .progress = list(
          type ="custom",
          extra = list(
            booklet_ids = pad_ids(booklet_ids)
          ),
          format = "Preparing booklet metadata for {.booklet-id {cli::pb_extra$booklet_ids[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
          format_done = "Prepared {cli::pb_total} booklet metadata in {cli::pb_elapsed}.",
          clear = FALSE
        )
      ),
      booklet_configuration = purrr::map(
        booklet_configuration,
        prepare_booklet_configuration,
        .progress = list(
          type ="custom",
          extra = list(
            booklet_ids = pad_ids(booklet_ids)
          ),
          format = "Preparing booklet configuration for {.booklet-id {cli::pb_extra$booklet_ids[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
          format_done = "Prepared {cli::pb_total} booklet configurations in {cli::pb_elapsed}.",
          clear = FALSE
        )
      ),
      booklet_units = purrr::map(
        booklet_units,
        prepare_booklet_units,
        .progress = list(
          type ="custom",
          extra = list(
            booklet_ids = pad_ids(booklet_ids)
          ),
          format = "Preparing booklet units and testlets for {.booklet-id {cli::pb_extra$booklet_ids[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
          format_done = "Prepared {cli::pb_total} booklet units / testlets in {cli::pb_elapsed}.",
          clear = FALSE
        )
      ),
      booklet_xml = purrr::pmap(
        # Hier noch die units
        list(booklet_metadata, booklet_configuration, booklet_units),
        function(booklet_metadata, booklet_configuration, booklet_units) {
          prepare_booklet_xml(booklet_metadata = booklet_metadata,
                              booklet_configuration = booklet_configuration,
                              booklet_units = booklet_units,
                              app_version = app_version)
        },
        .progress = list(
          type ="custom",
          extra = list(
            booklet_ids = pad_ids(booklet_ids)
          ),
          format = "Preparing booklet xmls for {.booklet-id {cli::pb_extra$booklet_ids[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
          format_done = "Prepared {cli::pb_total} booklet xmls in {cli::pb_elapsed}.",
          clear = FALSE
        )
      )
    ) %>%
    dplyr::select(
      -dplyr::any_of(c("booklet_metadata", "booklet_configuration", "booklet_units", "booklet_description"))
    )
}

# Helpers

# Metadata
prepare_booklet_metadata <- function(booklet_id, booklet_label, booklet_description = NULL) {
  list(
    Id = booklet_id,
    Label = booklet_label,
    Description = booklet_description
  ) %>%
    purrr::compact() %>%
    purrr::map(list)
}

# Booklet Configuration
prepare_booklet_configuration <- function(booklet_configuration) {
  rlang::exec("configure_booklet", !!! booklet_configuration)
}

# Testlet restrictions
prepare_testlet_restrictions <- function(testlet_restrictions) {
  rlang::exec("restrict_testlet", !!! testlet_restrictions)
}

# Booklet finalization
prepare_booklet_xml <- function(booklet_metadata, booklet_configuration, booklet_units, app_version) {
  list(
    Booklet = list(
      list(Metadata = booklet_metadata,
           BookletConfig = booklet_configuration,
           Units = booklet_units
      ),
      "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
      "xsi:noNamespaceSchemaLocation" = stringr::str_glue("https://raw.githubusercontent.com/iqb-berlin/testcenter/{app_version}/definitions/vo_Booklet.xsd")
    )) %>%
    list_to_xml() %>%
    xml2::as_xml_document()
}

prepare_testlet_units <- function(units) {
  if (is.null(units) || nrow(units) == 0L) {
    return(list())
  }

  units %>%
    dplyr::select(dplyr::any_of(c(
      "id" = "unit_key",
      "label" = "unit_label",
      "labelshort" = "unit_labelshort",
      "alias" = "unit_alias"
    ))) %>%
    as.list() %>%
    purrr::list_transpose(simplify = FALSE) %>%
    purrr::set_names("Unit")
}

prepare_booklet_units <- function(booklet_units) {
  prepare_booklet_nodes(booklet_units)
}

prepare_booklet_nodes <- function(booklet_units) {
  if (is.null(booklet_units) || nrow(booklet_units) == 0L) {
    return(list())
  }

  purrr::map(seq_len(nrow(booklet_units)), function(i) {
    prepare_booklet_node(booklet_units[i, , drop = FALSE])
  }) %>%
    purrr::list_c()
}

prepare_booklet_node <- function(booklet_unit) {
  testlet_id <- get_booklet_row_value(booklet_unit, "testlet_id", NA_character_)
  testlet_label <- get_booklet_row_value(booklet_unit, "testlet_label", NA_character_)
  units <- get_booklet_row_value(booklet_unit, "units", NULL)
  nested_testlets <- get_booklet_row_value(booklet_unit, "testlets", NULL)
  restrictions <- get_booklet_row_value(booklet_unit, "testlet_restrictions", NULL)

  current_units <- prepare_testlet_units(units)
  current_testlets <- prepare_booklet_nodes(nested_testlets)
  current_restrictions <- prepare_testlet_restrictions(restrictions)

  if (is_missing_booklet_value(testlet_id)) {
    return(c(current_units, current_testlets))
  }

  testlet_attributes <- list(
    id = testlet_id,
    label = if (is_missing_booklet_value(testlet_label)) NULL else testlet_label
  ) %>%
    purrr::compact()

  child_groups <- list(current_restrictions, current_units, current_testlets) %>%
    purrr::discard(is_empty_booklet_node_group)

  c(testlet_attributes, child_groups) %>%
    list(Testlet = .)
}

get_booklet_row_value <- function(booklet_unit, col, default = NULL) {
  if (!tibble::has_name(booklet_unit, col)) {
    return(default)
  }

  value <- booklet_unit[[col]]

  if (length(value) == 0L) {
    default
  } else if (is.list(value)) {
    value[[1]]
  } else {
    value[[1]]
  }
}

is_missing_booklet_value <- function(x) {
  is.null(x) || length(x) == 0L || is.na(x) || identical(x, "")
}

is_empty_booklet_node_group <- function(x) {
  is.null(x) || length(x) == 0L
}

# For tests:
# tibble::tibble(
# booklet_id = "Test",
# booklet_label = "Test",
# testlet_id = c(NA, NA, "Test"),
# testlet_label = c(NA, NA, "Test"),
# unit_key = c("test1", "test2", "test3"),
# unit_label = c("test1", "test2", "test3"),
# unit_alias = c("test1", "test2", "test3"),
# unit_labelshort = c("1", "1", "2")
# ) %>%
#   tidyr::nest(
#     units = starts_with("unit")
#   ) %>%
#   tidyr::nest(
#     booklet_units = c("testlet_id", "testlet_label", "units")
#   ) %>%  #-> booklets
#   generate_booklets() %>%
#   .$booklet_xml %>% .[[1]] %>% as.character() %>% cat()
