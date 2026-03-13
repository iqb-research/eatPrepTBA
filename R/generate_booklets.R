#' Generates booklet XMLs from booklet, testlet, and unit information
#'
#' @param booklets Must be a tibble with the columns `booklet_id`, `booklet_label`, and `booklet_units`.Optionally, the columns `booklet_description` (character) and `booklet_configuration` (list) can be added. The (list) column `booklet_units` is a nested tibble with columns `testlet_id`, `testlet_label`, and `units`. Optionally, it can contain the column `testlet_restrictions`. Finally, the (list) column `units` is again a nested tibble with columns `unit_key`, `unit_alias`, `unit_label`, and `unit_labelshort`.
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
  prepared_testlets <-
    booklet_units %>%
    dplyr::select(dplyr::any_of(c(
      "id" = "testlet_id",
      "label" = "testlet_label"
    ))) %>%
    as.list() %>%
    purrr::list_transpose(simplify = FALSE)

  prepared_units <-
    booklet_units %>%
    dplyr::pull(units) %>%
    purrr::map(prepare_testlet_units)

  if (tibble::has_name(booklet_units, "testlet_restrictions")) {
    prepared_restrictions <-
      booklet_units %>%
      dplyr::pull(testlet_restrictions) %>%
      purrr::map(prepare_testlet_restrictions)
  } else {
    prepared_restrictions <-
      purrr::map(seq_along(prepared_testlets), function(x) NULL)
  }

  node_names <- c()
  final_testlets <- vector("list", length = length(prepared_testlets))

  for (i in seq_along(prepared_testlets)) {
    # Only units (no testlet)
    current_units <- prepared_units[[i]]
    current_restrictions <- prepared_restrictions[[i]]

    if (is.na(prepared_testlets[[i]]$id)) {
      final_testlets[[i]] <- current_units

    } else {
      # Units nested within testlet
      final_testlets[[i]] <-
        c(prepared_testlets[[i]],
          list(current_restrictions),
          list(current_units)) %>%
        # Workaround to remove restrictions if none apply
        purrr::compact() %>%
        list(Testlet = .)
    }
  }

  final_testlets %>%
    purrr::list_c()
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
