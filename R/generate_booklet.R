#' Generates booklets XML from unit information
#'
#' @param booklet_id Character. `Id` of the booklet to be generated.
#' @param booklet_label Character. `Label` of the booklet to be generated.
#' @param booklet_description Character. `Description` of the booklet to be generated. Defaults to `NULL`.
#' @param booklet_configuration A list that can be submitted to [configure_booklet()].
#' @param units Tbd.
#' @param testlets Tbd.
#' @param app_version Version of the target Testcenter instance. Defaults to `"16.0.0"`.
#' @param login Target Testcenter instance. If it is available, the `app_version` will be overwritten.
#' @param booklet_config_version Booklet configuration version. `"18.0"` emits
#'   the current Testcenter booklet configuration keys. `"legacy-16"` emits the
#'   legacy key set used by older Testcenter 16 workflows.
#' @param custom_texts Optional named list of Testcenter custom text keys and
#'   replacement strings. These are emitted as booklet-level `CustomText` nodes
#'   directly after `Metadata`.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @return A booklet XML.
#' @export
generate_booklet <- function(booklet_id,
                             booklet_label,
                             booklet_description = NULL,
                             booklet_configuration = NULL,
                             units = NULL,
                             testlets = NULL,
                             app_version = "16.0.2",
                             login = NULL,
                             booklet_config_version = c("18.0", "legacy-16"),
                             custom_texts = NULL) {
  cli_setting()
  # input validation
  checkmate::assert_character(booklet_id)
  checkmate::assert_character(booklet_label)
  checkmate::assert_character(booklet_description, null.ok = TRUE)
  checkmate::assert_list(booklet_configuration, null.ok = TRUE)
  checkmate::assert_character(app_version, len = 1)
  checkmate::assert_class(login, "LoginTestcenter", null.ok = TRUE)
  checkmate::assert_data_frame(units, null.ok = TRUE)
  checkmate::assert_data_frame(testlets, null.ok = TRUE)
  booklet_config_version <- match.arg(booklet_config_version)

  CustomTexts <- prepare_custom_texts(custom_texts)

  BookletConfig <- prepare_booklet_configuration(
    booklet_configuration,
    booklet_config_version = booklet_config_version
  )

  if (!is.null(login)) {
    app_version <- login@app_version
  }

  # Add nodes
  Metadata <-
    list(
      Id = booklet_id,
      Label = booklet_label,
      Description = booklet_description
    ) %>%
    purrr::compact() %>%
    purrr::map(list)

  if ((is.null(units) & is.null(testlets)) |
      (! is.null(units) & ! is.null(testlets))) {
    cli::cli_alert_danger("Either {.arg units} or {.arg testlets} or must be specified.")
  } else {
    if (! is.null(units)) {
      Units <- prepare_units(units)
    } else if (! is.null(testlets)) {
      Units <- prepare_testlets(testlets)
    }
  }

  booklet_children <- list(
    Metadata = Metadata,
    CustomTexts = CustomTexts,
    BookletConfig = BookletConfig,
    Units = Units
  ) %>%
    purrr::discard(is.null)

  # Get nodes together
  list(
    Booklet = list(
      booklet_children,
      "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
      "xsi:noNamespaceSchemaLocation" = booklet_schema_location(
        booklet_config_version,
        app_version
      )
    ))  %>%
    list_to_xml() %>%
    xml2::as_xml_document()
}

prepare_restrictions <- function(code_to_enter = NULL,
                                 code = NULL,
                                 minutes = NULL,
                                 leave = NULL,
                                 presentation = NULL,
                                 response = NULL) {
  # Following XML Restrictions structure
  Restrictions <-
    list(
      CodeToEnter = list(
        list(code_to_enter),
        code = code
      ),
      TimeMax = list(
        minutes = minutes,
        leave = leave
      ),
      DenyNavigationOnIncomplete = list(
        presentation = presentation,
        response = response
      )
    ) %>%
    purrr::modify_tree(post = purrr::compact)

  if (length(Restrictions) == 0) {
    NULL
  } else {
    list(Restrictions = Restrictions)
  }
}

prepare_units <- function(units) {
  units %>%
    as.list() %>%
    purrr::list_transpose(simplify = FALSE) %>%
    purrr::set_names("Unit")
}

prepare_testlets <- function(testlets) {
  prepared_testlets <-
    testlets %>%
    dplyr::select(id, label) %>%
    as.list() %>%
    purrr::list_transpose(simplify = FALSE)

  prepared_units <-
    testlets %>%
    dplyr::pull(units) %>%
    purrr::map(prepare_units)

  prepared_restrictions <-
    testlets %>%
    dplyr::pull(restrictions) %>%
    purrr::map(function(x) {
      restrictions_list <-
        x %>%
        as.list()

      do.call(prepare_restrictions, args = restrictions_list)
    })

  node_names <- c()
  for (i in seq_along(prepared_testlets)) {
    current_units <- prepared_units[[i]]
    current_restrictions <- prepared_restrictions[[i]]

    if (is.na(prepared_testlets[[i]]$id)) {
      prepared_testlets[[i]] <- current_units$Unit

      node_names[[i]] <- "Unit"
    } else {
      prepared_testlets[[i]] <- c(prepared_testlets[[i]],
                                  list(current_restrictions), list(current_units))

      node_names[[i]] <- "Testlet"
    }
  }

  prepared_testlets %>%
    purrr::set_names(node_names)
}
