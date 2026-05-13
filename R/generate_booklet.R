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
                             login = NULL) {
  cli_setting()
  # input validation
  checkmate::assert_character(booklet_id)
  checkmate::assert_character(booklet_label)
  checkmate::assert_character(booklet_description, null.ok = TRUE)
  checkmate::assert_list(booklet_configuration, null.ok = TRUE)
  # tbd: units, testlets
  checkmate::assert_character(app_version)
  checkmate::assert_character(login, null.ok = TRUE)


  BookletConfig <- rlang::exec("configure_booklet", !!!booklet_configuration)

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

  # Get nodes together
  list(
    Booklet = list(
      list(Metadata = Metadata,
           BookletConfig = BookletConfig,
           Units = Units),
      "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
      "xsi:noNamespaceSchemaLocation" = stringr::str_glue("https://raw.githubusercontent.com/iqb-berlin/testcenter/{app_version}/definitions/vo_Booklet.xsd")
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
