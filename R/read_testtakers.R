#' Reads a testtakers XML
#'
#' @param testtakers_xml Must be a testtakers XML document.
#'
#' @details
#' Please note that this currently only works if you have a structure of `Group > Login > Booklet` in your testtakers XML.
#'
#' @return A tibble.
#'
#' @export
read_testtakers <- function(testtakers_xml) {
  cli_setting()
  checkmate::assert_class(testtakers_xml, "xml_document")

  # Metadata
  testtakers <-
    testtakers_xml %>%
    rvest::html_elements("Group") %>%
    purrr::map(function(group) {
      group_attr <-
        group %>%
        rvest::html_attrs() %>%
        dplyr::bind_rows()

      group_logins <-
        group %>%
        rvest::html_elements("Login") %>%
        purrr::map(function(login) {
          login_attr <-
            login %>%
            rvest::html_attrs() %>%
            dplyr::bind_rows()

          booklets <-
            login %>%
            rvest::html_elements("Booklet")

          booklet_ids <-
            booklets %>%
            rvest::html_text()

          booklet_attrs <-
            booklets %>%
            rvest::html_attrs() %>%
            dplyr::bind_rows()

          login_booklets <-
            booklet_attrs %>%
            dplyr::mutate(
              booklet_id = booklet_ids
            )

          login_attr %>%
            dplyr::mutate(
              login_booklets = list(login_booklets)
            )
        }) %>%
        dplyr::bind_rows()

      group_attr %>%
        dplyr::mutate(
          group_logins = list(group_logins)
        )
    },
    .progress = list(
        type ="custom",
        format = "Extracting {.testtaker-label testtaker} information ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
        format_done = "Extracted {cli::pb_total} {.testtaker-label testtakers} group{?s} in {cli::pb_elapsed}.",
        clear = FALSE)) %>%
    dplyr::bind_rows()

  testtakers %>%
    dplyr::rename(
      dplyr::any_of(
        c(
          "group_id" = "id",
          "group_label" = "label"
        )
      )
    ) %>%
    tidyr::unnest(dplyr::any_of("group_logins"), keep_empty = TRUE) %>%
    dplyr::rename(
      dplyr::any_of(
        c(
          "login_name" = "name",
          "login_pw" = "pw",
          "login_mode" = "mode"
        )
      )
    ) %>%
    tidyr::unnest(dplyr::any_of("login_booklets"), keep_empty = TRUE) %>%
    dplyr::rename(
      dplyr::any_of(
        c(
          "login_code" = "codes"
        )
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of("login_code"),
                    function(codes) codes %>%  stringr::str_split(" "))
    ) %>%
    tidyr::unnest(dplyr::any_of("login_code")) %>%
    dplyr::group_by(dplyr::across(
      dplyr::any_of(c(
        "group_id", "login_name", "login_code"
      ))
    )) %>%
    dplyr::mutate(
      booklet_no = seq_along(booklet_id)
    ) %>%
    dplyr::ungroup()
}
