#' Generates testtakers XML from unit information
#'
#' @param testtakers Must be a data frame with the columns ...
#' @param custom_texts Optional. List of custom texts to be modified.
#' @param profiles Optional. List of profiles for the group monitor.
#' @param app_version Version of the target Testcenter instance. Defaults to `"16.0.0"`.
#' @param login Target Testcenter instance. If it is available, the `app_version` will be overwritten.
#'
#' @return A testtakers XML.
#'
#' @export
generate_testtakers <- function(testtakers,
                                custom_texts = NULL,
                                profiles = NULL,
                                app_version = "16.0.2",
                                login = NULL) {
  cli_setting()

  if (!is.null(login)) {
    app_version <- login@app_version
  }

  if (!is.null(custom_texts) & length(custom_texts) > 0) {
    CustomTexts <- rlang::exec("customize_texts", !!!custom_texts)
  } else {
    CustomTexts <- list()
  }

  # Add nodes
  Metadata <- list()

  # Ab hier für die Testheftgenerierung relevant
  # testtakers <-
  #   tibble::tibble(
  #     group_id = "test",
  #     group_label = "test",
  #     login_name = c("a", "b"),
  #     login_pw = c("aa", "bb"),
  #     login_mode = c("run-hot-return")
  #   ) %>%
  #   tidyr::crossing(
  #     tidyr::nesting(
  #       booklet_id = c("a1", "a2"),
  #       booklet_codes = c("a1 a2 a3", "a2")
  #     )
  #   )
  #
  # testtakers_mon <-
  #   tibble::tibble(
  #     group_id = "test",
  #     group_label = "test",
  #     login_name = c("monitor"),
  #     login_pw = c("monitor"),
  #     login_mode = c("monitor-group"),
  #     profile_id = c("Anleitung", "TH1", "TH2", "TH3", "TH4")
  #   )
  #
  # testtakers <- testtakers %>%
  #   dplyr::bind_rows(testtakers_mon)
  #
  # parts <- c("Anleitung", "Teil 1", "Teil 2", "Teil 3", "Teil 4")
  #
  # profiles <-
  #   tidyr::crossing(
  #     tidyr::nesting(
  #       profile_id = c("Anleitung", "TH1", "TH2", "TH3", "TH4"),
  #       profile_label = parts,
  #     ),
  #     filter_label = parts
  #   ) %>%
  #   dplyr::filter(profile_label != filter_label) %>%
  #   dplyr::mutate(
  #     block_column = "show",
  #     unit_column = "hide",
  #     view = "full",
  #     group_column = "hide",
  #     booklet_column = "hide",
  #     filter_pending = "no",
  #     filter_locked = "no",
  #     autoselect_next_block = "no",
  #     filter_field = "bookletLabel",
  #     filter_type = "equal",
  #     filter_value = filter_label,
  #     filter_not = "false"
  #   )

  if (!is.null(profiles)) {
    Profiles <- list(
      GroupMonitor = prepare_profiles(profiles)
    )

    # Sanity checks
    if (! tibble::has_name(testtakers, "profile_id")) {
      cli::cli_alert_warning("You have defined {.profile-label profiles},
                             but no {.profile-label profiles} is applied to any
                             of the {.testtaker-label testtaker} groups.",
                             wrap = TRUE)
    } else {
      profile_ids <- unique(profiles$profile_id)
      testtaker_profile_ids <-
        testtakers %>%
        dplyr::filter(!is.na(profile_id)) %>%
        dplyr::distinct(profile_id) %>%
        dplyr::pull(profile_id)

      # Profile applied, but not defined (would potentially break)
      not_defined <- setdiff(testtaker_profile_ids, profile_ids)

      if (length(not_defined) > 0) {
        cli::cli_abort("You have applied the following {.profile-label profiles}
        {.profile-id {not_defined}} to {.testtaker-label testtaker} groups that
                       are not defined. Please define the {.profile-label profiles} beforehand.",
                       wrap = TRUE)
      }

      # Profile defined, but not applied (should not break)
      not_applied <- setdiff(profile_ids, testtaker_profile_ids)

      if (length(not_applied) > 0) {
        cli::cli_alert_warning("You have defined the following {.profile-label profiles}
        {.profile-id {not_applied}} that are not applied to any of the
                               {.testtaker-label testtaker} groups.",
                               wrap = TRUE)
      }
    }
  } else {
    Profiles <- list()
  }

  TesttakerGroups <- prepare_testtaker_groups(testtakers)

  # Get nodes together
  Testtakers <-
    list(
      Testtakers = list(
        c(list(Metadata = Metadata,
               CustomTexts = CustomTexts,
               Profiles = Profiles),
          TesttakerGroups),
        "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
        "xsi:noNamespaceSchemaLocation" = stringr::str_glue("https://raw.githubusercontent.com/iqb-berlin/testcenter/{app_version}/definitions/vo_Testtakers.xsd")
      ))

  # Bug in XSD scheme (CustomTexts MUST be filled)
  if (length(Testtakers$Testtakers[[1]]$CustomTexts) == 0) {
    Testtakers$Testtakers[[1]]$CustomTexts <- NULL
  }

  Testtakers %>%
    list_to_xml() %>%
    xml2::as_xml_document() #%>%
  # as.character() %>%
  # cat()
}

prepare_profiles <- function(profiles) {
  # Profile nodes
  profile_variables <- c(
    "label" = "profile_label",
    "blockColumn" = "block_column",
    "unitColumn" = "unit_column",
    "view" = "view",
    "groupColumn" = "group_column",
    "bookletColumn" = "booklet_column",
    "filterPending" = "filter_pending",
    "filterLocked" = "filter_locked",
    "autoselectNextBlock" = "autoselect_next_block"
  )

  profile_settings <-
    profiles %>%
    dplyr::select(
      "id" = "profile_id",
      dplyr::any_of(profile_variables)
    ) %>%
    dplyr::distinct()

  # Filter nodes
  filter_variables <- c("label" = "filter_label",
                        "field" = "filter_field",
                        "type" = "filter_type",
                        "value" = "filter_value",
                        "not" = "filter_not")

  filters <-
    profiles %>%
    dplyr::select(
      "id" = "profile_id",
      dplyr::any_of(filter_variables)
    ) %>%
    dplyr::distinct()

  # Preparation of nodes
  profiles_prep <-
    profile_settings %>%
    as.list() %>%
    purrr::list_transpose(simplify = FALSE) %>%
    purrr::set_names(purrr::map(., "id"))

  filters_prep <-
    filters %>%
    as.list() %>%
    purrr::list_transpose(simplify = FALSE) %>%
    purrr::set_names(purrr::map(., "id")) %>%
    # Deletes empty nodes (including passwords set to NA)
    purrr::modify_tree(leaf = function(x) if(is.na(x)) NULL else x,
                       post = purrr::compact) %>%
    purrr::map(function(x) {
      x$id <- NULL
      x
    })

  # Insertion of child nodes in parent nodes
  names_profiles <- names(profiles)
  names_filters <- names(filters_prep)

  profiles_prep %>%
    purrr::imap(function(x, i) {
      FilterMerge <- filters_prep[names(filters_prep) == i] %>%
        purrr::set_names("Filter") %>%
        list(.)

      c(x, FilterMerge)
    }) %>%
    purrr::set_names("Profile")
}

# needs to be rebuilt to allow for booklets
prepare_testtaker_groups <- function(testtakers) {
  # Group nodes
  group_variables <- c(
    "label" = "group_label",
    "validTo" = "group_valid_to",
    "validFrom" = "group_valid_from",
    "validFor" = "group_valid_for"
  )

  groups <-
    testtakers %>%
    dplyr::select(
      "id" = "group_id",
      dplyr::any_of(group_variables)
    ) %>%
    dplyr::distinct()

  # Login nodes
  login_variables <- c(
    "name" = "login_name" ,
    "pw" = "login_pw" ,
    "mode" = "login_mode"
  )

  logins <-
    testtakers %>%
    dplyr::select(
      "id" = "group_id",
      dplyr::any_of(login_variables)
    ) %>%
    dplyr::distinct()

  # Booklet nodes
  booklet_variables <- c(
    "booklet" = "booklet_id",
    "codes" = "booklet_codes"#,
    # "login_code" = "login_code"
  )

  booklets <-
    testtakers %>%
    dplyr::filter(
      dplyr::if_any(dplyr::any_of("booklet_id"), .fns = function(x) !is.na(x))
    ) %>%
    dplyr::select(
      "name" = "login_name",
      dplyr::any_of(booklet_variables)
    ) %>%
    dplyr::distinct()

  # TODO: Hier ggf. noch einen Algorithmus einbauen, der die login_codes verkettet
  # if (! tibble::has_name(booklets, "codes") & tibble::has_name(booklets, "login_code")) {
  #   booklets <-
  #     booklets %>%
  #     dplyr::select(
  #       "name",
  #       "login_code"
  #     ) %>%
  #     dplyr::distinct() %>%
  #     dplyr::group_by(dplyr::across(dplyr::any_of(c("name", "booklet")))) %>%
  #     dplyr::summarise(
  #       codes = stringr::str_c(login_code, collapse = " ")
  #     ) %>%
  #     dplyr::ungroup()
  # } else {
  # }

  # Preparation of nodes
  groups_prep <-
    groups %>%
    as.list() %>%
    purrr::list_transpose(simplify = FALSE) %>%
    purrr::set_names(purrr::map(., "id"))

  logins_prep <-
    logins %>%
    as.list() %>%
    purrr::list_transpose(simplify = FALSE) %>%
    purrr::set_names(purrr::map(., "name")) %>%
    # Deletes empty nodes (including passwords set to NA)
    purrr::modify_tree(leaf = function(x) if(is.na(x)) NULL else x,
                       post = purrr::compact)

  booklets_prep <-
    booklets %>%
    as.list() %>%
    purrr::list_transpose(simplify = FALSE) %>%
    purrr::set_names(purrr::map(., "name")) %>%
    purrr::map(function(x) purrr::list_modify(x, name = purrr::zap())) %>%
    # TODO: This line avoids that codes can be added here
    purrr::map(purrr::list_transpose) %>%
    purrr::map_depth(2, as.list) %>%
    purrr::map_depth(2, function(x) {
      is_booklet_name <- names(x) == "booklet"
      booklet <- x[is_booklet_name] %>%
        purrr::set_names(NULL)

      c(list(booklet), x[!is_booklet_name])
    })

  if (tibble::has_name(testtakers, "profile_id")) {
    monitor_profile_variables <- c(
      "id" = "profile_id"
    )

    monitor_profiles <-
      testtakers %>%
      dplyr::filter(!(is.na(profile_id))) %>%
      dplyr::select(
        "name" = "login_name",
        dplyr::any_of(monitor_profile_variables)
      ) %>%
      dplyr::distinct()

    monitor_profiles_prep <-
      monitor_profiles %>%
      as.list() %>%
      purrr::list_transpose(simplify = FALSE) %>%
      purrr::set_names(purrr::map(., "name")) %>%
      purrr::map(function(x) purrr::list_modify(x, name = purrr::zap()))# %>%
    # purrr::map(purrr::list_transpose)
    #
  } else {
    monitor_profiles_prep <- list(NULL)
  }

  # Insertion of child nodes in parent nodes
  names_login <- names(logins_prep)
  names_booklet <- names(booklets_prep)
  names_monitor_profiles <- names(monitor_profiles_prep)

  logins_insert <-
    logins_prep %>%
    purrr::imap(function(x, i) {
      BookletMerge <-
        booklets_prep[names_booklet == i] %>%
        purrr::map(purrr::set_names, "Booklet") %>%
        purrr::keep(names(.) %in% names_booklet) %>%
        unname(.) %>%
        purrr::reduce(c, .init = NULL) %>%
        list(.)

      ProfileMerge <-
        monitor_profiles_prep[names_monitor_profiles == i] %>%
        # purrr::map(purrr::set_names, "id") %>%
        purrr::set_names("Profile") %>%
        list(.)

      c(x, BookletMerge, ProfileMerge) %>%
        purrr::compact()
    }) %>%
    purrr::set_names(purrr::map(., "id")) %>%
    purrr::map(function(x) purrr::list_modify(x, id = purrr::zap()))

  ids_login <- names(logins_insert)

  groups_prep %>%
    purrr::imap(function(x, i) {
      LoginMerge <- logins_insert[ids_login == i] %>%
        purrr::set_names("Login") %>%
        list(.)

      c(x, LoginMerge)
    }) %>%
    purrr::set_names("Group")
}
