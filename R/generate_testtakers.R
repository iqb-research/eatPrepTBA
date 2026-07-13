#' Generate Testcenter testtakers XML
#'
#' @param testtakers Data frame in the eatPrepTBA testtakers target format.
#'   It must contain `group_id` and `login_name`. In current Testcenter 18
#'   output, `group_label` is required by the XML specification; missing labels
#'   are filled from `group_id` with a warning. Optional columns are
#'   `group_label`, `group_valid_to`, `group_valid_from`, `group_valid_for`,
#'   `login_pw`, `login_mode`, `login_monitor_code`, `booklet_id`,
#'   `booklet_codes`, `booklet_state`, and `profile_id`.
#' @param custom_texts Optional named list of custom text keys and values.
#'   For example, `list(AppTitle = "Pilot")` becomes a `CustomText` node with
#'   key `AppTitle`.
#' @param profiles Optional data frame defining group-monitor profiles. It must
#'   contain `profile_id` when supplied. Optional columns are `profile_label`,
#'   `block_column`, `unit_column`, `view`, `group_column`, `booklet_column`,
#'   `booklet_states_columns`, `filter_pending`, `filter_locked`,
#'   `autoselect_next_block`, `filter_label`, `filter_field`, `filter_type`,
#'   `filter_value`, `filter_sub_value`, and `filter_not`.
#' @param app_version Version of the target Testcenter instance. Defaults to `"16.0.0"`.
#' @param login Target Testcenter instance. If it is available, the `app_version` will be overwritten.
#' @param testtakers_version Testtakers XML specification version. `"18.0"`
#'   emits the current Testcenter testtakers schema URL. `"legacy-16"` emits
#'   the legacy Testcenter 16 schema URL used by older workflows.
#'
#' @details
#' This function expects the final target table for a Testcenter testtakers
#' file. Project-specific wrappers can read Excel files, join booklet designs,
#' expand days or parts, and create monitor logins, but they should eventually
#' hand over a `testtakers` data frame with one row per login/booklet or
#' login/profile assignment.
#'
#' Rows with the same `group_id` form one `Group`. Rows with the same
#' `login_name` form one `Login`; a `login_name` must not be reused across
#' groups. If `booklet_id` is present, `Booklet` children are added to the
#' login. If `profile_id` is present, `Profile` references are added instead.
#' In Testcenter 18.0 output, one login cannot mix `Booklet` and `Profile`
#' children.
#'
#' `generate_testtakers()` returns an XML document only. It does not read input
#' files or write `testtakers.xml`; wrapper functions should call
#' [xml2::write_xml()] if a file should be created.
#'
#' @return An `xml_document` containing a Testcenter `Testtakers` XML document.
#'
#' @export
generate_testtakers <- function(testtakers,
                                custom_texts = NULL,
                                profiles = NULL,
                                app_version = "16.0.2",
                                login = NULL,
                                testtakers_version = c("18.0", "legacy-16")) {
  cli_setting()
  # input validation
  testtakers_cols <- c("group_id", "login_name")
  checkmate::assert_data_frame(testtakers)
  assert_cols(testtakers, testtakers_cols, "testtakers")
  testtakers <- tibble::as_tibble(testtakers)
  assert_required_testtaker_values(testtakers, testtakers_cols, "testtakers")
  validate_unique_testtaker_logins(testtakers)
  checkmate::assert_list(custom_texts, null.ok = TRUE)
  validate_custom_texts(custom_texts)
  checkmate::assert_data_frame(profiles, null.ok = TRUE)
  if(!is.null(profiles)) {
    assert_cols(profiles, "profile_id", "profiles")
    profiles <- tibble::as_tibble(profiles)
    assert_required_testtaker_values(profiles, "profile_id", "profiles")
  }
  checkmate::assert_character(app_version, len = 1)
  checkmate::assert_class(login, "LoginTestcenter", null.ok = TRUE)
  testtakers_version <- match.arg(testtakers_version)


  if (!is.null(login)) {
    app_version <- login@app_version
  }

  testtakers <- prepare_testtakers_for_version(testtakers, testtakers_version)
  validate_testtaker_profiles(testtakers, profiles)
  validate_testtaker_login_children(testtakers, testtakers_version)

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
        "xsi:noNamespaceSchemaLocation" = testtakers_schema_location(
          testtakers_version,
          app_version
        )
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

testtakers_schema_location <- function(testtakers_version, app_version) {
  if (testtakers_version == "legacy-16") {
    stringr::str_glue("https://raw.githubusercontent.com/iqb-berlin/testcenter/{app_version}/definitions/vo_Testtakers.xsd")
  } else {
    stringr::str_glue("https://w3id.org/iqb/spec/testcenter-testtaker-xml/{testtakers_version}")
  }
}

assert_required_testtaker_values <- function(x, cols, data_name) {
  missing_values <-
    cols %>%
    purrr::set_names() %>%
    purrr::map(function(col) which(is_missing_testtaker_value(x[[col]])))

  missing_values <- missing_values[lengths(missing_values) > 0L]

  if (length(missing_values) == 0L) {
    return(invisible(NULL))
  }

  missing_labels <- purrr::imap_chr(
    missing_values,
    function(rows, col) {
      rows_label <- paste(utils::head(rows, 5L), collapse = ", ")
      if (length(rows) > 5L) {
        rows_label <- paste0(rows_label, ", ...")
      }
      paste0(col, " (row", if (length(rows) == 1L) "" else "s", " ", rows_label, ")")
    }
  )

  cli::cli_abort(
    c(
      "Required columns in {.arg {data_name}} must not contain missing or empty values.",
      "x" = "{missing_labels}"
    )
  )
}

validate_custom_texts <- function(custom_texts) {
  if (is.null(custom_texts) || length(custom_texts) == 0L) {
    return(invisible(NULL))
  }

  custom_text_names <- names(custom_texts)
  if (is.null(custom_text_names) ||
      any(is.na(custom_text_names) | custom_text_names == "")) {
    cli::cli_abort(
      "{.arg custom_texts} must be a named list, for example {.code list(AppTitle = \"Pilot\")}."
    )
  }

  invisible(NULL)
}

prepare_testtakers_for_version <- function(testtakers, testtakers_version) {
  if (testtakers_version == "legacy-16") {
    return(testtakers)
  }

  if (!tibble::has_name(testtakers, "group_label")) {
    warning(
      "Testcenter testtakers XML 18.0 requires `group_label`; using `group_id` as group labels.",
      call. = FALSE
    )
    testtakers$group_label <- testtakers$group_id
    return(testtakers)
  }

  missing_group_label <- is_missing_testtaker_value(testtakers$group_label)
  if (any(missing_group_label)) {
    warning(
      "Testcenter testtakers XML 18.0 requires `group_label`; replacing missing group labels with `group_id`.",
      call. = FALSE
    )
    testtakers$group_label[missing_group_label] <- testtakers$group_id[missing_group_label]
  }

  testtakers
}

validate_unique_testtaker_logins <- function(testtakers) {
  login_groups <-
    testtakers %>%
    dplyr::distinct(login_name, group_id) %>%
    dplyr::group_by(login_name) %>%
    dplyr::summarise(
      .groups_for_login = dplyr::n_distinct(group_id),
      .groups = "drop"
    ) %>%
    dplyr::filter(.groups_for_login > 1L)

  if (nrow(login_groups) > 0L) {
    login_labels <- login_groups$login_name
    cli::cli_abort(
      "Each {.field login_name} must belong to only one {.field group_id}; found repeated logins across groups: {.testtaker-id {login_labels}}.",
      wrap = TRUE
    )
  }

  login_attribute_cols <- c("login_pw", "login_mode", "login_monitor_code")
  login_attribute_cols <- intersect(login_attribute_cols, names(testtakers))
  if (length(login_attribute_cols) == 0L) {
    return(invisible(NULL))
  }

  inconsistent_logins <-
    testtakers %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(
      "group_id",
      "login_name",
      login_attribute_cols
    )))) %>%
    dplyr::group_by(group_id, login_name) %>%
    dplyr::summarise(.settings = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(.settings > 1L)

  if (nrow(inconsistent_logins) > 0L) {
    login_labels <- inconsistent_logins %>%
      dplyr::mutate(.label = paste0(group_id, "/", login_name)) %>%
      dplyr::pull(.label)

    cli::cli_abort(
      "Rows for the same {.field group_id}/{.field login_name} must use the same login-level values (`login_pw`, `login_mode`, `login_monitor_code`): {.testtaker-id {login_labels}}.",
      wrap = TRUE
    )
  }

  invisible(NULL)
}

validate_testtaker_profiles <- function(testtakers, profiles) {
  has_applied_profiles <-
    tibble::has_name(testtakers, "profile_id") &&
    any(has_testtaker_value(testtakers$profile_id))

  if (has_applied_profiles && is.null(profiles)) {
    cli::cli_abort(
      "{.arg testtakers} contains {.field profile_id} values. Please provide matching {.arg profiles} definitions or remove {.field profile_id}.",
      wrap = TRUE
    )
  }

  invisible(NULL)
}

validate_testtaker_login_children <- function(testtakers, testtakers_version) {
  if (testtakers_version == "legacy-16" ||
      !all(c("booklet_id", "profile_id") %in% names(testtakers))) {
    return(invisible(NULL))
  }

  mixed_logins <-
    testtakers %>%
    dplyr::mutate(
      .has_booklet = has_testtaker_value(booklet_id),
      .has_profile = has_testtaker_value(profile_id)
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("group_id", "login_name")))) %>%
    dplyr::summarise(
      .has_booklet = any(.has_booklet),
      .has_profile = any(.has_profile),
      .groups = "drop"
    ) %>%
    dplyr::filter(.has_booklet & .has_profile)

  if (nrow(mixed_logins) > 0L) {
    login_labels <- mixed_logins %>%
      dplyr::mutate(.label = paste0(group_id, "/", login_name)) %>%
      dplyr::pull(.label)

    cli::cli_abort(
      "Testcenter testtakers XML 18.0 does not allow {.testtaker-label logins}
      to contain both {.booklet-label booklets} and {.profile-label profiles}.
      Split or remove one child type for {.testtaker-id {login_labels}}.",
      wrap = TRUE
    )
  }

  invisible(NULL)
}

has_testtaker_value <- function(x) {
  !is_missing_testtaker_value(x)
}

is_missing_testtaker_value <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(TRUE)
  }

  if (is.list(x)) {
    return(vapply(x, is_missing_testtaker_value, logical(1)))
  }

  missing <- is.na(x)

  if (is.character(x)) {
    missing <- missing | x == ""
  }

  missing
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
    "bookletStatesColumns" = "booklet_states_columns",
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
                        "subValue" = "filter_sub_value",
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
    "mode" = "login_mode",
    "monitorcode" = "login_monitor_code"
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
    "codes" = "booklet_codes",
    "state" = "booklet_state"
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
