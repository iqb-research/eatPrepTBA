#' Prepare booklet specifications from a tabular block design
#'
#' @param booklets A tibble with one row per booklet. It must contain either
#'   `booklet_id` or `booklet` and one or more `block_*` columns containing the
#'   block ids in booklet order. `booklet_label`, `booklet_description`, and
#'   `booklet_configuration` are optional.
#' @param blocks A tibble with one row per block. It must contain `block` and
#'   may contain `subject`, `domain`, `minutes`, `testlet_id`, `testlet_label`,
#'   and `wrap`. Missing `subject` or `domain` values apply to all matching
#'   unit subjects or domains.
#' @param units A tibble with one row per unit. It must contain `block` and
#'   `unit_key`; `subject`, `domain`, `sequence`, `unit_alias`, `unit_label`,
#'   and `unit_labelshort` are optional.
#' @param restrictions An optional tibble with testlet restriction information.
#'   It may contain `booklet_id`, `subject`, `domain`, `block`, `testlet_id`,
#'   `testlet_label`, `minutes`, `leave`, `code`, `code_to_enter`,
#'   `presentation`, `response`, and `wrap`. Missing `subject` or `domain`
#'   values apply to all matching unit subjects or domains. It can also use the
#'   compact timing format `design`, `block`, `seconds`, and optional
#'   `block_group` and `leave`. `design` is matched as an underscore-delimited
#'   part of `booklet_id`. `block` contains one booklet position column, such as
#'   `"block_4"`. If `block_group` is present, rows with the same `design` and
#'   `block_group` are combined into one timed testlet and `block_group` is used
#'   as the testlet label. If `block_group` is missing or empty, each `block`
#'   becomes its own timed testlet. If `leave` is missing or empty in this
#'   compact format, it defaults to `"allowed"`.
#' @param booklet_subject_fn Optional function that receives `booklet_id` and
#'   returns a subject used to disambiguate blocks that exist for several
#'   subjects.
#' @param add_start_end Logical. Should `Start_page` and `End_page` units be
#'   added to every booklet?
#' @param booklet_filter Optional character vector of booklet ids to keep.
#' @param wrap_blocks Logical. Should blocks be wrapped into testlets by
#'   default?
#' @param default_leave Optional default value for the `leave` attribute of
#'   `TimeMax` restrictions.
#' @param keep_leave_without_minutes Logical. Should `leave` be kept when no
#'   `minutes` restriction is present?
#'
#' @return A tibble that can be passed to [generate_booklets()].
#' @export
prepare_booklets_from_block_design <- function(
    booklets,
    blocks,
    units,
    restrictions = NULL,
    booklet_subject_fn = NULL,
    add_start_end = TRUE,
    booklet_filter = NULL,
    wrap_blocks = TRUE,
    default_leave = NULL,
    keep_leave_without_minutes = FALSE
) {
  checkmate::assert_tibble(booklets)
  checkmate::assert_tibble(blocks)
  checkmate::assert_tibble(units)
  checkmate::assert_tibble(restrictions, null.ok = TRUE)
  checkmate::assert_function(booklet_subject_fn, null.ok = TRUE)
  checkmate::assert_logical(add_start_end, len = 1)
  checkmate::assert_character(booklet_filter, null.ok = TRUE)
  checkmate::assert_logical(wrap_blocks, len = 1)
  checkmate::assert_character(default_leave, len = 1, null.ok = TRUE)
  checkmate::assert_logical(keep_leave_without_minutes, len = 1)

  assert_cols(blocks, "block", "blocks")
  assert_cols(units, c("block", "unit_key"), "units")

  if (is.null(default_leave)) {
    default_leave <- NA_character_
  }

  booklet_design <- standardise_booklet_block_design(booklets)
  booklet_metadata <- booklet_design$booklet_metadata
  booklet_design <- booklet_design$booklet_design

  if (!is.null(booklet_filter)) {
    booklet_design <- booklet_design %>%
      dplyr::filter(.data$booklet_id %in% booklet_filter)
    booklet_metadata <- booklet_metadata %>%
      dplyr::filter(.data$booklet_id %in% booklet_filter)
  }

  if (!is.null(booklet_subject_fn)) {
    booklet_design <- booklet_design %>%
      dplyr::mutate(booklet_subject = booklet_subject_fn(.data$booklet_id))
  } else {
    booklet_design <- booklet_design %>%
      dplyr::mutate(booklet_subject = NA_character_)
  }

  booklet_design <- booklet_design %>%
    dplyr::add_count(.data$booklet_id, .data$block, name = "booklet_block_n") %>%
    dplyr::group_by(.data$booklet_id, .data$block) %>%
    dplyr::mutate(block_occurrence = dplyr::dense_rank(.data$block_pos)) %>%
    dplyr::ungroup()

  units <- standardise_block_units(units)
  blocks <- standardise_blocks(blocks)
  time_restrictions <- prepare_position_time_restrictions(
    restrictions,
    booklet_design
  )
  if (is_position_time_restrictions(restrictions)) {
    restrictions <- NULL
  }
  restrictions <- coerce_booklet_restrictions(restrictions)

  unit_block_scope <- units %>%
    dplyr::distinct(.data$subject, .data$domain, .data$block)

  block_lookup <- resolve_scoped_values(
    scope = unit_block_scope,
    rules = blocks,
    exact_cols = "block"
  ) %>%
    dplyr::add_count(.data$block, name = "block_id_n")

  block_restrictions <- restrictions %>%
    dplyr::filter(is.na(.data$booklet_id) | .data$booklet_id == "") %>%
    dplyr::select(
      "subject", "domain", "block",
      block_restr_testlet_id = "testlet_id",
      block_restr_testlet_label = "testlet_label",
      block_restr_minutes = "minutes",
      block_restr_leave = "leave",
      block_restr_code = "code",
      block_restr_code_to_enter = "code_to_enter",
      block_restr_presentation = "presentation",
      block_restr_response = "response",
      block_restr_wrap = "wrap"
    ) %>%
    resolve_scoped_values(
      scope = unit_block_scope,
      exact_cols = "block"
    )

  booklet_block_scope <- booklet_design %>%
    dplyr::left_join(
      block_lookup %>%
        dplyr::select("subject", "domain", "block", "block_id_n"),
      by = "block",
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(
      .data$block_id_n == 1L |
        is.na(.data$booklet_subject) |
        .data$subject == .data$booklet_subject
    ) %>%
    dplyr::select("booklet_id", "subject", "domain", "block") %>%
    dplyr::distinct()

  booklet_restrictions <- restrictions %>%
    dplyr::filter(!is.na(.data$booklet_id), .data$booklet_id != "") %>%
    dplyr::select(
      "booklet_id", "subject", "domain", "block",
      booklet_restr_testlet_id = "testlet_id",
      booklet_restr_testlet_label = "testlet_label",
      booklet_restr_minutes = "minutes",
      booklet_restr_leave = "leave",
      booklet_restr_code = "code",
      booklet_restr_code_to_enter = "code_to_enter",
      booklet_restr_presentation = "presentation",
      booklet_restr_response = "response",
      booklet_restr_wrap = "wrap"
    ) %>%
    resolve_scoped_values(
      scope = booklet_block_scope,
      exact_cols = c("booklet_id", "block")
    )

  booklet_units <- booklet_design %>%
    dplyr::left_join(block_lookup, by = "block", relationship = "many-to-many") %>%
    dplyr::filter(
      .data$block_id_n == 1L |
        is.na(.data$booklet_subject) |
        .data$subject == .data$booklet_subject
    ) %>%
    dplyr::left_join(
      units,
      by = c("subject", "domain", "block"),
      relationship = "many-to-many"
    ) %>%
    dplyr::left_join(
      block_restrictions,
      by = c("subject", "domain", "block"),
      relationship = "many-to-many"
    ) %>%
    dplyr::left_join(
      booklet_restrictions,
      by = c("booklet_id", "subject", "domain", "block"),
      relationship = "many-to-many"
    ) %>%
    dplyr::left_join(
      time_restrictions,
      by = c("booklet_id", "block_pos")
    ) %>%
    dplyr::mutate(
      minutes = dplyr::coalesce(
        .data$time_restr_minutes,
        .data$minutes,
        .data$block_restr_minutes,
        .data$booklet_restr_minutes
      ),
      leave = dplyr::coalesce(
        .data$time_restr_leave,
        .data$booklet_restr_leave,
        .data$block_restr_leave,
        default_leave
      ),
      leave = dplyr::if_else(
        is.na(.data$minutes) & !keep_leave_without_minutes,
        NA_character_,
        .data$leave
      ),
      code = dplyr::coalesce(.data$booklet_restr_code, .data$block_restr_code),
      code_to_enter = dplyr::coalesce(
        .data$booklet_restr_code_to_enter,
        .data$block_restr_code_to_enter
      ),
      presentation = dplyr::coalesce(
        .data$booklet_restr_presentation,
        .data$block_restr_presentation
      ),
      response = dplyr::coalesce(.data$booklet_restr_response, .data$block_restr_response),
      wrap = dplyr::coalesce(
        .data$time_restr_wrap,
        .data$booklet_restr_wrap,
        .data$block_restr_wrap,
        wrap_blocks
      ),
      testlet_id = dplyr::coalesce(
        .data$time_restr_testlet_id,
        .data$booklet_restr_testlet_id,
        .data$block_restr_testlet_id,
        .data$testlet_id,
        .data$block
      ),
      testlet_id = dplyr::if_else(
        .data$wrap & is.na(.data$time_group_id) & .data$booklet_block_n > 1L,
        paste0(.data$testlet_id, "_", .data$block_pos),
        .data$testlet_id
      ),
      testlet_label = dplyr::coalesce(
        .data$time_restr_testlet_label,
        .data$booklet_restr_testlet_label,
        .data$block_restr_testlet_label,
        .data$testlet_label,
        .data$block
      ),
      testlet_group_id = dplyr::coalesce(
        .data$time_group_id,
        paste0("block_", .data$block_pos)
      ),
      testlet_block_pos = dplyr::coalesce(
        .data$time_group_start_pos,
        .data$block_pos
      ),
      testlet_id = dplyr::if_else(.data$wrap, .data$testlet_id, NA_character_),
      testlet_label = dplyr::if_else(.data$wrap, .data$testlet_label, NA_character_)
    ) %>%
    dplyr::filter(!is.na(.data$unit_key), .data$unit_key != "") %>%
    dplyr::arrange(.data$booklet_id, .data$block_pos, .data$unit_order)

  block_rows <- booklet_units %>%
    dplyr::group_by(
      .data$booklet_id,
      .data$booklet_label,
      .data$testlet_group_id,
      .data$testlet_block_pos,
      .data$testlet_id,
      .data$testlet_label
    ) %>%
    dplyr::summarise(
      testlet_restrictions = list(booklet_restriction_list(
        code = .data$code,
        code_to_enter = .data$code_to_enter,
        minutes = .data$minutes,
        leave = .data$leave,
        presentation = .data$presentation,
        response = .data$response
      )),
      units = list(dplyr::pick(dplyr::any_of(c(
        "unit_key", "unit_alias", "unit_label", "unit_labelshort"
      )))),
      .groups = "drop"
    ) %>%
    dplyr::rename(block_pos = "testlet_block_pos") %>%
    dplyr::select(-"testlet_group_id")

  if (add_start_end) {
    edge_rows <- block_rows %>%
      dplyr::distinct(.data$booklet_id, .data$booklet_label) %>%
      tidyr::crossing(edge = c("start", "end")) %>%
      dplyr::mutate(
        block_pos = dplyr::if_else(.data$edge == "start", 0L, .Machine$integer.max),
        block = dplyr::if_else(.data$edge == "start", "Start_page", "End_page"),
        testlet_id = NA_character_,
        testlet_label = NA_character_,
        testlet_restrictions = list(list()),
        units = purrr::map(.data$block, function(id) {
          tibble::tibble(
            unit_key = id,
            unit_alias = id,
            unit_label = id,
            unit_labelshort = ""
          )
        })
      ) %>%
      dplyr::select(-"edge")

    block_rows <- dplyr::bind_rows(block_rows, edge_rows)
  }

  block_rows %>%
    dplyr::arrange(.data$booklet_id, .data$block_pos) %>%
    dplyr::select(
      "booklet_id",
      "booklet_label",
      "testlet_id",
      "testlet_label",
      "testlet_restrictions",
      "units"
    ) %>%
    dplyr::group_by(.data$booklet_id, .data$booklet_label) %>%
    tidyr::nest(booklet_units = c(
      "testlet_id", "testlet_label", "testlet_restrictions", "units"
    )) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(booklet_metadata, by = c("booklet_id", "booklet_label"))
}

standardise_booklet_block_design <- function(booklets) {
  booklet_id_col <- dplyr::case_when(
    "booklet_id" %in% names(booklets) ~ "booklet_id",
    "booklet" %in% names(booklets) ~ "booklet",
    TRUE ~ NA_character_
  )

  if (is.na(booklet_id_col)) {
    stop(
      "'booklets' must contain either 'booklet_id' or 'booklet'.",
      call. = FALSE
    )
  }

  block_cols <- names(booklets)[stringr::str_detect(names(booklets), "^block_")]
  if (length(block_cols) == 0L) {
    stop(
      "'booklets' must contain one or more columns starting with 'block_'.",
      call. = FALSE
    )
  }

  booklets <- booklets %>%
    add_missing_columns(list(booklet_label = NA_character_)) %>%
    dplyr::mutate(
      booklet_id = as.character(.data[[booklet_id_col]]),
      booklet_label = dplyr::coalesce(
        as.character(.data[["booklet_label"]]),
        .data$booklet_id
      )
    )

  metadata_cols <- c("booklet_description", "booklet_configuration")
  booklet_metadata <- booklets %>%
    dplyr::select("booklet_id", "booklet_label", dplyr::any_of(metadata_cols)) %>%
    dplyr::distinct()

  booklet_design <- booklets %>%
    tidyr::pivot_longer(
      dplyr::all_of(block_cols),
      names_to = "block_column",
      values_to = "block"
    ) %>%
    dplyr::filter(!is.na(.data$block), .data$block != "") %>%
    dplyr::mutate(
      block_pos = match(.data$block_column, block_cols),
      block = as.character(.data$block)
    ) %>%
    dplyr::select("booklet_id", "booklet_label", "block_pos", "block")

  list(
    booklet_metadata = booklet_metadata,
    booklet_design = booklet_design
  )
}

standardise_blocks <- function(blocks) {
  blocks <- add_missing_columns(
    blocks,
    list(
      subject = NA_character_,
      domain = NA_character_,
      minutes = NA_real_,
      testlet_id = NA_character_,
      testlet_label = NA_character_,
      wrap = NA
    )
  )

  blocks %>%
    dplyr::transmute(
      subject = as.character(.data$subject),
      domain = as.character(.data$domain),
      block = as.character(.data$block),
      minutes = as.numeric(.data$minutes),
      testlet_id = as.character(.data$testlet_id),
      testlet_label = as.character(.data$testlet_label),
      wrap = as.logical(.data$wrap)
    )
}

standardise_block_units <- function(units) {
  units <- add_missing_columns(
    units,
    list(
      subject = NA_character_,
      domain = NA_character_,
      unit_alias = NA_character_,
      unit_label = NA_character_,
      unit_labelshort = NA_character_,
      sequence = NA_real_
    )
  )

  units %>%
    dplyr::filter(!is.na(.data$unit_key), .data$unit_key != "") %>%
    dplyr::group_by(.data$subject, .data$domain, .data$block) %>%
    dplyr::mutate(unit_row = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      subject = as.character(.data$subject),
      domain = as.character(.data$domain),
      block = as.character(.data$block),
      unit_key = as.character(.data$unit_key),
      unit_alias = dplyr::coalesce(
        as.character(.data$unit_alias),
        as.character(.data$unit_key)
      ),
      unit_label = dplyr::coalesce(
        as.character(.data$unit_label),
        as.character(.data$unit_key)
      ),
      unit_labelshort = dplyr::coalesce(
        as.character(.data$unit_labelshort),
        ""
      ),
      unit_order = dplyr::coalesce(as.numeric(.data$sequence), .data$unit_row)
    )
}

empty_booklet_restrictions <- function() {
  tibble::tibble(
    booklet_id = character(),
    subject = character(),
    domain = character(),
    block = character(),
    testlet_id = character(),
    testlet_label = character(),
    minutes = numeric(),
    leave = character(),
    code = character(),
    code_to_enter = character(),
    presentation = character(),
    response = character(),
    wrap = logical()
  )
}

coerce_booklet_restrictions <- function(restrictions = NULL) {
  if (is.null(restrictions) || nrow(restrictions) == 0L) {
    return(empty_booklet_restrictions())
  }

  restrictions <- add_missing_columns(
    restrictions,
    as.list(empty_booklet_restrictions()[NA_integer_, ])
  )

  restrictions %>%
    dplyr::transmute(
      booklet_id = as.character(.data$booklet_id),
      subject = as.character(.data$subject),
      domain = as.character(.data$domain),
      block = as.character(.data$block),
      testlet_id = as.character(.data$testlet_id),
      testlet_label = as.character(.data$testlet_label),
      minutes = as.numeric(.data$minutes),
      leave = as.character(.data$leave),
      code = as.character(.data$code),
      code_to_enter = as.character(.data$code_to_enter),
      presentation = as.character(.data$presentation),
      response = as.character(.data$response),
      wrap = as.logical(.data$wrap)
    )
}

resolve_scoped_values <- function(scope,
                                  rules,
                                  exact_cols,
                                  scope_cols = c("subject", "domain")) {
  value_cols <- setdiff(names(rules), c(exact_cols, scope_cols))
  scope <- scope %>%
    dplyr::select(dplyr::all_of(c(exact_cols, scope_cols))) %>%
    dplyr::distinct()

  if (length(value_cols) == 0L) {
    return(scope)
  }

  rule_scope_cols <- paste0(".rule_", scope_cols)
  rules <- rules %>%
    dplyr::mutate(.rule_row = dplyr::row_number()) %>%
    dplyr::rename_with(
      function(x) paste0(".rule_", x),
      dplyr::all_of(scope_cols)
    )

  matched <- scope %>%
    dplyr::left_join(rules, by = exact_cols, relationship = "many-to-many")

  for (i in seq_along(scope_cols)) {
    matched <- matched %>%
      dplyr::filter(
        is.na(.data[[rule_scope_cols[[i]]]]) |
          .data[[scope_cols[[i]]]] == .data[[rule_scope_cols[[i]]]]
      )
  }

  matched_keys <- matched %>%
    dplyr::select(dplyr::all_of(c(exact_cols, scope_cols))) %>%
    dplyr::distinct()
  unmatched <- scope %>%
    dplyr::anti_join(matched_keys, by = c(exact_cols, scope_cols))

  if (nrow(unmatched) > 0L) {
    for (col in setdiff(names(matched), names(unmatched))) {
      unmatched[[col]] <- rep(matched[[col]][NA_integer_][[1]], nrow(unmatched))
    }
    matched <- dplyr::bind_rows(matched, unmatched)
  }

  matched %>%
    dplyr::mutate(
      .specificity = rowSums(!is.na(dplyr::pick(dplyr::all_of(rule_scope_cols)))),
      .rule_row = dplyr::coalesce(.data$.rule_row, .Machine$integer.max)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$.specificity), .data$.rule_row) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(exact_cols, scope_cols)))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(value_cols), first_scoped_value),
      .groups = "drop"
    )
}

first_scoped_value <- function(x) {
  if (is.character(x)) {
    x <- x[!is.na(x) & x != ""]
  } else {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0L) {
    x[NA_integer_][[1]]
  } else {
    x[[1]]
  }
}

empty_position_time_restrictions <- function() {
  tibble::tibble(
    booklet_id = character(),
    block_pos = integer(),
    time_group_id = character(),
    time_group_start_pos = integer(),
    time_restr_testlet_id = character(),
    time_restr_testlet_label = character(),
    time_restr_minutes = numeric(),
    time_restr_leave = character(),
    time_restr_wrap = logical()
  )
}

is_position_time_restrictions <- function(restrictions = NULL) {
  !is.null(restrictions) &&
    nrow(restrictions) > 0L &&
    all(c("design", "block", "seconds") %in% names(restrictions))
}

prepare_position_time_restrictions <- function(restrictions = NULL,
                                               booklet_design) {
  if (!is_position_time_restrictions(restrictions)) {
    if (
      !is.null(restrictions) &&
        nrow(restrictions) > 0L &&
        all(c("design", "seconds") %in% names(restrictions))
    ) {
      stop(
        "Compact timing restrictions must contain 'design', 'block', ",
        "and 'seconds' columns. 'block_group' is optional.",
        call. = FALSE
      )
    }
    return(empty_position_time_restrictions())
  }

  restrictions <- add_missing_columns(
    restrictions,
    list(
      block_group = NA_character_,
      leave = NA_character_
    )
  )

  time_groups <- restrictions %>%
    dplyr::mutate(
      design = stringr::str_squish(as.character(.data$design)),
      block = stringr::str_squish(as.character(.data$block)),
      block_group = stringr::str_squish(as.character(.data$block_group)),
      block_group = dplyr::na_if(.data$block_group, ""),
      block_group = dplyr::coalesce(.data$block_group, .data$block),
      seconds = suppressWarnings(as.numeric(.data$seconds)),
      leave = stringr::str_squish(as.character(.data$leave)),
      leave = dplyr::na_if(.data$leave, ""),
      leave = dplyr::coalesce(.data$leave, "allowed"),
      time_restr_minutes = .data$seconds / 60,
      time_restr_leave = .data$leave,
      time_restr_wrap = TRUE
    )

  if (any(is.na(time_groups$design) | time_groups$design == "")) {
    stop(
      "Compact timing restrictions must contain non-empty 'design' values.",
      call. = FALSE
    )
  }

  if (any(is.na(time_groups$block) | time_groups$block == "")) {
    stop(
      "Compact timing restrictions 'block' must contain values like 'block_4'.",
      call. = FALSE
    )
  }

  if (any(is.na(time_groups$seconds))) {
    stop(
      "Compact timing restrictions must contain numeric 'seconds' values.",
      call. = FALSE
    )
  }

  time_groups <- time_groups %>%
    dplyr::mutate(
      block_pos = as.integer(stringr::str_match(.data$block, "^block_(\\d+)$")[, 2])
    )

  if (any(is.na(time_groups$block_pos))) {
    stop(
      "Compact timing restrictions 'block' must contain values like 'block_4'.",
      call. = FALSE
    )
  }

  inconsistent_groups <- time_groups %>%
    dplyr::group_by(.data$design, .data$block_group) %>%
    dplyr::summarise(
      seconds_n = dplyr::n_distinct(.data$seconds),
      leave_n = dplyr::n_distinct(.data$leave),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$seconds_n > 1L | .data$leave_n > 1L)

  if (nrow(inconsistent_groups) > 0L) {
    stop(
      "Compact timing restrictions must use one 'seconds' and one 'leave' ",
      "value per 'design'/'block_group'.",
      call. = FALSE
    )
  }

  nonconsecutive_groups <- time_groups %>%
    dplyr::distinct(.data$design, .data$block_group, .data$block_pos) %>%
    dplyr::group_by(.data$design, .data$block_group) %>%
    dplyr::summarise(
      consecutive = all(diff(sort(.data$block_pos)) == 1L),
      .groups = "drop"
    ) %>%
    dplyr::filter(!.data$consecutive)

  if (nrow(nonconsecutive_groups) > 0L) {
    stop(
      "Compact timing restrictions must use consecutive 'block' values ",
      "within each 'design'/'block_group'.",
      call. = FALSE
    )
  }

  time_groups <- time_groups %>%
    dplyr::arrange(.data$design, .data$block_group, .data$block_pos) %>%
    dplyr::group_by(.data$design, .data$block_group) %>%
    dplyr::mutate(
      time_group_start_pos = min(.data$block_pos),
      time_group_id = paste0(
        .data$design,
        "_",
        stringr::str_replace_all(.data$block_group, "\\s+", "_")
      ),
      time_restr_testlet_id = .data$time_group_id,
      time_restr_testlet_label = .data$block_group
    ) %>%
    dplyr::ungroup()

  booklet_design %>%
    dplyr::select("booklet_id", "block_pos") %>%
    dplyr::inner_join(time_groups, by = "block_pos", relationship = "many-to-many") %>%
    dplyr::filter(booklet_design_matches(.data$booklet_id, .data$design)) %>%
    dplyr::transmute(
      booklet_id = .data$booklet_id,
      block_pos = .data$block_pos,
      time_group_id = .data$time_group_id,
      time_group_start_pos = .data$time_group_start_pos,
      time_restr_testlet_id = .data$time_restr_testlet_id,
      time_restr_testlet_label = .data$time_restr_testlet_label,
      time_restr_minutes = .data$time_restr_minutes,
      time_restr_leave = .data$time_restr_leave,
      time_restr_wrap = .data$time_restr_wrap
    ) %>%
    dplyr::distinct()
}

booklet_design_matches <- function(booklet_id, design) {
  stringr::str_detect(
    paste0("_", booklet_id, "_"),
    stringr::fixed(paste0("_", design, "_"))
  )
}

booklet_restriction_list <- function(code,
                                     code_to_enter,
                                     minutes,
                                     leave,
                                     presentation,
                                     response) {
  list(
    code = first_present_booklet_value(code),
    code_to_enter = first_present_booklet_value(code_to_enter),
    minutes = first_present_booklet_value(minutes),
    leave = first_present_booklet_value(leave),
    presentation = first_present_booklet_value(presentation),
    response = first_present_booklet_value(response)
  ) %>%
    purrr::compact()
}

first_present_booklet_value <- function(x) {
  x <- x[!is.na(x)]
  if (is.character(x)) {
    x <- x[x != ""]
  }

  if (length(x) == 0L) {
    NULL
  } else {
    x[[1]]
  }
}

add_missing_columns <- function(x, defaults) {
  for (col in setdiff(names(defaults), names(x))) {
    x[[col]] <- defaults[[col]]
  }
  x
}
