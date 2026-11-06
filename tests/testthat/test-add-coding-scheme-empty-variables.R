units_with_empty_variable_metadata <- function() {
  eatPrepTBA:::read_units(list(
    list(id = 1L, key = "STANDARD", name = "Standard unit",
         scheme = minimal_coding_scheme(),
         variables = list(list(id = "v1", alias = "V1", type = "STRING"))),
    list(id = 2L, key = "STARS", name = "StarS unit",
         scheme = NA_character_, variables = list()),
    list(id = 3L, key = "NO_VARIABLES", name = "No variables",
         scheme = NA_character_, variables = NULL)
  ), ws_id = 1L)
}

test_that("empty Studio variable lists coexist with populated tables", {
  units <- units_with_empty_variable_metadata()
  # Exercise the actual reader output, including both [] and null metadata.
  expect_identical(units$unit_variables[[2]], list())
  expect_null(units$unit_variables[[3]])
  expected <- suppressMessages(add_coding_scheme(units[1, ]))
  attr(units, "ws_settings") <- list(label = "Keep settings")
  original <- units

  expect_no_warning(out <- suppressMessages(add_coding_scheme(units)))
  expect_identical(out$unit_key, units$unit_key)
  expect_identical(out$unit_variables, units$unit_variables)
  expect_identical(out$unit_codes[[1]], expected$unit_codes[[1]])
  expect_identical(out$unit_codes[2:3], list(NULL, NULL))
  expect_identical(attr(out, "ws_settings"), attr(units, "ws_settings"))
  expect_identical(units, original)
})

test_that("empty variable metadata works with and without coding schemes", {
  empty_tables <- list(NULL, list(), tibble::tibble(), tibble::tibble(
    variable_ref = character(), variable_type = character(), variable_format = character()
  ))
  for (variables in empty_tables) {
    units <- units_with_empty_variable_metadata()[1, ]
    units$unit_variables <- list(variables)
    expect_no_warning(out <- suppressMessages(add_coding_scheme(units)))
    expect_identical(out$unit_codes[[1]]$variable_id, "V1")
    expect_identical(out$unit_codes[[1]]$variable_ref, "v1")
    expect_equal(out$unit_codes[[1]]$variable_codes[[1]]$code_score, c(1, 0))
    expect_identical(out$unit_variables, units$unit_variables)

    units$coding_scheme <- NA_character_
    expect_no_warning(out <- suppressMessages(add_coding_scheme(units)))
    expect_identical(out$unit_codes, list(NULL))
  }
})

test_that("complete_design preserves supplied StarS results with mixed variable metadata", {
  units <- units_with_empty_variable_metadata()
  design <- tibble::tibble(
    group_id = "G", login_name = "P", booklet_id = "B", booklet_no = 1L,
    testlet_no = 1L, unit_booklet_no = 1L, unit_key = "STARS", unit_alias = "S",
    variable_id = c("interact", "VIDEO", "UNOBSERVED")
  )
  coded <- dplyr::mutate(
    dplyr::select(design[1:2, ], group_id, login_name, booklet_id,
                  unit_key, unit_alias, variable_id),
    value = c("A", "played"), code_id = c(4L, 0L), code_score = c(1, 0),
    code_status = c("CODING_COMPLETE", "VALUE_CHANGED"),
    # Explicit caller-supplied helper types; no credit category is inferred.
    code_type = code_status
  )
  expect_warning(out <- suppressMessages(complete_design(
    coded, units, design, identifiers = c("group_id", "login_name"),
    recode_omissions_to_not_reached = FALSE, preserve_not_reached = FALSE
  )), class = "eatPrepTBA_item_boundary_fallback")

  expect_equal(nrow(out), nrow(design))
  expect_setequal(out$variable_id, design$variable_id)
  supplied <- out[match(coded$variable_id, out$variable_id), names(coded)]
  expect_equal(supplied, coded)
  expect_true(is.na(out$code_status[out$variable_id == "UNOBSERVED"]))
  expect_true(all(is.na(out$variable_position)))
  # A successful join does not establish item relevance or variable positions.
  expect_identical(units$unit_variables[[2]], list())
})
