missing_summary_fixture <- function() {
  tibble::tibble(
    person = c(rep("P2", 7), "P1", "P1", "P0"),
    unit_key = c(rep("U1", 9), NA_character_),
    item_id = c("01", "02", "03", "04", "05", "06", NA, "01", "02", NA),
    code_type = c("FULL_CREDIT", "MISSING_NOT_REACHED", "MISSING_BY_OMISSION",
                  "MISSING_INVALID_RESPONSE", "MISSING_CODING_IMPOSSIBLE", "RESIDUAL",
                  "MISSING_NOT_REACHED", "MISSING_NOT_REACHED", "MISSING_NOT_REACHED", NA),
    id_used = c(rep(TRUE, 7), FALSE, FALSE, TRUE)
  )
}

test_that("each person's design items are the denominator for all missing types", {
  data <- missing_summary_fixture()
  # Deliberately misleading statuses, scores and custom missing codes.
  data$code_status <- "NOT_REACHED"
  data$code_score <- c(NA, 1, 0, 0, NA, 0, NA, NA, NA, NA)
  data$code_id <- 999L
  before <- data
  out <- summarise_missings(data, identifiers = "person")

  expect_identical(out$person, c("P2", "P1", "P0"))
  expect_identical(out$id_used, c(TRUE, FALSE, TRUE))
  expect_identical(out$n_items, c(6L, 2L, 0L))
  expect_identical(out$n_missing, c(4L, 2L, 0L))
  expect_equal(out$prop_missing, c(4 / 6, 1, NA_real_))
  expect_identical(out$n_not_reached, c(1L, 2L, 0L))
  expect_equal(out$prop_not_reached, c(1 / 6, 1, NA_real_))
  for (type in c("by_omission", "invalid_response", "coding_impossible")) {
    expect_identical(out[[paste0("n_", type)]], c(1L, 0L, 0L))
    expect_equal(out[[paste0("prop_", type)]], c(1 / 6, 0, NA_real_))
  }
  expect_identical(out$n_unclassified, c(0L, 0L, 0L))
  expect_true(all(is.na(unlist(out[3, startsWith(names(out), "prop_")]))))
  expect_identical(data, before)
})

test_that("used_only filters people while default retains unused and itemless people", {
  data <- missing_summary_fixture()
  all <- summarise_missings(data, identifiers = "person")
  used <- summarise_missings(data, identifiers = "person", used_only = TRUE)
  expect_identical(used, all[all$id_used, ])
  expect_identical(used$person, c("P2", "P0"))
  data$id_used <- FALSE
  expect_equal(nrow(summarise_missings(data, identifiers = "person", used_only = TRUE)), 0L)
  data$id_used <- NULL
  expect_equal(nrow(summarise_missings(data, identifiers = "person")), 3L)
  expect_error(summarise_missings(data, identifiers = "person", used_only = TRUE), "id_used")
})

test_that("custom missing types and unresolved coding results remain distinguishable", {
  types <- c("MISSING_TIMEOUT", "MISSING_TIMEOUT", "NO_CODING", "CODING_INCOMPLETE",
             "DERIVE_PENDING", "INTENDED_INCOMPLETE", NA, " ", "FULL_CREDIT", "RESIDUAL")
  data <- tibble::tibble(person = "P1", unit_key = "U1",
                         item_id = as.character(seq_along(types)), code_type = factor(types))
  out <- summarise_missings(data, identifiers = "person")
  expect_identical(out$n_items, 10L)
  expect_identical(out$n_missing, 2L)
  expect_equal(out$prop_missing, 0.2)
  expect_identical(out$n_missing_timeout, 2L)
  expect_equal(out$prop_missing_timeout, 0.2)
  expect_identical(out$n_unclassified, 2L)
  expect_equal(out$prop_unclassified, 0.2)
  for (type in c("no_coding", "coding_incomplete", "derive_pending", "intended_incomplete")) {
    expect_identical(out[[paste0("n_", type)]], 1L)
    expect_equal(out[[paste0("prop_", type)]], 0.1)
  }
  expect_identical(out$n_not_reached, 0L)
  expect_equal(out$prop_not_reached, 0)
})

test_that("design missings and unmapped variables do not enter item counts", {
  data <- tibble::tibble(
    person = c("P1", "P1", "P1", "P2"), unit_key = "U1",
    item_id = c("01", "", "  ", "01"),
    code_type = c("MISSING_BY_DESIGN", "MISSING_NOT_REACHED",
                  "MISSING_NOT_REACHED", "FULL_CREDIT")
  )
  out <- summarise_missings(data, identifiers = "person")
  expect_identical(out$n_items, c(0L, 1L))
  expect_identical(out$n_missing, c(0L, 0L))
  expect_equal(out$prop_missing, c(NA_real_, 0))
  expect_false(any(grepl("by_design", names(out))))
})

test_that("item keys include units and preserve leading zeros", {
  data <- tibble::tibble(person = "P1", unit_key = factor(c("U1", "U2", "U2")),
                         item_id = factor(c("01", "01", "1")),
                         code_type = c("FULL_CREDIT", "MISSING_NOT_REACHED", "FULL_CREDIT"))
  out <- summarise_missings(data, identifiers = "person")
  expect_identical(out$n_items, 3L)
  expect_equal(out$prop_not_reached, 1 / 3)
})

test_that("duplicate items and repeated administrations require an explicit resolution", {
  data <- tibble::tibble(person = "P1", unit_key = "U1", item_id = "01",
                         code_type = "FULL_CREDIT", occasion = 1:2)
  expect_error(summarise_missings(data, identifiers = "person"), "Multiple rows.*person and item")
  expect_error(summarise_missings(data[c(1, 1), ], identifiers = "person"), "Multiple rows")
  data$variable_id <- c("source", "derived")
  expect_error(summarise_missings(data, identifiers = "person"), "Multiple rows")
  out <- summarise_missings(data, identifiers = c("person", "occasion"))
  expect_identical(out$n_items, c(1L, 1L))
  data$item_id <- NA_character_
  expect_identical(summarise_missings(data, identifiers = "person")$n_items, 0L)
})

test_that("empty input and unused factor levels have a stable standard schema", {
  data <- missing_summary_fixture()
  data$person <- factor(data$person, levels = c("P1", "P0", "P2", "absent"))
  data <- dplyr::group_by(data, .data$person, .drop = FALSE)
  out <- summarise_missings(data, identifiers = "person")
  expect_equal(nrow(out), 3L)
  expect_identical(as.character(out$person), c("P2", "P1", "P0"))
  expect_false(dplyr::is_grouped_df(out))
  empty <- summarise_missings(data[0, ], identifiers = "person")
  expect_identical(empty, out[0, ])
  base_out <- summarise_missings(as.data.frame(data), identifiers = "person")
  expect_identical(base_out, out)
})

test_that("all default person identifiers participate in grouping", {
  data <- tibble::tibble(group_id = c("G1", "G2", "G1", "G1"),
                         login_name = c("L1", "L1", "L2", "L1"),
                         login_code = c("C1", "C1", "C1", "C2"),
                         unit_key = "U1", item_id = "01", code_type = "MISSING_NOT_REACHED")
  out <- summarise_missings(data)
  expect_identical(out[1:3], data[1:3])
  expect_identical(out$n_items, rep(1L, 4))
  expect_identical(out$prop_not_reached, rep(1, 4))
  expect_error(summarise_missings(data[setdiff(names(data), "login_code")]), "login_code")
})

test_that("row order and irrelevant columns do not change the summary", {
  data <- missing_summary_fixture()
  data <- dplyr::bind_rows(data, dplyr::mutate(data[1, ], item_id = "07",
                                             code_type = "MISSING_TIMEOUT"))
  expected <- summarise_missings(data, identifiers = "person") |>
    dplyr::arrange(.data$person)
  shuffled <- data[c(8, 3, 11, 10, 1, 5, 9, 7, 2, 4, 6), ]
  shuffled$item <- "ignored external item label"
  out <- summarise_missings(shuffled, identifiers = "person") |>
    dplyr::arrange(.data$person)
  expect_identical(out, expected)
  missing_counts <- out$n_not_reached + out$n_by_omission + out$n_invalid_response +
    out$n_coding_impossible + out$n_missing_timeout
  expect_identical(out$n_missing, missing_counts)
  expect_true(all(out$n_missing <= out$n_items))
})

test_that("validation prevents ambiguous keys, filtering and column names", {
  data <- missing_summary_fixture()
  run <- function(x, ...) summarise_missings(x, identifiers = "person", ...)
  expect_error(run(data[setdiff(names(data), "item_id")]), "add_item_id")
  expect_error(run(data[setdiff(names(data), "code_type")]), "code_type")
  expect_error(run(data, used_only = NA), "used_only")
  expect_error(summarise_missings(data, identifiers = character()), "identifiers")
  expect_error(summarise_missings(data, identifiers = c("person", "person")), "identifiers")
  expect_error(summarise_missings(data, identifiers = "item_id"), "person keys")
  for (value in c(NA_character_, "", " ")) {
    bad <- data
    bad$person[1] <- value
    expect_error(run(bad), "Person identifier")
    bad <- data
    bad$unit_key[1] <- value
    expect_error(run(bad), "unit_key")
  }
  bad <- data
  bad$id_used[1] <- FALSE
  expect_error(run(bad), "constant within each person")
  expect_error(run(bad, used_only = TRUE), "constant within each person")
  bad$id_used[1] <- NA
  expect_error(run(bad), "id_used")
  bad$id_used <- rep(1L, nrow(bad))
  expect_error(run(bad), "id_used")
  bad <- data
  bad$item_id <- seq_len(nrow(bad))
  expect_error(run(bad), "item_id.*character or factor")
  bad <- dplyr::rename(data, n_items = person)
  expect_error(summarise_missings(bad, identifiers = "n_items"), "conflicting summary column names")
  bad <- data
  bad$code_type[1:2] <- c("MISSING_TIMEOUT", "MISSING_timeout")
  expect_error(run(bad), "conflicting summary column names")
})

test_that("completion and metadata mapping retain unanswered items and unused people", {
  # complete_design() currently uses dplyr::case_match(), deprecated in 1.2.0.
  rlang::local_options(lifecycle_verbosity = "quiet")
  units <- minimal_units()
  codes <- units$unit_codes[[1]][rep(1, 4), ]
  codes$variable_id <- c("V01a", "V01b", "V01", "V02")
  codes$variable_ref <- tolower(codes$variable_id)
  codes$variable_source_type[3] <- "SUM_CODE"
  codes$variable_sources[[3]] <- tibble::tibble(
    variable_source_ref = c("v01a", "v01b"), variable_source_direct = TRUE
  )
  units$unit_codes <- list(codes)
  units$items_list <- list(tibble::tibble(variable_id = c("V01", "V02"),
                                         item_id = c("01", "02")))
  design <- tidyr::expand_grid(person = c("P1", "P2"), variable_id = codes$variable_id) |>
    dplyr::mutate(unit_key = "U1", unit_alias = "U1", booklet_id = "B1",
                  booklet_no = 1L, testlet_no = 1L, unit_booklet_no = 1L)
  coded <- design |>
    dplyr::filter(.data$person == "P1", .data$variable_id != "V02") |>
    dplyr::select(dplyr::all_of(c("person", "unit_key", "unit_alias", "booklet_id", "variable_id"))) |>
    dplyr::mutate(code_status = "CODING_COMPLETE", value = "answer",
                  code_id = 1L, code_type = "FULL_CREDIT", code_score = 1)
  completed <- suppressMessages(complete_design(
    coded, units, design, identifiers = "person",
    recode_omissions_to_not_reached = TRUE, preserve_not_reached = TRUE
  ))
  mapped <- add_item_id(completed, units)
  out <- summarise_missings(mapped, identifiers = "person")
  expect_identical(out$person, c("P1", "P2"))
  expect_identical(out$id_used, c(TRUE, FALSE))
  expect_identical(out$n_items, c(2L, 2L))
  expect_identical(out$n_not_reached, c(1L, 2L))
  expect_equal(out$prop_not_reached, c(0.5, 1))
  expect_identical(summarise_missings(mapped, identifiers = "person", used_only = TRUE), out[1, ])
})
