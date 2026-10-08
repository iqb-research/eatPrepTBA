bulk_metadata_fixture <- function() {
  tibble::tibble(unit_key = c("U", "U", "U", "U", "U", "U"),
    variable_id = c("B", "A", "D", "A", "D", "E"),
    variable_ref = c("b", "a", "d", "a", "d", "e"),
    variable_source_type = c("BASE", "BASE", "SUM_CODE", "BASE", "SUM_CODE", "SOLVER"),
    variable_level = c(0L, 0L, 1L, 0L, 1L, 2L),
    variable_page = c(2, 1, NA, NA, NA, NA),
    variable_section = list(0L, c(1L, 2L), NA_integer_, integer(), NA_integer_, NA_integer_),
    variable_element = list(1L, 1L, NA_integer_, 2L, NA_integer_, NA_integer_),
    variable_page_always_visible = c(FALSE, FALSE, NA, FALSE, NA, NA),
    variable_sources = list(NULL, tibble::tibble(),
      tibble::tibble(variable_source_ref = c("a", "b"), variable_source_direct = c(TRUE, FALSE)),
      NULL, tibble::tibble(variable_source_ref = "b", variable_source_direct = TRUE),
      tibble::tibble(variable_source_id = "D")))
}

test_that("bulk metadata preserves duplicate paths and direct versus legacy sources", {
  raw <- bulk_metadata_fixture()
  out <- design_order_metadata_merge(raw)
  expect_identical(out$variable_id, c("A", "B", "D", "E"))
  expect_identical(out$variable_level, c(0L, 0L, 1L, 2L))
  expect_identical(out$variable_page, c(1, 2, NA_real_, NA_real_))
  expect_identical(out$variable_section, list(c(1L, 2L), 0L, NA_integer_, NA_integer_))
  expect_identical(out$variable_element, list(NA_integer_, 1L, NA_integer_, NA_integer_))
  expect_identical(out$.source_refs, list(character(), character(), c("a", "b"), ".alias:D"))
  expect_identical(design_order_metadata_merge(raw[c(6L, 4L, 3L, 2L, 1L, 5L), ]), out)

  units <- tibble::tibble(unit_key = "U", unit_codes = list(raw[setdiff(names(raw), "unit_key")]))
  metadata <- design_order_metadata(units)
  expect_identical(metadata$source_ids, list(character(), character(), c("A", "B"), "D"))
  expect_identical(metadata$basis_sources, list("A", "B", c("A", "B"), c("A", "B")))
  expect_true(all(metadata$sources_known))
})

test_that("bulk metadata retains original-reference and ambiguous-source safeguards", {
  raw <- bulk_metadata_fixture()
  raw$derive_sources <- list(character(), character(), "a", character(), character(), "d")
  expect_identical(design_order_metadata_merge(raw)$.source_refs,
                   list(character(), character(), "a", "d"))
  # An empty reference column still suppresses legacy alias fallback when
  # another coding row has alias-only source metadata.
  raw$derive_sources <- NULL
  raw$variable_sources[[3]] <- tibble::tibble(variable_source_ref = character())
  raw$variable_sources[[5]] <- tibble::tibble(variable_source_id = "B")
  expect_identical(design_order_metadata_merge(raw)$.source_refs[[3]], character())
  raw <- bulk_metadata_fixture()
  raw$variable_ref[[4]] <- "conflicting"
  expect_error(design_order_metadata_merge(raw), "variable_ref.*U / A")
  raw <- bulk_metadata_fixture()
  raw$variable_source_type[[4]] <- "SOLVER"
  expect_error(design_order_metadata_merge(raw), "variable_source_type.*U / A")
  raw <- bulk_metadata_fixture()
  raw$variable_level[[4]] <- 1L
  expect_error(design_order_metadata_merge(raw), "variable_level.*U / A")
})

test_that("static classifier cache handles reordered person rows and partial precedence", {
  data <- tibble::tibble(login_code = rep(c("P1", "P2", "P3"), each = 3L),
    booklet_id = "b", testlet_no = 1L, unit_booklet_no = 1L, unit_key = "U", unit_alias = "U",
    variable_id = rep(c("A", "B", "D"), 3L), value = NA_character_,
    code_status = rep(c("DISPLAYED", "NOT_REACHED", "CODING_COMPLETE"), 3L),
    code_type = rep(c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED", "NO_CREDIT"), 3L),
    code_id = rep(c(-99, -96, 0), 3L), code_score = rep(c(0, NA_real_, 0), 3L))
  metadata <- tibble::tibble(unit_key = "U", variable_id = c("A", "B", "D"),
    variable_source_type = c("BASE", "BASE", "SUM_CODE"),
    source_ids = list(character(), character(), c("A", "B")),
    basis_sources = list("A", "B", c("A", "B")), sources_known = TRUE)
  profile <- tibble::tribble(~code_type, ~code_id, ~code_score,
    "MISSING_BY_OMISSION", -99, 0, "MISSING_NOT_REACHED", -96, NA_real_)
  entry <- list(keys = data[1L, c("booklet_id", "testlet_no", "unit_booklet_no", "unit_key", "unit_alias")],
    variable_ids = c("A", "B", "D"), before = upper.tri(matrix(FALSE, 3L, 3L)))
  run <- function(rows, entries) classify_eatpreptba(data[rows, ], metadata, profile,
    precedence = entries, recode_omissions_to_not_reached = TRUE, identifiers = "login_code")
  ordered <- run(seq_len(nrow(data)), list(entry))
  permutation <- c(1L, 2L, 3L, 6L, 4L, 5L, 8L, 9L, 7L)
  reordered <- run(permutation, list(entry))
  expect_identical(reordered$data, ordered$data[permutation, ])
  expect_identical(reordered$diagnostics, ordered$diagnostics[permutation])
  expect_identical(ordered$data$code_id, rep(-96, nrow(data)))
  # Different key schemas use the custom-key fallback with first-match rules.
  entry$keys <- entry$keys["unit_key"]
  expect_identical(run(seq_len(nrow(data)), list(entry)), ordered)
})
