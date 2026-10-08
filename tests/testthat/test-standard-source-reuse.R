source_reuse_fixture <- function() {
  ids <- c("A", "B", "D", "E", "F")
  data <- tibble::tibble(
    login_code = "P1", booklet_id = "B", testlet_no = 1L,
    unit_booklet_no = 1L, unit_key = "U", unit_alias = "U",
    variable_id = ids, value = NA_character_,
    code_status = c("NOT_REACHED", "NOT_REACHED", rep("DERIVE_PENDING", 3)),
    code_type = c(rep("MISSING_NOT_REACHED", 2), rep("DERIVE_PENDING", 3)),
    code_id = c(-96, -96, rep(NA_real_, 3)), code_score = NA_real_,
    analysis_included = c(TRUE, TRUE, FALSE, FALSE, TRUE),
    box_is_item = c(FALSE, FALSE, FALSE, FALSE, TRUE),
    position_group = c(1, 2, 2, 2, 2)
  )
  metadata <- tibble::tibble(
    unit_key = "U", variable_id = ids,
    variable_source_type = c("BASE", "BASE", rep("SUM_CODE", 3)),
    source_ids = list(character(), character(), c("A", "B"), c("A", "D"), c("D", "E")),
    basis_sources = list("A", "B", c("A", "B"), c("A", "B"), c("A", "B")),
    sources_known = TRUE
  )
  list(data = data, metadata = metadata, profile = missing_default_profile())
}

test_that("shared dependency graphs preserve separate person outcomes and input order", {
  f <- source_reuse_fixture()
  second <- f$data
  second$login_code <- "P2"
  second$code_type[[1]] <- "FULL_CREDIT"
  second$code_status[[1]] <- "CODING_COMPLETE"
  second$code_id[[1]] <- second$code_score[[1]] <- 1
  second$value[[1]] <- "response"
  third <- f$data[c(5, 2, 4, 1, 3), ]
  third$login_code <- "P3"
  f$data <- dplyr::bind_rows(f$data, second, third)
  out <- do.call(classify_eatpreptba, f)
  expect_identical(out$data$login_code, f$data$login_code)
  expect_identical(out$data$variable_id, f$data$variable_id)
  expect_equal(out$data$code_id, c(rep(-96, 5), 1, -96, -98, -98, -98, rep(-96, 5)))
  expect_true(all(is.na(out$diagnostics)))
  expect_identical(out$data$code_status, f$data$code_status)
  expect_identical(out$data$value, f$data$value)
})

test_that("shared cyclic and absent dependencies retain their diagnostic meaning", {
  f <- source_reuse_fixture()
  f$metadata$source_ids[3:4] <- list("E", "D")
  f$metadata$sources_known[3:5] <- FALSE
  out <- do.call(classify_eatpreptba, f)
  expect_true(all(is.na(out$data$code_id[3:5])))
  expect_equal(out$diagnostics[3:5], rep("derived-cycle", 3))
  f$metadata$source_ids[[3]] <- "absent"
  out <- do.call(classify_eatpreptba, f)
  expect_true(all(is.na(out$data$code_id[3:5])))
  expect_equal(out$diagnostics[3:5], rep("derived-source-unresolved", 3))
})

test_that("empty list responses and scalar NA responses give the same classification", {
  f <- source_reuse_fixture()
  scalar <- do.call(classify_eatpreptba, f)
  f$data$value <- list(NULL, character(), c(NA_character_, NA_character_), NA, NA_character_)
  listed <- do.call(classify_eatpreptba, f)
  expect_identical(listed$data[c("code_id", "code_score", "code_type")],
                   scalar$data[c("code_id", "code_score", "code_type")])
  expect_identical(listed$diagnostics, scalar$diagnostics)
  expect_identical(listed$data$value, f$data$value)
})
