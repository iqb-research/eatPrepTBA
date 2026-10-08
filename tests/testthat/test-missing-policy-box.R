# Reference cases for Coding Box 39468e5a28a24dc9ec860aee46daf8d4ed5c8682.
# These exercise its reverse positional scan, stored-result recognition, and
# pairwise missing aggregation without requiring Node or a matrix export.
box_fixture <- function(status, code = rep(NA_real_, length(status)),
                        score = rep(NA_real_, length(status)),
                        sources = list(), source_type = "SUM_CODE",
                        positions = seq_along(status)) {
  ids <- names(status)
  if (is.null(ids)) ids <- paste0("V", seq_along(status))
  status <- unname(status)
  n <- length(status)
  derived <- ids %in% names(sources)
  data <- tibble::tibble(
    login_code = "P", booklet_id = "B", booklet_no = 1L,
    testlet_no = 1L, unit_booklet_no = 1L, unit_key = "U", unit_alias = "U",
    variable_id = ids, code_status = status, code_id = as.numeric(code),
    code_score = as.numeric(score), code_type = NA_character_,
    value = rep(NA_character_, n), response_present = TRUE,
    box_position = as.numeric(positions), box_included = TRUE,
    box_is_item = TRUE
  )
  metadata <- tibble::tibble(
    unit_key = "U", variable_id = ids,
    variable_source_type = ifelse(derived, source_type, "BASE"),
    variable_level = ifelse(derived, 1L, 0L),
    source_ids = lapply(ids, function(id) if (id %in% names(sources)) sources[[id]] else character()),
    basis_sources = lapply(ids, function(id) if (id %in% names(sources)) sources[[id]] else id),
    sources_known = TRUE
  )
  profile <- tibble::tribble(
    ~code_type, ~code_id, ~code_score,
    "MISSING_INVALID_RESPONSE", -98, 0,
    "MISSING_CODING_IMPOSSIBLE", -97, NA_real_,
    "MISSING_BY_OMISSION", -99, 0,
    "MISSING_NOT_REACHED", -96, NA_real_,
    "MISSING_BY_DESIGN", -94, NA_real_
  )
  list(data = data, metadata = metadata, profile = profile)
}

run_box <- function(f, trailing = FALSE, scope = "unit") {
  classify_coding_box(f$data, f$metadata, f$profile,
                     recode_omissions_to_not_reached = trailing,
                     not_reached_scope = scope, identifiers = "login_code")
}

test_that("box corrects bare NR but preserves numeric NR before later valid work", {
  f <- box_fixture(c("NOT_REACHED", "NOT_REACHED", "CODING_COMPLETE"),
                   code = c(NA, -96, 0), score = c(NA, NA, 0))
  f$data$code_type <- c("FULL_CREDIT", "MISSING_NOT_REACHED", "NO_CREDIT")
  out <- run_box(f)$data
  expect_equal(out$code_id, c(-99, -96, 0))
  expect_equal(out$code_score, c(0, NA, 0))
  expect_equal(out$code_type, c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED", "NO_CREDIT"))
  expect_identical(out$code_status, f$data$code_status)
  expect_identical(out$value, f$data$value)
})

test_that("box scans strictly later groups before same-position activity", {
  f <- box_fixture(c("NOT_REACHED", "DISPLAYED", "CODING_COMPLETE"),
                   code = c(NA, NA, 1), score = c(NA, NA, 1),
                   positions = c(1, 2, 2))
  expect_equal(run_box(f, trailing = TRUE, scope = "testlet")$data$code_id,
               c(-99, -96, 1))
  f$data$box_position <- c(2, 2, 2)
  expect_equal(run_box(f, trailing = TRUE, scope = "testlet")$data$code_id,
               c(-96, -96, 1))
  expect_error(run_box(f, trailing = TRUE), "only with testlet or booklet")
})

test_that("box applies unit, testlet, and booklet boundaries separately", {
  f <- box_fixture(c("NOT_REACHED", "CODING_COMPLETE", "NOT_REACHED", "CODING_COMPLETE"),
                   code = c(NA, 1, NA, 1), score = c(NA, 1, NA, 1))
  f$data$unit_booklet_no <- 1:4
  f$data$unit_alias <- paste0("Occurrence", 1:4)
  f$data$testlet_no <- c(1L, 1L, 2L, 3L)
  expect_equal(run_box(f, scope = "unit")$data$code_id, c(-96, 1, -96, 1))
  expect_equal(run_box(f, scope = "testlet")$data$code_id, c(-99, 1, -96, 1))
  expect_equal(run_box(f, scope = "booklet")$data$code_id, c(-99, 1, -99, 1))
  repeated <- dplyr::bind_rows(f$data, dplyr::mutate(f$data, login_code = "P2", code_id = NA_real_,
                                                  code_score = NA_real_, code_status = "NOT_REACHED"))
  f$data <- repeated
  expect_equal(run_box(f, scope = "booklet")$data$code_id[5:8], rep(-96, 4))
})

test_that("box uses omissions as anchors only when trailing conversion is off", {
  f <- box_fixture(c("NOT_REACHED", "DISPLAYED", "NOT_REACHED"))
  expect_equal(run_box(f)$data$code_id, c(-99, -99, -96))
  expect_equal(run_box(f, trailing = TRUE, scope = "booklet")$data$code_id,
               rep(-96, 3))
  f$data$response_present[[2]] <- FALSE
  f$data$code_status[[2]] <- NA_character_
  expect_equal(run_box(f)$data$code_id, rep(-96, 3))
})

test_that("box errors and pending states anchor even without a response value", {
  for (status in c("CODING_ERROR", "NO_CODING", "DERIVE_PENDING", "VALUE_CHANGED", "INVALID")) {
    f <- box_fixture(c("NOT_REACHED", status))
    out <- run_box(f)
    expect_equal(out$data$code_id[[1]], -99)
    if (status %in% c("NO_CODING", "DERIVE_PENDING", "VALUE_CHANGED")) {
      expect_equal(out$diagnostics[[2]], "unresolved-status")
      expect_true(is.na(out$data$code_id[[2]]))
    }
  }
})

test_that("box requires selected missing ID and matching score together", {
  f <- box_fixture(c("NOT_REACHED", "DISPLAYED", "NOT_REACHED"),
                   code = c(NA, -99, -96), score = c(NA, NA, NA))
  f$data$code_type <- c(NA, "MISSING_BY_OMISSION", "MISSING_NOT_REACHED")
  # The -99/NA pair is not an omission under the selected profile and remains
  # numeric evidence. It must neither be silently repaired nor made NR.
  out <- run_box(f, trailing = TRUE, scope = "booklet")
  expect_equal(out$data$code_id, c(-99, -99, -96))
  expect_true(is.na(out$data$code_score[[2]]))
  expect_equal(out$diagnostics[[2]], "unrecognized-negative-code")
  f$profile$code_id[f$profile$code_type == "MISSING_BY_OMISSION"] <- -199
  f$data$code_score[[2]] <- 0
  expect_equal(run_box(f, trailing = TRUE, scope = "booklet")$data$code_id,
               c(-199, -99, -96))
  f$data$code_id[[2]] <- -199
  expect_equal(run_box(f, trailing = TRUE, scope = "booklet")$data$code_id,
               rep(-96, 3))
})

test_that("box maps internal manual codes to the selected profile", {
  f <- box_fixture(c("CODING_COMPLETE", "CODING_COMPLETE"),
                   code = c(-3, -4), score = c(9, 8))
  f$profile$code_id[1:2] <- c(-198, -197)
  f$profile$code_score[1:2] <- c(0, NA)
  out <- run_box(f)$data
  expect_equal(out$code_id, c(-198, -197))
  expect_equal(out$code_score, c(0, NA))
  expect_equal(out$code_status, f$data$code_status)
})

test_that("box protects coded derived valid and invalid results, including zero", {
  for (code in c(0, 1, -98)) {
    f <- box_fixture(c(B = "NOT_REACHED", D = "INVALID"),
                     code = c(-96, code), score = c(NA, 0), sources = list(D = "B"),
                     positions = c(1, 1))
    expect_equal(run_box(f)$data$code_id, c(-96, code))
    expect_equal(run_box(f)$data$code_score, c(NA, 0))
  }
  f <- box_fixture(c(B = "NOT_REACHED", D = "CODING_COMPLETE"),
                   code = c(NA, 0), score = c(NA, 0), sources = list(D = "B"))
  expect_equal(run_box(f)$data$code_id, c(-99, 0))
})

test_that("box aggregates missing derived results after positional classification", {
  cases <- list(
    list(code = c(-96, -96), score = c(NA, NA), target = -96),
    list(code = c(-99, -96), score = c(0, NA), target = -96),
    list(code = c(-99, -99), score = c(0, 0), target = -99),
    list(code = c(-98, -96), score = c(0, NA), target = -98),
    list(code = c(-97, 1), score = c(NA, 1), target = -97),
    list(code = c(-94, -94), score = c(NA, NA), target = -94)
  )
  for (case in cases) {
    f <- box_fixture(c(A = "NOT_REACHED", B = "NOT_REACHED", D = "DERIVE_PENDING"),
                     code = c(case$code, NA), score = c(case$score, NA),
                     sources = list(D = c("A", "B")), positions = c(1, 1, 1))
    expect_equal(run_box(f)$data$code_id[[3]], case$target)
    expect_identical(run_box(f)$data$code_status[[3]], "DERIVE_PENDING")
  }
})

test_that("box pinned partial SUM and CONCAT rules classify mixed sources invalid", {
  for (source_type in c("SUM_CODE", "SUM_SCORE", "CONCAT_CODE")) {
    for (missing in c(-99, -96, -98)) {
      f <- box_fixture(c(A = "CODING_COMPLETE", B = "NOT_REACHED", D = "DERIVE_PENDING"),
                       code = c(1, missing, NA),
                       score = c(1, if (missing == -96) NA else 0, NA),
                       sources = list(D = c("A", "B")), source_type = source_type,
                       positions = c(1, 1, 1))
      expect_equal(run_box(f)$data$code_id[[3]], -98)
    }
  }
  f <- box_fixture(c(A = "CODING_COMPLETE", B = "CODING_COMPLETE", D = "DERIVE_PENDING"),
                   code = c(1, 1, NA), score = c(1, 1, NA),
                   sources = list(D = c("A", "B")))
  out <- run_box(f)
  expect_equal(out$diagnostics[[3]], "derived-result-missing")
  expect_true(is.na(out$data$code_id[[3]]))
  f$metadata$variable_source_type[[3]] <- "COPY_VALUE"
  f$data$code_id[[2]] <- -99
  f$data$code_score[[2]] <- 0
  expect_equal(run_box(f)$diagnostics[[3]], "derived-result-missing")
})

test_that("box recursively aggregates hidden intermediate sources and diagnoses cycles", {
  f <- box_fixture(c(A = "NOT_REACHED", H = "DERIVE_PENDING", D = "DERIVE_PENDING"),
                   code = c(-96, NA, NA), score = c(NA, NA, NA),
                   sources = list(H = "A", D = "H"), positions = c(1, 1, 1))
  f$data$box_is_item <- c(FALSE, FALSE, TRUE)
  out <- run_box(f)
  expect_equal(out$data$code_id[[3]], -96)
  expect_true(is.na(out$data$code_id[[2]]))
  f$metadata$source_ids[[2]] <- "D"
  expect_equal(run_box(f)$diagnostics[[3]], "derived-cycle")
  f$metadata$source_ids[[3]] <- "unknown"
  expect_equal(run_box(f)$diagnostics[[3]], "derived-source-unresolved")
})

test_that("box retains excluded variables and reports incompatible source designs", {
  f <- box_fixture(c(A = "NOT_REACHED", B = "NOT_REACHED", D = "DERIVE_PENDING"),
                   code = c(-94, -96, NA), sources = list(D = c("A", "B")))
  expect_equal(run_box(f)$diagnostics[[3]], "derived-design-conflict")
  f <- box_fixture(c("DISPLAYED", "NOT_REACHED"))
  f$data$box_included[[1]] <- FALSE
  expect_equal(run_box(f, trailing = TRUE, scope = "testlet")$data[1, ], f$data[1, ])
  expect_equal(run_box(f)$diagnostics[[1]], "excluded-variable")
  expect_equal(nrow(run_box(f)$data), nrow(f$data))
})

test_that("box never substitutes zeros for unavailable valid codes or scores", {
  f <- box_fixture(c("CODING_COMPLETE", "CODING_COMPLETE", "CODING_COMPLETE"),
                   code = c(NA, 1, -1), score = c(0, NA, NA))
  out <- run_box(f)
  expect_equal(out$diagnostics, c("missing-code", "missing-score", "invalid-code"))
  expect_identical(out$data$code_id, f$data$code_id)
  expect_identical(out$data$code_score, f$data$code_score)
})

test_that("box validates the five semantic profile entries and reserved codes", {
  f <- box_fixture("NOT_REACHED")
  incomplete <- f
  incomplete$profile <- f$profile[-5, ]
  expect_error(run_box(incomplete), "five|requires invalid")
  f$profile$code_id[[1]] <- -3
  expect_error(run_box(f), "reserved technical codes")
  f$profile$code_id[[1]] <- -96
  expect_error(run_box(f), "distinct negative IDs")
})

test_that("box global booklet positions distinguish restarted testlet unit numbers", {
  f <- box_fixture(c("NOT_REACHED", "CODING_COMPLETE"),
                   code = c(NA, 1), score = c(NA, 1), positions = c(1, 1))
  f$data$testlet_no <- c(1L, 2L)
  f$data$unit_booklet_no <- c(8L, 1L)
  f$data$unit_alias <- c("first", "second")
  expect_equal(run_box(f, scope = "booklet")$data$code_id, c(-99, 1))
  # Equal local unit/item numbers in different testlets are not a single
  # position group. The second unit still supplies strictly later activity.
  f$data$unit_booklet_no <- c(1L, 1L)
  expect_equal(run_box(f, scope = "booklet")$data$code_id, c(-99, 1))
  f$data$code_status <- c("CODING_COMPLETE", "DISPLAYED")
  f$data$code_id <- c(1, NA)
  f$data$code_score <- c(1, NA)
  expect_equal(run_box(f, trailing = TRUE, scope = "booklet")$data$code_id, c(1, -96))
})
