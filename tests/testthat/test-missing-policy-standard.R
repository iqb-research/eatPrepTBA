standard_fixture <- function(types, sources = list(), positions = seq_along(types),
                             unit_positions = rep(1L, length(types))) {
  ids <- names(types)
  if (is.null(ids)) ids <- paste0("V", seq_along(types))
  types <- unname(types)
  code_ids <- c(FULL_CREDIT = 1, NO_CREDIT = 0, MISSING_BY_OMISSION = -99,
                MISSING_NOT_REACHED = -96, MISSING_INVALID_RESPONSE = -98,
                MISSING_CODING_IMPOSSIBLE = -97, NO_CODING = -93, DERIVE_PENDING = -90)
  statuses <- c(FULL_CREDIT = "CODING_COMPLETE", NO_CREDIT = "CODING_COMPLETE",
                MISSING_BY_OMISSION = "DISPLAYED", MISSING_NOT_REACHED = "NOT_REACHED",
                MISSING_INVALID_RESPONSE = "INVALID", MISSING_CODING_IMPOSSIBLE = "CODING_ERROR",
                NO_CODING = "NO_CODING", DERIVE_PENDING = "DERIVE_PENDING")
  derived <- ids %in% names(sources)
  data <- tibble::tibble(
    login_code = "P", booklet_id = "B", booklet_no = 1L,
    testlet_no = 1L, unit_booklet_no = unit_positions, unit_key = "U",
    unit_alias = paste0("U", unit_positions), variable_id = ids,
    code_type = types, code_status = unname(statuses[types]),
    code_id = unname(code_ids[types]),
    code_score = ifelse(types %in% "FULL_CREDIT", 1,
                        ifelse(types %in% c("NO_CREDIT", "MISSING_BY_OMISSION", "MISSING_INVALID_RESPONSE"), 0, NA_real_)),
    value = ifelse(types %in% c("FULL_CREDIT", "NO_CREDIT", "MISSING_INVALID_RESPONSE"), "response", NA_character_),
    response_present = TRUE, analysis_included = TRUE, box_is_item = TRUE,
    position_group = as.numeric(positions)
  )
  # A fixture graph uses direct bases; individual tests introduce intermediate
  # or missing source metadata where that distinction matters.
  metadata <- tibble::tibble(
    unit_key = "U", variable_id = ids,
    variable_source_type = ifelse(derived, "SUM_CODE", "BASE"),
    variable_level = ifelse(derived, 1L, 0L),
    source_ids = lapply(ids, function(id) if (id %in% names(sources)) sources[[id]] else character()),
    basis_sources = lapply(ids, function(id) if (id %in% names(sources)) sources[[id]] else id),
    sources_known = TRUE
  )
  profile <- tibble::tribble(
    ~code_type, ~code_id, ~code_score,
    "MISSING_BY_OMISSION", -99, 0,
    "MISSING_NOT_REACHED", -96, NA_real_,
    "MISSING_INVALID_RESPONSE", -98, 0,
    "MISSING_CODING_IMPOSSIBLE", -97, NA_real_,
    "MISSING_BY_DESIGN", -94, NA_real_,
    "NO_CODING", -93, NA_real_,
    "DERIVE_PENDING", -90, NA_real_
  )
  list(data = data, metadata = metadata, profile = profile)
}

run_standard <- function(f, trailing = FALSE, scope = "testlet", correction = FALSE,
                         derived = "recode", input_profile = f$profile, precedence = NULL) {
  classify_eatpreptba(f$data, f$metadata, f$profile, input_profile = input_profile,
                      precedence = precedence, recode_omissions_to_not_reached = trailing,
                      not_reached_scope = scope, recode_existing_not_reached = correction,
                      derived_not_reached = derived, identifiers = "login_code")
}

test_that("standard protects numeric NR and optionally corrects it before later work", {
  f <- standard_fixture(c("MISSING_NOT_REACHED", "FULL_CREDIT"))
  expect_equal(run_standard(f)$data$code_id, c(-96, 1))
  expect_equal(run_standard(f, correction = TRUE)$data$code_id, c(-99, 1))
  f$data$code_id[[1]] <- NA_real_
  expect_equal(run_standard(f)$data$code_id, c(-99, 1))
})

test_that("standard trailing conversion works within all three scopes", {
  f <- standard_fixture(c("MISSING_BY_OMISSION", "FULL_CREDIT", "MISSING_BY_OMISSION"))
  for (scope in c("unit", "testlet", "booklet")) {
    expect_equal(run_standard(f, trailing = TRUE, scope = scope)$data$code_id, c(-99, 1, -96))
    expect_equal(run_standard(f, scope = scope)$data$code_id, c(-99, 1, -99))
  }
  f$data$unit_booklet_no <- c(1L, 2L, 3L)
  f$data$unit_alias <- c("first", "second", "third")
  f$data$testlet_no <- c(1L, 1L, 2L)
  expect_equal(run_standard(f, trailing = TRUE, scope = "unit")$data$code_id, c(-96, 1, -96))
  expect_equal(run_standard(f, trailing = TRUE, scope = "testlet")$data$code_id, c(-99, 1, -96))
  f$data$testlet_no <- c(1L, 2L, 3L)
  expect_equal(run_standard(f, trailing = TRUE, scope = "testlet")$data$code_id, c(-96, 1, -96))
  expect_equal(run_standard(f, trailing = TRUE, scope = "booklet")$data$code_id, c(-99, 1, -96))
})

test_that("standard sources override valid and invalid derived activity even with FALSE", {
  for (derived_type in c("FULL_CREDIT", "NO_CREDIT", "MISSING_INVALID_RESPONSE")) {
    for (trailing in c(FALSE, TRUE)) {
      f <- standard_fixture(c(B = "MISSING_NOT_REACHED", D = derived_type),
                            sources = list(D = "B"), positions = c(1, 1))
      out <- run_standard(f, trailing = trailing)$data
      expect_equal(out$code_id, c(-96, -96))
      expect_true(all(is.na(out$code_score)))
      expect_identical(out$code_status, f$data$code_status)
      expect_identical(out$value, f$data$value)
      protected <- run_standard(f, trailing = trailing, derived = "preserve")$data
      expect_equal(protected$code_id[[2]], f$data$code_id[[2]])
      expect_equal(protected$code_score[[2]], f$data$code_score[[2]])
    }
  }
})

test_that("standard retains FALSE Invalid when sources include an omission", {
  f <- standard_fixture(c(A = "MISSING_BY_OMISSION", B = "MISSING_NOT_REACHED",
                          D = "MISSING_INVALID_RESPONSE"),
                        sources = list(D = c("A", "B")), positions = c(1, 1, 1))
  expect_equal(run_standard(f)$data$code_id, c(-99, -96, -98))
  expect_equal(run_standard(f, trailing = TRUE)$data$code_id, rep(-96, 3))
  f$data$code_type[[2]] <- "FULL_CREDIT"
  f$data$code_id[[2]] <- 1
  f$data$code_score[[2]] <- 1
  expect_equal(run_standard(f, trailing = TRUE)$data$code_id, c(-99, 1, -98))
})

test_that("standard restores withheld derived evidence monotonically", {
  f <- standard_fixture(c(B1 = "MISSING_NOT_REACHED", R = "FULL_CREDIT",
                          B2 = "MISSING_NOT_REACHED", D = "FULL_CREDIT", E = "FULL_CREDIT"),
                        sources = list(D = "B1", E = "B2"), positions = c(1, 2, 3, 5, 6))
  # R first rescues D because B1 is before R. D then rescues E because B2 is
  # before D. Both derived results remain valid rather than depending on their
  # processing order or being removed simultaneously.
  expect_equal(run_standard(f)$data$code_id, c(-96, 1, -96, 1, 1))
  reversed <- f
  reversed$data <- f$data[c(5, 4, 3, 2, 1), ]
  reversed$metadata <- f$metadata[c(5, 4, 3, 2, 1), ]
  expect_equal(run_standard(reversed)$data$code_id, c(1, 1, -96, 1, -96))
  f$data$code_id[1:3] <- c(NA, NA, NA)
  f$data$code_score[1:3] <- c(NA, NA, NA)
  f$data$code_type[1:3] <- c("MISSING_NOT_REACHED", "FULL_CREDIT", "MISSING_NOT_REACHED")
  expect_equal(run_standard(f)$data$code_id[c(1, 3)], c(-99, -99))
})

test_that("standard unknown intra-unit variables still provide cross-unit activity", {
  f <- standard_fixture(c("MISSING_BY_OMISSION", "FULL_CREDIT", "MISSING_BY_OMISSION"),
                        positions = c(NA, NA, NA), unit_positions = c(1, 2, 2))
  f$data$analysis_included[[2]] <- FALSE
  f$data$box_is_item[[2]] <- FALSE
  out <- run_standard(f, trailing = TRUE)
  expect_equal(out$data$code_id, c(-99, 1, -99))
  expect_equal(out$diagnostics[[3]], "ambiguous-position")
  f$data$unit_booklet_no[[3]] <- 3
  f$data$unit_alias[[3]] <- "U3"
  expect_equal(run_standard(f, trailing = TRUE)$data$code_id, c(-99, 1, -96))
})

test_that("standard unworked unordered groups become NR but mixed groups retain O", {
  f <- standard_fixture(c("MISSING_BY_OMISSION", "MISSING_BY_OMISSION"), positions = c(1, 1))
  expect_equal(run_standard(f, trailing = TRUE)$data$code_id, c(-96, -96))
  f$data$code_type[[2]] <- "FULL_CREDIT"
  f$data$code_id[[2]] <- f$data$code_score[[2]] <- 1
  expect_equal(run_standard(f, trailing = TRUE)$data$code_id, c(-99, 1))
  # Technical O can be mapped to its baseline output schema even when a
  # stronger NR classification cannot be justified.
  f$data$code_type[[1]] <- NA_character_
  f$data$code_id[[1]] <- f$data$code_score[[1]] <- NA_real_
  expect_equal(run_standard(f, trailing = TRUE)$data$code_id[[1]], -99)
  f$data$code_status[[1]] <- NA_character_
  expect_true(is.na(run_standard(f, trailing = TRUE)$data$code_id[[1]]))
})

test_that("standard uses explicit partial precedence rather than display ranks", {
  f <- standard_fixture(c("FULL_CREDIT", "MISSING_BY_OMISSION"), positions = c(2, 1))
  relation <- matrix(c(FALSE, FALSE, TRUE, FALSE), 2, 2)
  entry <- list(keys = f$data[1, c("booklet_id", "testlet_no", "unit_booklet_no", "unit_key", "unit_alias")],
                variable_ids = f$data$variable_id, before = relation)
  expect_equal(run_standard(f, trailing = TRUE, precedence = list(entry))$data$code_id, c(1, -96))
  entry$before[,] <- FALSE
  expect_equal(run_standard(f, trailing = TRUE, precedence = list(entry))$data$code_id, c(1, -99))
})

test_that("standard coded errors and no-coding anchor only with raw response work", {
  for (type in c("MISSING_CODING_IMPOSSIBLE", "NO_CODING", "DERIVE_PENDING")) {
    f <- standard_fixture(c("MISSING_BY_OMISSION", type))
    expect_equal(run_standard(f, trailing = TRUE)$data$code_id[[1]], -96)
    f$data$value[[2]] <- "actual input"
    expect_equal(run_standard(f, trailing = TRUE)$data$code_id[[1]], -99)
    expect_equal(run_standard(f, trailing = TRUE)$data$code_type[[2]], type)
  }
})

test_that("standard keeps input decoding separate from the output profile", {
  f <- standard_fixture(c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED"))
  input <- f$profile[c("code_type", "code_id")]
  f$data$code_type <- f$data$code_status <- NA_character_
  f$profile$code_id[f$profile$code_type == "MISSING_BY_OMISSION"] <- -199
  expect_equal(run_standard(f, input_profile = input)$data$code_id, c(-199, -96))
  f <- standard_fixture(c("MISSING_NOT_REACHED", "MISSING_INVALID_RESPONSE"))
  input <- f$profile[c("code_type", "code_id")]
  f$data$code_type <- f$data$code_status <- NA_character_
  f$profile$code_id[f$profile$code_type == "MISSING_INVALID_RESPONSE"] <- -198
  expect_equal(run_standard(f, correction = TRUE, input_profile = input)$data$code_id, c(-99, -198))
})

test_that("standard leaves ambiguous and unknown negative input codes untouched", {
  f <- standard_fixture(c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED"))
  f$data$code_type[[2]] <- NA_character_
  f$data$code_id[[2]] <- -700
  f$data$code_score[[2]] <- 0
  out <- run_standard(f, trailing = TRUE)
  expect_equal(out$data$code_id, c(-96, -700))
  expect_equal(out$diagnostics[[2]], "unrecognized-negative-code")
  f$data$value[[2]] <- "actual response"
  expect_equal(run_standard(f, trailing = TRUE)$data$code_id[[1]], -99)
  decoder <- dplyr::bind_rows(f$profile[c("code_type", "code_id")],
                             tibble::tibble(code_type = "MISSING_INVALID_RESPONSE", code_id = -700),
                             tibble::tibble(code_type = "MISSING_BY_OMISSION", code_id = -700))
  expect_equal(run_standard(f, input_profile = decoder)$diagnostics[[2]], "ambiguous-input-code")
})

test_that("standard derives missing results from sources without inventing valid codes", {
  f <- standard_fixture(c(A = "MISSING_NOT_REACHED", B = "MISSING_BY_OMISSION", D = "DERIVE_PENDING"),
                        sources = list(D = c("A", "B")), positions = c(1, 1, 1))
  f$data$code_id[[3]] <- f$data$code_score[[3]] <- NA_real_
  expect_equal(run_standard(f)$data$code_id[[3]], -96)
  for (missing in c("MISSING_NOT_REACHED", "MISSING_BY_OMISSION", "MISSING_INVALID_RESPONSE")) {
    g <- standard_fixture(c(A = "FULL_CREDIT", B = missing, D = "DERIVE_PENDING"),
                          sources = list(D = c("A", "B")))
    g$data$code_id[[3]] <- g$data$code_score[[3]] <- NA_real_
    expect_equal(run_standard(g)$data$code_id[[3]], -98)
  }
  f <- standard_fixture(c(A = "FULL_CREDIT", B = "FULL_CREDIT", D = "DERIVE_PENDING"),
                        sources = list(D = c("A", "B")))
  f$data$code_id[[3]] <- f$data$code_score[[3]] <- NA_real_
  out <- run_standard(f)
  expect_equal(out$diagnostics[[3]], "derived-result-missing")
  expect_true(is.na(out$data$code_id[[3]]))
  expect_true(is.na(out$data$code_type[[3]]))
})

test_that("standard protects coded derived Invalid fields unless NR is proved", {
  f <- standard_fixture(c(A = "MISSING_BY_OMISSION", D = "MISSING_INVALID_RESPONSE"),
                        sources = list(D = "A"))
  f$data$code_score[[2]] <- 0.75
  f$profile$code_id[f$profile$code_type == "MISSING_INVALID_RESPONSE"] <- -198
  expect_equal(run_standard(f)$data$code_id[[2]], -98)
  expect_equal(run_standard(f)$data$code_score[[2]], 0.75)
  expect_equal(run_standard(f, trailing = TRUE)$data$code_id[[2]], -96)
  f$metadata$sources_known[[2]] <- FALSE
  expect_equal(run_standard(f, trailing = TRUE)$data$code_id[[2]], -98)
  expect_equal(run_standard(f, trailing = TRUE)$diagnostics[[2]], "incomplete-basis-sources")
})

test_that("standard classifies inserted basis sources without altering provenance", {
  f <- standard_fixture(c(A = NA_character_, D = "NO_CREDIT"), sources = list(D = "A"))
  f$data$response_present[[1]] <- FALSE
  f$data$code_id[[1]] <- f$data$code_score[[1]] <- NA_real_
  out <- run_standard(f)$data
  expect_equal(out$code_id, c(-96, -96))
  expect_identical(out$response_present, f$data$response_present)
  expect_identical(out$code_status, f$data$code_status)
  expect_equal(nrow(out), nrow(f$data))
})

test_that("standard keeps explicit derived coding failures without numeric results", {
  for (type in c("MISSING_CODING_IMPOSSIBLE", "NO_CODING", "CODING_INCOMPLETE",
                 "INTENDED_INCOMPLETE", "CODE_SELECTION_PENDING")) {
    f <- standard_fixture(c(A = "MISSING_NOT_REACHED", D = type), sources = list(D = "A"))
    f$data$code_id[[2]] <- f$data$code_score[[2]] <- NA_real_
    out <- run_standard(f, trailing = TRUE)$data
    expect_identical(out$code_type[[2]], type)
    expect_true(is.na(out$code_id[[2]]))
    expect_true(is.na(out$code_score[[2]]))
  }
})

test_that("standard derived omissions require missing sources rather than display tail", {
  f <- standard_fixture(c(A = "FULL_CREDIT", D = "MISSING_BY_OMISSION"),
                        sources = list(D = "A"), positions = c(1, 2))
  expect_equal(run_standard(f, trailing = TRUE)$data$code_id, c(1, -99))
  f$data$code_type[[1]] <- "MISSING_BY_OMISSION"
  f$data$code_id[[1]] <- -99
  f$data$code_score[[1]] <- 0
  f$data$value[[1]] <- NA_character_
  expect_equal(run_standard(f, trailing = TRUE)$data$code_id, c(-96, -96))
})

test_that("standard absent Invalid results follow source priority and remain stable", {
  for (trailing in c(FALSE, TRUE)) {
    for (policy in c("recode", "preserve")) {
      f <- standard_fixture(c(A = "MISSING_NOT_REACHED", D = "MISSING_INVALID_RESPONSE"),
                            sources = list(D = "A"))
      f$data$code_id <- f$data$code_score <- rep(NA_real_, 2)
      f$data$value <- rep(NA_character_, 2)
      first <- run_standard(f, trailing = trailing, derived = policy)$data
      expect_equal(first$code_id, c(-96, -96))
      f$data <- first
      second <- run_standard(f, trailing = trailing, derived = policy)$data
      expect_identical(second, first)
    }
  }
})

test_that("standard booklet order handles unit positions restarting per testlet", {
  f <- standard_fixture(c("MISSING_BY_OMISSION", "FULL_CREDIT"),
                        unit_positions = c(8L, 1L))
  f$data$testlet_no <- c(1L, 2L)
  expect_equal(run_standard(f, trailing = TRUE, scope = "booklet")$data$code_id, c(-99, 1))
  f$data$code_type <- c("FULL_CREDIT", "MISSING_BY_OMISSION")
  f$data$code_id <- c(1, -99)
  f$data$code_score <- c(1, 0)
  expect_equal(run_standard(f, trailing = TRUE, scope = "booklet")$data$code_id, c(1, -96))
})

test_that("standard remaps existing derived O and NR but protects coded Invalid", {
  f <- standard_fixture(c(A = "MISSING_NOT_REACHED", O = "MISSING_BY_OMISSION",
                          N = "MISSING_NOT_REACHED", I = "MISSING_INVALID_RESPONSE"),
                        sources = list(O = "A", N = "A", I = "A"))
  f$profile$code_id <- f$profile$code_id - 100
  original_input <- standard_fixture("MISSING_NOT_REACHED")$profile
  out <- run_standard(f, derived = "preserve", input_profile = original_input)$data
  expect_equal(out$code_id, c(-196, -199, -196, -98))
})

test_that("standard unknown numerical negatives override technical fallback guesses", {
  for (status in c("NOT_REACHED", "DISPLAYED", "INVALID", "CODING_ERROR", "NO_CODING")) {
    f <- standard_fixture(c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED"))
    f$data$code_type[[2]] <- NA_character_
    f$data$code_id[[2]] <- -700
    f$data$code_status[[2]] <- status
    f$data$code_score[[2]] <- 0
    expect_equal(run_standard(f, trailing = TRUE)$data$code_id, c(-96, -700))
    expect_true(is.na(run_standard(f)$data$code_type[[2]]))
  }
})

test_that("standard source recursion does not bypass protected intermediate failures", {
  for (type in c("NO_CODING", "CODING_INCOMPLETE", "MISSING_CODING_IMPOSSIBLE")) {
    f <- standard_fixture(c(B = "MISSING_NOT_REACHED", H = type, D = "DERIVE_PENDING"),
                          sources = list(H = "B", D = "H"))
    f$data$code_id[2:3] <- f$data$code_score[2:3] <- NA_real_
    f$metadata$basis_sources[[3]] <- "B"
    out <- run_standard(f)
    expect_identical(out$data$code_type[[2]], type)
    if (type == "MISSING_CODING_IMPOSSIBLE") {
      expect_equal(out$data$code_id[[3]], -97)
    } else {
      expect_true(is.na(out$data$code_id[[3]]))
      expect_equal(out$diagnostics[[3]], "derived-source-unresolved")
    }
  }
})

test_that("standard numeric derived NR correction requires complete source evidence", {
  f <- standard_fixture(c(B = "MISSING_NOT_REACHED", D = "MISSING_NOT_REACHED", R = "FULL_CREDIT"),
                        sources = list(D = "B"))
  f$metadata$sources_known[[2]] <- FALSE
  expect_equal(run_standard(f, correction = TRUE)$data$code_id, c(-99, -96, 1))
  f$metadata$sources_known[[2]] <- TRUE
  expect_equal(run_standard(f, correction = TRUE)$data$code_id, c(-99, -99, 1))
})
