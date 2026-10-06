recode_fixture <- function(types, sources = list(), status = NULL, value = NULL) {
  ids <- names(types)
  if (is.null(ids)) ids <- sprintf("V%02d", seq_along(types))
  n <- length(types)
  defaults <- c(MISSING_NOT_REACHED = -96, MISSING_BY_OMISSION = -99,
                MISSING_INVALID_RESPONSE = -98, MISSING_CODING_IMPOSSIBLE = -97,
                NO_CODING = -93, CODING_INCOMPLETE = -90)
  code_id <- unname(defaults[types])
  valid <- !is.na(types) & !types %in% names(defaults)
  code_id[valid] <- 1
  score <- rep(NA_real_, n)
  score[types %in% c("MISSING_BY_OMISSION", "MISSING_INVALID_RESPONSE")] <- 0
  score[valid] <- 1
  if (is.null(value)) {
    value <- rep(NA_character_, n)
    value[valid | types %in% "MISSING_INVALID_RESPONSE"] <- "response"
  }
  if (is.null(status)) status <- rep(NA_character_, n)
  data <- tibble::tibble(
    group_id = "G1", login_name = "L1", login_code = "C1", booklet_id = "B1",
    booklet_no = 1L, testlet_no = 1L, unit_booklet_no = 1L, unit_key = "U1",
    unit_alias = "U1", variable_id = ids, variable_order = seq_len(n),
    order_source = "override",
    value = value, code_status = status, code_id = code_id,
    code_type = unname(types), code_score = score, response_present = TRUE
  )
  basis <- !ids %in% names(sources)
  units <- tibble::tibble(
    unit_key = "U1", variable_id = ids, variable_ref = ids,
    variable_source_type = ifelse(basis, "BASE", "SUM_CODE"),
    variable_level = ifelse(basis, 0L, 1L),
    basis_sources = lapply(ids, function(id) {
      if (id %in% names(sources)) sources[[id]] else id
    }),
    sources_known = basis | vapply(ids, function(id) {
      id %in% names(sources) && length(sources[[id]]) > 0L && !anyNA(sources[[id]])
    }, logical(1))
  )
  list(data = data, units = units)
}

local_recode_metadata <- function() {
  testthat::local_mocked_bindings(
    design_order_metadata = function(units, ...) units,
    .package = "eatPrepTBA", .env = parent.frame()
  )
}

test_that("negative missing IDs with zero scores do not become valid work evidence", {
  local_recode_metadata()
  f <- recode_fixture(c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED"))
  f$data$code_type <- NA_character_
  f$data$code_score <- 0
  unchanged_omissions <- recode_missings(f$data, f$units)
  expect_equal(unchanged_omissions$code_type,
               c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED"))
  expect_equal(unchanged_omissions$code_score, c(0, NA_real_))
  tail <- recode_missings(f$data, f$units, recode_omissions_to_not_reached = TRUE)
  expect_equal(tail$code_type, rep("MISSING_NOT_REACHED", 2))
  expect_true(all(is.na(tail$code_score)))
  expect_true(all(is.na(tail$code_status)))
})

test_that("both settings correct not reached before later work within a unit", {
  local_recode_metadata()
  f <- recode_fixture(c("FULL_CREDIT", "MISSING_NOT_REACHED", "FULL_CREDIT",
                        "MISSING_BY_OMISSION", "MISSING_NOT_REACHED"),
                      status = c("CODING_COMPLETE", "NOT_REACHED", "CODING_COMPLETE",
                                 "DISPLAYED", NA_character_))
  for (setting in c(FALSE, TRUE)) {
    out <- recode_missings(f$data, f$units, recode_omissions_to_not_reached = setting)
    expect_equal(out$code_type[2], "MISSING_BY_OMISSION")
    expect_equal(out$code_id[2], -99)
    expect_equal(out$code_score[2], 0)
    expect_equal(out$code_type[5], "MISSING_NOT_REACHED")
    expect_equal(out$code_status, f$data$code_status)
    expect_equal(out$value, f$data$value)
  }
  expect_equal(recode_missings(f$data, f$units)$code_type[4], "MISSING_BY_OMISSION")
  expect_equal(recode_missings(f$data, f$units,
                              recode_omissions_to_not_reached = TRUE)$code_type[4],
               "MISSING_NOT_REACHED")
})

test_that("TRUE retains middle omissions and only changes trailing omissions", {
  local_recode_metadata()
  f <- recode_fixture(c("FULL_CREDIT", "MISSING_BY_OMISSION", "FULL_CREDIT",
                        "MISSING_BY_OMISSION"))
  out <- recode_missings(f$data, f$units, recode_omissions_to_not_reached = TRUE)
  expect_equal(out$code_type, c("FULL_CREDIT", "MISSING_BY_OMISSION", "FULL_CREDIT",
                                "MISSING_NOT_REACHED"))
  expect_equal(out$code_id, c(1, -99, 1, -96))
  expect_equal(out$code_score, c(1, 0, 1, NA_real_))
})

test_that("FALSE preserves omission anchors and TRUE can classify an all-omission testlet", {
  local_recode_metadata()
  f <- recode_fixture(c("MISSING_NOT_REACHED", "MISSING_BY_OMISSION"),
                      status = c("NOT_REACHED", "DISPLAYED"))
  expect_equal(recode_missings(f$data, f$units)$code_type,
               rep("MISSING_BY_OMISSION", 2))
  expect_equal(recode_missings(f$data, f$units,
                              recode_omissions_to_not_reached = TRUE)$code_type,
               rep("MISSING_NOT_REACHED", 2))
})

test_that("basis invalid responses anchor but coding failures without values do not", {
  local_recode_metadata()
  f <- recode_fixture(c("MISSING_NOT_REACHED", "MISSING_INVALID_RESPONSE",
                        "MISSING_NOT_REACHED", "MISSING_CODING_IMPOSSIBLE", "NO_CODING"),
                      value = c(NA, "invalid input", NA, NA, NA))
  out <- recode_missings(f$data, f$units)
  expect_equal(out$code_type[c(1, 3)], c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED"))
  expect_equal(out$code_type[2], "MISSING_INVALID_RESPONSE")
  f$data$value[5] <- "uncoded response"
  out <- recode_missings(f$data, f$units)
  expect_equal(out$code_type[c(1, 3)], rep("MISSING_BY_OMISSION", 2))
  expect_equal(out$code_type[4:5], f$data$code_type[4:5])
})

test_that("derived valid and invalid results never move the basis boundary", {
  local_recode_metadata()
  for (derived_type in c("FULL_CREDIT", "MISSING_INVALID_RESPONSE")) {
    f <- recode_fixture(c(B = "MISSING_NOT_REACHED", D = derived_type),
                        sources = list(D = "B"))
    out <- recode_missings(f$data, f$units)
    expect_equal(out$code_type[1], "MISSING_NOT_REACHED")
    expect_equal(out$code_type[2], "MISSING_NOT_REACHED")
    expect_equal(out$code_id[2], -96)
    expect_true(is.na(out$code_score[2]))
  }
})

test_that("derived missing classification uses actual last source instead of virtual rank", {
  local_recode_metadata()
  f <- recode_fixture(c(B1 = "FULL_CREDIT", B2 = "MISSING_NOT_REACHED",
                        B3 = "FULL_CREDIT", D = "MISSING_NOT_REACHED"),
                      sources = list(D = c("B1", "B2")))
  out <- recode_missings(f$data, f$units)
  expect_equal(out$code_type[c(2, 4)], rep("MISSING_BY_OMISSION", 2))
  expect_equal(out$code_id[c(2, 4)], rep(-99, 2))

  f <- recode_fixture(c(B = "FULL_CREDIT", D = "MISSING_BY_OMISSION"),
                      sources = list(D = "B"))
  out <- recode_missings(f$data, f$units, recode_omissions_to_not_reached = TRUE)
  expect_equal(out$code_type[2], "MISSING_BY_OMISSION")
})

test_that("derived invalid recoding requires all sources to be proven not reached", {
  local_recode_metadata()
  f <- recode_fixture(c(B1 = "MISSING_BY_OMISSION", B2 = "FULL_CREDIT",
                        B3 = "MISSING_NOT_REACHED", D = "MISSING_INVALID_RESPONSE"),
                      sources = list(D = c("B1", "B3")),
                      status = c("DISPLAYED", "CODING_COMPLETE", "NOT_REACHED", "INVALID"))
  f$data$code_score[4] <- 0.75
  original <- recode_missings(f$data, f$units)
  expect_equal(original[4, c("code_status", "code_id", "code_type", "code_score")],
               f$data[4, c("code_status", "code_id", "code_type", "code_score")])
  out <- recode_missings(f$data, f$units, recode_omissions_to_not_reached = TRUE)
  expect_equal(out[4, c("code_status", "code_id", "code_type", "code_score")],
               f$data[4, c("code_status", "code_id", "code_type", "code_score")])

  # A source before the reached boundary prevents the stricter invalid rule,
  # even though every source is missing and the last source is not reached.
  out <- recode_missings(f$data, f$units, recode_omissions_to_not_reached = TRUE)
  expect_equal(out$code_type[4], "MISSING_INVALID_RESPONSE")
  expect_equal(out$code_score[4], 0.75)

  f$units$basis_sources[[4]] <- c("B2", "B3")
  out <- recode_missings(f$data, f$units, recode_omissions_to_not_reached = TRUE)
  expect_equal(out$code_type[4], "MISSING_INVALID_RESPONSE")
  expect_equal(out$code_score[4], 0.75)

  f$units$basis_sources[[4]] <- "B3"
  out <- recode_missings(f$data, f$units, recode_omissions_to_not_reached = TRUE)
  expect_equal(out$code_type[4], "MISSING_NOT_REACHED")
  expect_true(is.na(out$code_score[4]))
  expect_equal(out$code_id[4], -96)
  expect_equal(out$code_status[4], "INVALID")
})

test_that("all-not-reached sources correct valid derived results but preserve coding failures", {
  local_recode_metadata()
  f <- recode_fixture(c(B = "MISSING_NOT_REACHED", D1 = "FULL_CREDIT",
                        D2 = "MISSING_CODING_IMPOSSIBLE", D3 = "NO_CODING"),
                      sources = list(D1 = "B", D2 = "B", D3 = "B"))
  f$data$code_id[2:4] <- c(42, 43, 44)
  f$data$code_score[2:4] <- c(0.4, 0.5, 0.6)
  out <- recode_missings(f$data, f$units, recode_omissions_to_not_reached = TRUE)
  expect_equal(out$code_type[2], "MISSING_NOT_REACHED")
  expect_equal(out$code_id[2], -96)
  expect_true(is.na(out$code_score[2]))
  expect_equal(out[3:4, c("code_id", "code_type", "code_score")],
               f$data[3:4, c("code_id", "code_type", "code_score")])
})

test_that("valid and invalid derived corrections share the source rule and custom profiles", {
  local_recode_metadata()
  profile <- tibble::tibble(code_type = "MISSING_NOT_REACHED", code_id = -196,
                            code_status = "CUSTOM_NR", code_score = NA_real_)
  for (type in c("RESIDUAL_AUTO", "FULL_CREDIT", "MISSING_INVALID_RESPONSE", NA_character_)) {
    for (score in c(0, 1)) {
      f <- recode_fixture(c(B1 = "MISSING_NOT_REACHED", B2 = "MISSING_NOT_REACHED", D = type),
                          sources = list(D = c("B1", "B2")),
                          status = c("NOT_REACHED", NA_character_,
                                     if (type %in% "MISSING_INVALID_RESPONSE") "INVALID" else "CODING_COMPLETE"))
      f$data$code_id[3] <- 42
      f$data$code_score[3] <- score
      f$data$value[3] <- "stored derived value"
      out <- recode_missings(f$data, f$units, missings = profile, diagnostics = "none")
      expect_equal(out$code_type[3], "MISSING_NOT_REACHED")
      expect_equal(out$code_id[3], -196)
      expect_true(is.na(out$code_score[3]))
      expect_identical(out$code_status, f$data$code_status)
      expect_identical(out$value, f$data$value)
      expect_identical(recode_missings(out, f$units, missings = profile,
                                       diagnostics = "none"), out)
    }
  }
})

test_that("valid derived results require all sources to be proven not reached", {
  local_recode_metadata()
  for (source_type in c("FULL_CREDIT", "MISSING_BY_OMISSION", "MISSING_INVALID_RESPONSE")) {
    f <- recode_fixture(c(B1 = source_type, B2 = "MISSING_NOT_REACHED", D = "RESIDUAL_AUTO"),
                        sources = list(D = c("B1", "B2")))
    f$data$code_id[3] <- 0
    f$data$code_score[3] <- 0
    out <- recode_missings_impl(f$data, f$units)
    expect_identical(out$data[3, ], f$data[3, ])
    expect_true(is.na(out$reasons[3]))
  }
  # Unknown source trees and unconfirmed relative positions cannot justify the change.
  f <- recode_fixture(c(B = "MISSING_NOT_REACHED", D = "RESIDUAL_AUTO"),
                      sources = list(D = character()))
  out <- recode_missings_impl(f$data, f$units)
  expect_identical(out$data[2, ], f$data[2, ])
  expect_equal(out$reasons[2], "sources")

  f <- recode_fixture(c(B1 = "FULL_CREDIT", B2 = "MISSING_NOT_REACHED", D = "RESIDUAL_AUTO"),
                      sources = list(D = "B2"))
  f$data$order_source <- "name_fallback"
  f$units$variable_page <- NA_integer_
  uncertain <- recode_missings_impl(f$data, f$units)
  expect_identical(uncertain$data[3, ], f$data[3, ])
  expect_equal(uncertain$reasons[3], "order")
  confirmed <- recode_missings(f$data, f$units, use_variable_names_for_recoding = TRUE,
                               diagnostics = "none")
  expect_equal(confirmed$code_type[3], "MISSING_NOT_REACHED")
})

test_that("design-added derived rows are reconstructed only from complete missing sources", {
  local_recode_metadata()
  f <- recode_fixture(c(B1 = "MISSING_NOT_REACHED", B2 = "FULL_CREDIT", D = NA_character_),
                      sources = list(D = "B1"))
  f$data$response_present[3] <- FALSE
  out <- recode_missings(f$data, f$units)
  expect_equal(out$code_type[3], "MISSING_BY_OMISSION")
  expect_equal(out$code_score[3], 0)
  expect_true(is.na(out$code_status[3]))

  f$units$basis_sources[[3]] <- "B2"
  out <- recode_missings(f$data, f$units)
  expect_true(is.na(out$code_type[3]))
  expect_true(is.na(out$code_id[3]))
  expect_true(is.na(out$code_score[3]))

  f$units$basis_sources[[3]] <- "B1"
  f$data$response_present[3] <- TRUE
  out <- recode_missings(f$data, f$units)
  expect_true(is.na(out$code_type[3]))
})

test_that("unknown derived sources preserve outputs and cannot justify an invalid override", {
  local_recode_metadata()
  f <- recode_fixture(c(B = "MISSING_NOT_REACHED", D = "MISSING_INVALID_RESPONSE"),
                      sources = list(D = character()))
  f$data$code_score[2] <- 0.75
  out <- recode_missings(f$data, f$units, recode_omissions_to_not_reached = TRUE)
  expect_equal(out$code_type[2], "MISSING_INVALID_RESPONSE")
  expect_equal(out$code_score[2], 0.75)
  f$data$code_type[2] <- "MISSING_NOT_REACHED"
  f$data$code_score[2] <- 0.5
  f$data$code_id[2] <- -196
  out <- recode_missings(f$data, f$units)
  expect_equal(out$code_score[2], 0.5)
  expect_equal(out$code_id[2], -196)
})

test_that("missing profiles deliberately assign NA scores without changing technical status", {
  local_recode_metadata()
  f <- recode_fixture(c("MISSING_NOT_REACHED", "FULL_CREDIT", "MISSING_BY_OMISSION"),
                      status = c("NOT_REACHED", "CODING_COMPLETE", NA_character_))
  f$data$code_score <- c(0.7, 1, 0.9)
  profile <- tibble::tibble(
    code_type = c("MISSING_NOT_REACHED", "MISSING_BY_OMISSION"),
    code_id = c(-196, -199), code_status = c("CUSTOM_NR", "CUSTOM_O"),
    code_score = c(NA_real_, 0.25)
  )
  out <- recode_missings(f$data, f$units, missings = profile,
                         recode_omissions_to_not_reached = TRUE)
  expect_equal(out$code_id, c(-199, 1, -196))
  expect_equal(out$code_score, c(0.25, 1, NA_real_))
  expect_equal(out$code_status, f$data$code_status)
})

test_that("unresolved raw statuses initialize analytical types while preserving valid results", {
  local_recode_metadata()
  f <- recode_fixture(c(NA_character_, "FULL_CREDIT", NA_character_, NA_character_),
                      status = c("NOT_REACHED", "DISPLAYED", "INVALID", "NO_CODING"),
                      value = c(NA, "valid result", "invalid response", NA))
  out <- recode_missings(f$data, f$units)
  expect_equal(out$code_type, c("MISSING_BY_OMISSION", "FULL_CREDIT",
                                "MISSING_INVALID_RESPONSE", "NO_CODING"))
  expect_equal(out$code_id, c(-99, 1, -98, -93))
  expect_equal(out$code_status, f$data$code_status)
})

test_that("classification is isolated between people, booklets, and testlets", {
  local_recode_metadata()
  f <- recode_fixture(rep("MISSING_NOT_REACHED", 2))
  later <- f$data
  later$code_type <- "FULL_CREDIT"
  later$code_id <- 1
  later$code_score <- 1
  later$value <- "response"
  f$data <- dplyr::bind_rows(f$data, later)
  f$data$testlet_no <- c(1L, 1L, 2L, 2L)
  f$data$variable_order <- 1:4
  expect_equal(recode_missings(f$data, f$units)$code_type[1:2], rep("MISSING_NOT_REACHED", 2))
  f$data$testlet_no <- 1L
  f$data$login_name <- c("L1", "L1", "L2", "L2")
  f$data$variable_order <- c(1L, 2L, 1L, 2L)
  expect_equal(recode_missings(f$data, f$units)$code_type[1:2], rep("MISSING_NOT_REACHED", 2))
  f$data$login_name <- "L1"
  f$data$booklet_id <- c("B1", "B1", "B2", "B2")
  expect_equal(recode_missings(f$data, f$units)$code_type[1:2], rep("MISSING_NOT_REACHED", 2))
})

test_that("standalone positions are matched case-insensitively without reordering rows", {
  local_recode_metadata()
  f <- recode_fixture(c("MISSING_NOT_REACHED", "FULL_CREDIT", "MISSING_BY_OMISSION"))
  keys <- c("booklet_id", "testlet_no", "unit_booklet_no", "unit_key", "unit_alias", "variable_id")
  positions <- f$data[c(keys, "variable_order")]
  positions$booklet_id <- "b1"
  positions$item_order <- c(1L, NA_integer_, 2L)
  positions$order_group <- c(1L, 1L, 2L)
  positions$order_source <- rep("override", 3)
  positions$item_id <- c("I1", NA_character_, "I2")
  positions$item_order_source <- c("item_selection", "not_an_item", "item_selection")
  shuffled <- f$data[c(3, 1, 2), ]
  shuffled$variable_order <- NULL
  shuffled$item_order_source <- "old_selection"
  out <- recode_missings(shuffled, f$units, positions = positions,
                         recode_omissions_to_not_reached = TRUE)
  expect_equal(out$variable_id, shuffled$variable_id)
  expect_equal(out$variable_order, c(3, 1, 2))
  expect_equal(out$item_order, c(2L, 1L, NA_integer_))
  expect_equal(out$order_group, c(2L, 1L, 1L))
  expect_equal(out$item_id, c("I2", "I1", NA_character_))
  expect_equal(out$item_order_source, c("item_selection", "item_selection", "not_an_item"))
  expect_equal(out$code_type, c("MISSING_NOT_REACHED", "MISSING_BY_OMISSION", "FULL_CREDIT"))
  expect_equal(nrow(out), nrow(shuffled))
})

test_that("missing orders, duplicate occurrences, and missing source rows fail clearly", {
  local_recode_metadata()
  f <- recode_fixture(c(B = "MISSING_NOT_REACHED", D = "MISSING_NOT_REACHED"),
                      sources = list(D = "B"))
  no_order <- f$data
  no_order$variable_order <- NULL
  expect_error(recode_missings(no_order, f$units), "variable_order")
  expect_error(recode_missings(dplyr::bind_rows(f$data, f$data[1, ]), f$units), "duplicate")
  expect_error(recode_missings(f$data[2, ], f$units), "missing expected basis variables")
  f$data$variable_order <- c(1, 1)
  expect_error(recode_missings(f$data, f$units), "uniquely")
})

test_that("a supplied position table detects fully omitted unit occurrences", {
  local_recode_metadata()
  f <- recode_fixture(c("MISSING_NOT_REACHED", "MISSING_NOT_REACHED"))
  keys <- c("booklet_id", "testlet_no", "unit_booklet_no", "unit_key", "unit_alias", "variable_id")
  positions <- f$data[c(keys, "variable_order")]
  missing_unit <- positions
  missing_unit$unit_booklet_no <- 2L
  missing_unit$unit_alias <- "SECOND"
  missing_unit$variable_order <- 3:4
  positions <- dplyr::bind_rows(positions, missing_unit)
  expect_error(recode_missings(f$data, f$units, positions = positions), "missing unit occurrences")
})

test_that("static ranks cannot differ between people with the same booklet", {
  local_recode_metadata()
  f <- recode_fixture(c("MISSING_NOT_REACHED", "FULL_CREDIT"))
  second <- f$data
  second$login_name <- "L2"
  second$variable_order <- c(2L, 1L)
  expect_error(recode_missings(dplyr::bind_rows(f$data, second), f$units), "same static")
  f$data$variable_order <- c(1, 1.5)
  expect_error(recode_missings(f$data, f$units), "integer")
})

test_that("a valid untyped basis code remains evidence when raw values and status are masked", {
  local_recode_metadata()
  f <- recode_fixture(c("MISSING_NOT_REACHED", NA_character_, "MISSING_NOT_REACHED"),
                      value = rep(NA_character_, 3))
  f$data$code_id[2] <- 0
  f$data$code_score[2] <- 0
  out <- recode_missings(f$data, f$units)
  expect_equal(out$code_type[c(1, 3)], c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED"))
  expect_true(is.na(out$code_type[2]))
  expect_equal(out$code_id[2], 0)
  expect_equal(out$code_score[2], 0)

  f$data$code_id[2] <- NA_real_
  out <- recode_missings(f$data, f$units)
  expect_equal(out$code_type[1], "MISSING_BY_OMISSION")
  expect_equal(out$code_score[2], 0)
})

test_that("untyped derived invalid fields are preserved unless all sources are not reached", {
  local_recode_metadata()
  f <- recode_fixture(c(B = "FULL_CREDIT", D = NA_character_),
                      sources = list(D = "B"), status = c("CODING_COMPLETE", "INVALID"))
  f$data$code_id[2] <- -198
  f$data$code_score[2] <- 0.75
  out <- recode_missings(f$data, f$units)
  expect_equal(out$code_type[2], "MISSING_INVALID_RESPONSE")
  expect_equal(out$code_id[2], -198)
  expect_equal(out$code_score[2], 0.75)
  out <- recode_missings(f$data, f$units, recode_omissions_to_not_reached = TRUE)
  expect_equal(out$code_type[2], "MISSING_INVALID_RESPONSE")
  expect_equal(out$code_id[2], -198)
  expect_equal(out$code_score[2], 0.75)

  # Existing not-reached sources suffice: omission recoding remains FALSE.
  f$data$code_type[1] <- "MISSING_NOT_REACHED"
  f$data$code_id[1] <- -96
  f$data$code_score[1] <- NA_real_
  f$data$code_status[1] <- "NOT_REACHED"
  f$data$value[1] <- NA_character_
  out <- recode_missings(f$data, f$units)
  expect_equal(out$code_type[2], "MISSING_NOT_REACHED")
  expect_equal(out$code_id[2], -96)
  expect_true(is.na(out$code_score[2]))
  expect_equal(out$code_status, f$data$code_status)
})

test_that("automatic derived invalid recoding is stable for custom or missing IDs and statuses", {
  local_recode_metadata()
  for (raw_status in c("INVALID", "DERIVE_ERROR", NA_character_)) {
    for (raw_id in c(-98, -198, NA_real_)) {
      f <- recode_fixture(c(B = "MISSING_NOT_REACHED", D = "MISSING_INVALID_RESPONSE"),
                          sources = list(D = "B"), status = c("NOT_REACHED", raw_status))
      f$data$code_id[2] <- raw_id
      f$data$code_score[2] <- 0.75
      first <- recode_missings(f$data, f$units,
                               diagnostics = "none")
      second <- recode_missings(first, f$units,
                                diagnostics = "none")
      expect_equal(first$code_type[2], "MISSING_NOT_REACHED")
      expect_equal(first$code_id[2], -96)
      expect_true(is.na(first$code_score[2]))
      expect_equal(second, first)
      switched <- recode_missings(first, f$units, recode_omissions_to_not_reached = TRUE,
                                  diagnostics = "none")
      expect_equal(switched, first)
      expect_equal(first$code_status, f$data$code_status)
    }
  }
})

test_that("custom profiles consistently recode derived invalid and ordinary omission IDs", {
  local_recode_metadata()
  f <- recode_fixture(c(B = "MISSING_NOT_REACHED", D = "MISSING_INVALID_RESPONSE",
                        O = "MISSING_BY_OMISSION"), sources = list(D = "B", O = "B"))
  f$data$code_id[2] <- -198
  f$data$code_id[3] <- -98
  profile <- tibble::tibble(
    code_type = c("MISSING_INVALID_RESPONSE", "MISSING_BY_OMISSION", "MISSING_NOT_REACHED"),
    code_id = c(-198, -98, -196), code_status = c("INVALID", "DISPLAYED", "NOT_REACHED"),
    code_score = c(0, 0, NA_real_)
  )
  default <- recode_missings(f$data, f$units, missings = profile)
  expect_equal(default$code_type, c("MISSING_NOT_REACHED", "MISSING_NOT_REACHED",
                                     "MISSING_BY_OMISSION"))
  expect_equal(default$code_id, c(-196, -196, -98))

  first <- recode_missings(f$data, f$units, missings = profile,
                           recode_omissions_to_not_reached = TRUE)
  second <- recode_missings(first, f$units, missings = profile,
                            recode_omissions_to_not_reached = TRUE)
  expect_equal(first$code_type, rep("MISSING_NOT_REACHED", 3))
  expect_equal(first$code_id, rep(-196, 3))
  expect_true(all(is.na(first$code_score)))
  expect_equal(first$code_status, f$data$code_status)
  expect_equal(second, first)
  switched <- recode_missings(first, f$units, missings = profile)
  expect_equal(switched, first)
})

test_that("untyped basis missing IDs follow the effective custom profile", {
  local_recode_metadata()
  f <- recode_fixture(c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED"))
  f$data$code_type <- NA_character_
  f$data$code_id <- c(-98, -196)
  f$data$code_score <- 0
  profile <- tibble::tibble(
    code_type = c("MISSING_INVALID_RESPONSE", "MISSING_BY_OMISSION", "MISSING_NOT_REACHED"),
    code_id = c(-198, -98, -196), code_status = c("INVALID", "DISPLAYED", "NOT_REACHED"),
    code_score = c(0, 0, NA_real_)
  )
  out <- recode_missings(f$data, f$units, missings = profile)
  expect_equal(out$code_type, c("MISSING_BY_OMISSION", "MISSING_NOT_REACHED"))
  expect_equal(out$code_id, c(-98, -196))
  expect_equal(out$code_score, c(0, NA_real_))
  expect_equal(out$code_status, f$data$code_status)
  tail <- recode_missings(f$data, f$units, missings = profile,
                          recode_omissions_to_not_reached = TRUE)
  expect_equal(tail$code_type, rep("MISSING_NOT_REACHED", 2))
  expect_equal(tail$code_id, rep(-196, 2))
  expect_true(all(is.na(tail$code_score)))
})
