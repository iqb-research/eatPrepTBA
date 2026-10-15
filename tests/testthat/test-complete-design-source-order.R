source_order_fixture <- function(ids, sources = list(), types = rep("A", length(ids)),
                                 pages = 1, sections = 1) {
  refs <- paste0("reference_", seq_along(ids))
  source_rows <- lapply(ids, function(id) {
    src <- sources[[id]]
    at <- match(src, ids)
    tibble::tibble(
      variable_source_ref = ifelse(is.na(at), paste0("missing_", src), refs[at]),
      variable_source_id = as.character(src),
      variable_source_direct = rep(TRUE, length(src))
    )
  })
  codes <- tibble::tibble(
    variable_id = ids, variable_ref = refs,
    variable_source_type = ifelse(ids %in% names(sources), "SUM_CODE", "BASE"),
    variable_level = ifelse(ids %in% names(sources), 1L, 0L),
    variable_page = pages, variable_section = sections,
    variable_page_always_visible = FALSE, variable_sources = source_rows
  )
  design <- tibble::tibble(
    login_code = "P1", booklet_id = "B1", booklet_no = 1L,
    testlet_no = 1L, unit_booklet_no = 1L, unit_key = "U1",
    unit_alias = "first", variable_id = ids
  )
  type_map <- c(A = "FULL_CREDIT", I = "MISSING_INVALID_RESPONSE",
                E = "MISSING_CODING_IMPOSSIBLE", O = "MISSING_BY_OMISSION",
                NR = "MISSING_NOT_REACHED")
  id_map <- c(A = 1, I = -98, E = -97, O = -99, NR = -96)
  status_map <- c(A = "CODING_COMPLETE", I = "INVALID", E = "CODING_ERROR",
                  O = "DISPLAYED", NR = "NOT_REACHED")
  score_map <- c(A = 1, I = 0, E = NA_real_, O = 0, NR = NA_real_)
  coded <- design %>%
    dplyr::select(login_code, booklet_id, unit_key, unit_alias, variable_id) %>%
    dplyr::mutate(
      value = ifelse(types == "A", "answer", NA_character_),
      code_type = unname(type_map[types]), code_id = unname(id_map[types]),
      code_status = unname(status_map[types]), code_score = unname(score_map[types])
    )
  list(units = tibble::tibble(
    unit_key = "U1", unit_codes = list(codes),
    items_list = list(tibble::tibble(variable_id = ids, item_id = paste0("I", seq_along(ids))))
  ),
       design = design, coded = coded)
}

source_order_complete <- function(f, recode = FALSE, expect_fallback = FALSE) {
  boundary_warnings <- character()
  out <- withCallingHandlers(suppressMessages(complete_design(
    f$coded, f$units, f$design, identifiers = "login_code",
    recode_omissions_to_not_reached = recode
  )), eatPrepTBA_item_boundary_fallback = function(w) {
    boundary_warnings <<- c(boundary_warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_length(boundary_warnings, as.integer(expect_fallback))
  out
}

source_order_ids <- function(out, ids, column = "code_id") {
  out[[column]][match(ids, out$variable_id)]
}

test_that("a derived variable follows its last source rather than its own position", {
  f <- source_order_fixture(c("01", "01a", "01b", "02"),
                            sources = list(`01` = c("01a", "01b")))
  f$units$unit_codes[[1]]$variable_page[[1]] <- 9
  f$units$unit_codes[[1]]$variable_section[[1]] <- 99
  f$units$unit_codes[[1]]$variable_level[[1]] <- 20L
  out <- source_order_complete(f)
  expect_equal(out$variable_id, c("01a", "01b", "01", "02"))
  expect_identical(out$variable_position, 1:4)
  expect_equal(out$code_id, rep(1, 4))

  # Alias-only tables are supported when no reference column is available.
  f$units$unit_codes[[1]]$variable_sources <- lapply(
    f$units$unit_codes[[1]]$variable_sources,
    function(x) dplyr::select(x, -variable_source_ref)
  )
  expect_equal(source_order_complete(f)$variable_id, out$variable_id)
})

test_that("derived siblings use natural names and newly ready descendants follow immediately", {
  f <- source_order_fixture(c("D10", "D2", "Z", "B2", "B1"),
    sources = list(D10 = "B1", D2 = "B1", Z = "D2"))
  expect_equal(source_order_complete(f)$variable_id, c("B1", "D2", "Z", "D10", "B2"))

  # A dependency on a later basis delays the result until that source occurs.
  f$units$unit_codes[[1]]$variable_sources[[3]] <- tibble::tibble(
    variable_source_ref = f$units$unit_codes[[1]]$variable_ref[c(2, 4)],
    variable_source_direct = TRUE
  )
  expect_equal(source_order_complete(f)$variable_id, c("B1", "D2", "D10", "B2", "Z"))
})

test_that("source-aware ordering and classification are invariant to shuffled input", {
  f <- source_order_fixture(c("01", "01a", "01b", "02"),
    sources = list(`01` = c("01a", "01b")), types = c("A", "A", "NR", "O"))
  shuffled <- f
  shuffled$design <- f$design[c(4, 1, 3, 2), ]
  shuffled$coded <- f$coded[c(2, 4, 1, 3), ]
  shuffled$units$unit_codes[[1]] <- f$units$unit_codes[[1]][c(3, 1, 4, 2), ]
  shuffled$units$unit_codes[[1]]$variable_sources[[2]] <-
    shuffled$units$unit_codes[[1]]$variable_sources[[2]][2:1, ]
  fields <- c("variable_id", "variable_position", "value", "code_status", "code_id", "code_type", "code_score")
  for (recode in c(FALSE, TRUE)) {
    expect_equal(source_order_complete(shuffled, recode)[fields],
                 source_order_complete(f, recode)[fields])
  }
})

test_that("valid and invalid derived results can turn earlier NR sources into omissions", {
  ids <- c("01", "01a", "01b", "02")
  for (derived_type in c("A", "I", "E")) {
    f <- source_order_fixture(ids, sources = list(`01` = c("01a", "01b")),
                              types = c(derived_type, "A", "NR", "NR"))
    for (recode in c(FALSE, TRUE)) {
      out <- source_order_complete(f, recode)
      expect_equal(source_order_ids(out, ids), c(f$coded$code_id[[1]], 1, -99, -96))
      expect_equal(source_order_ids(out, ids, "code_status"), f$coded$code_status)
      expect_equal(source_order_ids(out, "01", "code_type"), f$coded$code_type[[1]])
      expect_equal(source_order_ids(out, "01", "code_score"), f$coded$code_score[[1]])
      expect_equal(source_order_ids(out, ids, "value"), f$coded$value)
    }
  }
})

test_that("derived omissions and NR still follow the existing missing rules", {
  ids <- c("01", "01a", "01b", "02")
  f <- source_order_fixture(ids, sources = list(`01` = c("01a", "01b")),
                            types = c("O", "A", "NR", "NR"))
  expect_equal(source_order_ids(source_order_complete(f), ids), c(-99, 1, -99, -96))
  expect_equal(source_order_ids(source_order_complete(f, TRUE), ids), c(-96, 1, -96, -96))

  f <- source_order_fixture(ids, sources = list(`01` = c("01a", "01b")),
                            types = c("NR", "A", "NR", "A"))
  f$coded$code_status[[1]] <- NA_character_
  for (recode in c(FALSE, TRUE)) {
    out <- source_order_complete(f, recode)
    expect_equal(source_order_ids(out, ids), c(-99, 1, -99, 1))
    expect_equal(source_order_ids(out, ids, "code_status"), f$coded$code_status)
  }
})

test_that("missing source metadata uses positional fallback without inferring name prefixes", {
  ids <- c("01", "01a", "01b", "02")
  for (kind in c("absent", "empty", "partial", "ambiguous")) {
    f <- source_order_fixture(ids, sources = list(`01` = c("01a", "01b")))
    codes <- f$units$unit_codes[[1]]
    if (kind == "absent") {
      codes$variable_sources <- NULL
    } else if (kind == "empty") {
      codes$variable_sources[[1]] <- tibble::tibble()
    } else if (kind == "partial") {
      codes$variable_sources[[1]]$variable_source_ref[[2]] <- "unknown_reference"
      codes$variable_sources[[1]]$variable_source_id[[2]] <- NA_character_
    } else {
      codes$variable_ref[2:3] <- "duplicate_reference"
      codes$variable_sources[[1]] <- tibble::tibble(variable_source_ref = "duplicate_reference")
    }
    f$units$unit_codes[[1]] <- codes
    expect_equal(source_order_complete(f, expect_fallback = TRUE)$variable_id, ids, info = kind)
  }

  # Fallback still honours an existing page and section, including inherited pages.
  f$units$unit_codes[[1]]$variable_page[[1]] <- 2
  expect_equal(source_order_complete(f, expect_fallback = TRUE)$variable_id, c("01a", "01b", "02", "01"))
})

test_that("derive_sources is a fallback for absent or empty prepared source lists", {
  f <- source_order_fixture(c("01", "01a", "01b", "02"),
                            sources = list(`01` = c("01a", "01b")))
  codes <- f$units$unit_codes[[1]]
  codes$derive_sources <- list(codes$variable_ref[2:3], character(), character(), character())
  expected <- c("01a", "01b", "01", "02")
  codes$variable_sources <- NULL
  f$units$unit_codes[[1]] <- codes
  expect_equal(source_order_complete(f)$variable_id, expected)
  codes$variable_sources <- rep(list(tibble::tibble()), 4)
  f$units$unit_codes[[1]] <- codes
  expect_equal(source_order_complete(f)$variable_id, expected)

  # A nonempty, incomplete prepared list must not be hidden by an alternative.
  codes$variable_sources[[1]] <- tibble::tibble(
    variable_source_ref = c(codes$variable_ref[[2]], "unknown_reference")
  )
  f$units$unit_codes[[1]] <- codes
  expect_equal(source_order_complete(f, expect_fallback = TRUE)$variable_id, c("01", "01a", "01b", "02"))

  # Nonempty malformed entries are not equivalent to absent source metadata.
  for (entry in list("unknown_reference", list(variable_source_ref = "unknown_reference"))) {
    codes$variable_sources[[1]] <- entry
    f$units$unit_codes[[1]] <- codes
    expect_equal(source_order_complete(f, expect_fallback = TRUE)$variable_id, c("01", "01a", "01b", "02"))
  }
})

test_that("direct-source flags separate immediate dependencies from the expanded source tree", {
  f <- source_order_fixture(c("D", "E", "B1", "B2"),
    sources = list(D = "B1", E = "D"))
  codes <- f$units$unit_codes[[1]]
  codes$variable_sources[[2]] <- tibble::tibble(
    variable_source_ref = c(codes$variable_ref[[1]], "unavailable_indirect_reference"),
    variable_source_direct = c(TRUE, FALSE)
  )
  f$units$unit_codes[[1]] <- codes
  expect_equal(source_order_complete(f)$variable_id, c("B1", "D", "E", "B2"))

  # Old expanded trees can have no TRUE direct flags. Their complete list still
  # expresses the dependency, including both the intermediate and basis source.
  codes$variable_sources[[2]] <- tibble::tibble(
    variable_source_ref = codes$variable_ref[c(1, 3)],
    variable_source_direct = FALSE
  )
  codes$variable_sources[[1]]$variable_source_direct <- FALSE
  f$units$unit_codes[[1]] <- codes
  expect_equal(source_order_complete(f)$variable_id, c("B1", "D", "E", "B2"))
})

test_that("cycles fall back to positions without blocking independent derivations", {
  f <- source_order_fixture(c("A", "D", "X", "Y", "Z"),
    sources = list(D = "X", X = "Y", Y = "X", Z = "A"))
  expect_equal(source_order_complete(f, expect_fallback = TRUE)$variable_id, c("A", "Z", "D", "X", "Y"))

  f <- source_order_fixture(c("D", "B", "E"), sources = list(D = "D", E = "B"))
  expect_equal(source_order_complete(f, expect_fallback = TRUE)$variable_id, c("B", "E", "D"))

  # A known dependency on a variable with missing source metadata is still usable.
  f <- source_order_fixture(c("D", "B", "A"),
    sources = list(D = character(), A = "D"))
  expect_equal(source_order_complete(f, expect_fallback = TRUE)$variable_id, c("B", "D", "A"))
})

test_that("sources never resolve against variables of another unit", {
  first <- source_order_fixture(c("01", "A"), sources = list(`01` = "external"))
  second <- source_order_fixture(c("01", "external"), sources = list(`01` = "external"))
  second$units$unit_codes[[1]]$variable_ref[[2]] <- "missing_external"
  second$units$unit_codes[[1]]$variable_sources[[1]]$variable_source_ref <- "missing_external"
  second$units$unit_key <- "U2"
  second$design$unit_key <- second$coded$unit_key <- "U2"
  second$design$unit_alias <- second$coded$unit_alias <- "second"
  second$design$unit_booklet_no <- 2L
  f <- list(units = dplyr::bind_rows(first$units, second$units),
            design = dplyr::bind_rows(first$design, second$design),
            coded = dplyr::bind_rows(first$coded, second$coded))
  out <- source_order_complete(f, expect_fallback = TRUE)
  expect_equal(out$variable_id[out$unit_key == "U1"], c("01", "A"))
  expect_equal(out$variable_id[out$unit_key == "U2"], c("external", "01"))
  expect_identical(out$variable_position, c(1:2, 1:2))
})

test_that("long dependency chains are ordered without recursive stack growth", {
  derived <- paste0("D", seq_len(800L))
  ids <- c(derived, "B", "Z")
  sources <- stats::setNames(as.list(c("B", head(derived, -1L))), derived)
  f <- source_order_fixture(ids, sources)
  expect_equal(source_order_complete(f)$variable_id, c("B", derived, "Z"))
})

source_order_raw_units <- function() {
  ids <- c("01a", "01b", "01", "02")
  refs <- c("source_a", "source_b", "sum_ab", "later")
  variables <- lapply(seq_along(ids), function(i) {
    list(id = refs[[i]], alias = ids[[i]], label = ids[[i]],
      sourceType = if (i == 3L) "SUM_CODE" else "BASE",
      sourceParameters = list(processing = list(), solverExpression = NULL),
      deriveSources = if (i == 3L) as.list(refs[1:2]) else list(),
      processing = list(), codeModel = "basic",
      codes = list(list(id = 1, type = "FULL_CREDIT", label = "Right", score = 1,
                         ruleSetOperatorAnd = TRUE, ruleSets = list())))
  })
  tibble::tibble(
    ws_id = 1L, unit_id = 1L, unit_key = "U1",
    items_list = list(tibble::tibble(variable_id = c("01", "02"), item_id = c("I1", "I2"))),
    coding_scheme = as.character(jsonlite::toJSON(
      list(variableCodings = variables), auto_unbox = TRUE, null = "null")),
    unit_variables = list(tibble::tibble(
      variable_ref = refs[c(1, 2, 4)], variable_type = "text", variable_format = "",
      variable_multiple = FALSE, variable_page = 1L
    )),
    variable_pages = list(tibble::tibble(
      variable_ref = refs[c(1, 2, 4)], variable_page = 1L,
      variable_section = c(1L, 1L, 2L), variable_element = 1:3,
      variable_content = NA_character_, variable_page_always_visible = FALSE
    ))
  )
}

test_that("actual prepared coding schemes supply references and inferred derived pages", {
  prepared <- suppressMessages(add_coding_scheme(source_order_raw_units()))
  codes <- prepared$unit_codes[[1]]
  derived <- match("01", codes$variable_id)
  expect_equal(codes$variable_page[[derived]], 1)
  expect_true(is.na(codes$variable_section[[derived]]))
  expect_setequal(codes$variable_sources[[derived]]$variable_source_ref, c("source_a", "source_b"))
  expect_setequal(codes$variable_sources[[derived]]$variable_source_id, c("01a", "01b"))
  expect_true(all(codes$variable_sources[[derived]]$variable_source_direct))
  f <- source_order_fixture(c("01", "01a", "01b", "02"),
                            types = c("A", "A", "NR", "NR"))
  f$units <- prepared
  out <- source_order_complete(f)
  expect_equal(out$variable_id, c("01a", "01b", "01", "02"))
  expect_equal(out$code_id, c(1, -99, 1, -96))
  expect_equal(source_order_ids(out, f$coded$variable_id, "code_status"), f$coded$code_status)
})
