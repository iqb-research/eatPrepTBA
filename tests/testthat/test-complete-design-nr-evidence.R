nr_evidence_fixture <- function(
    states = c("O", "NR", "A", "NR", "NR"),
    ids = c("EarlierO", "EarlierNR", "Marker", "Reached", "Item"),
    key = "U") {
  lookup <- tibble::tribble(
    ~state, ~code_type, ~code_status, ~code_id, ~code_score, ~value,
    "A", "FULL_CREDIT", "CODING_COMPLETE", 1, 1, "answer",
    "O", "MISSING_BY_OMISSION", "DISPLAYED", -99, 0, NA_character_,
    "NR", "MISSING_NOT_REACHED", "NOT_REACHED", -96, NA_real_, NA_character_
  )
  design <- tibble::tibble(
    login_code = "P", booklet_id = "B", booklet_no = 1L,
    testlet_no = 1L, unit_key = key, unit_alias = key,
    unit_booklet_no = 1L, variable_id = ids
  )
  coded <- dplyr::bind_cols(
    dplyr::select(design, login_code, booklet_id, unit_key, unit_alias, variable_id),
    lookup[match(states, lookup$state),
           c("code_type", "code_status", "code_id", "code_score", "value")]
  )
  marker <- which(ids == "Marker")
  coded$code_type[marker] <- "NO_CREDIT"
  coded$code_id[marker] <- coded$code_score[marker] <- 0
  coded$value[marker] <- NA_character_
  coded$coding_from_not_reached <- ids == "Marker"
  codes <- tibble::tibble(
    variable_id = ids, variable_ref = paste0("ref_", ids),
    variable_source_type = "BASE", variable_level = 0L,
    variable_page = seq_along(ids), variable_section = 1L,
    variable_page_always_visible = FALSE,
    variable_sources = rep(list(NULL), length(ids)),
    variable_source_processing = lapply(ids, function(id) {
      if (id == "Marker") "TAKE_NOT_REACHED_AS_VALUE_CHANGED" else character()
    })
  )
  item <- which(ids == "Item")
  if (length(item)) {
    sources <- intersect(c("Marker", "Reached"), ids)
    codes$variable_source_type[item] <- "COPY_VALUE"
    codes$variable_level[item] <- 1L
    codes$variable_sources[[item]] <- tibble::tibble(
      variable_source_id = sources, variable_source_ref = paste0("ref_", sources),
      variable_source_direct = TRUE
    )
  }
  item_ids <- setdiff(ids, c("Marker", "Reached"))
  units <- tibble::tibble(
    unit_key = key, unit_codes = list(codes),
    items_list = list(tibble::tibble(
      variable_id = item_ids, item_id = paste0("I_", item_ids)
    ))
  )
  list(coded = coded, units = units, design = design)
}

nr_evidence_run <- function(f, recode = TRUE, preserve = FALSE, ...) {
  rlang::local_options(lifecycle_verbosity = "quiet")
  suppressMessages(complete_design(
    f$coded, f$units, f$design, identifiers = "login_code",
    recode_omissions_to_not_reached = recode,
    preserve_not_reached = preserve, ...
  ))
}

nr_evidence_row <- function(out, id) out[out$variable_id == id, , drop = FALSE]

test_that("known NR promotions are boundary-neutral under all missing options", {
  f <- nr_evidence_fixture()
  for (recode in c(FALSE, TRUE)) {
    for (preserve in c(FALSE, TRUE)) {
      expect_no_warning(out <- nr_evidence_run(f, recode, preserve))
      expect_equal(out$code_id, c(if (recode) -96 else -99, -96, 0, -96, -96))
      expect_equal(out$code_score, c(if (recode) NA_real_ else 0, NA, 0, NA, NA))
      expect_identical(out$code_status, f$coded$code_status)
      expect_identical(out$value, f$coded$value)
      expect_identical(out$coding_from_not_reached, f$coded$coding_from_not_reached)
      expect_identical(out$variable_position, seq_len(nrow(f$design)))
      expect_equal(nr_evidence_row(out, "Marker")$code_type, "NO_CREDIT")
      expect_false(any(startsWith(names(out), ".completion")))

      ordinary <- f
      ordinary$coded$coding_from_not_reached <- NULL
      baseline <- nr_evidence_run(ordinary, recode, preserve)
      expect_equal(baseline$code_id, c(-99, if (preserve) -96 else -99, 0, -96, -96))
    }
  }
})

test_that("the neutral source does not establish a boundary in a later unit", {
  first <- nr_evidence_fixture(c("O", "NR"), c("EarlierO", "EarlierNR"), "U1")
  last <- nr_evidence_fixture(c("A", "NR", "NR"), c("Marker", "Reached", "Item"), "U2")
  last$design$unit_booklet_no <- 2L
  f <- list(coded = dplyr::bind_rows(first$coded, last$coded),
            units = dplyr::bind_rows(first$units, last$units),
            design = dplyr::bind_rows(first$design, last$design))
  out <- nr_evidence_run(f)
  expect_equal(out$code_id, c(-96, -96, 0, -96, -96))
  f$coded$coding_from_not_reached <- FALSE
  baseline <- nr_evidence_run(f)
  expect_equal(baseline$code_id, c(-99, -99, 0, -96, -96))
})

test_that("schema rules and missing values do not replace response provenance", {
  f <- nr_evidence_fixture()
  f$coded$coding_from_not_reached <- NULL
  expect_no_warning(baseline <- nr_evidence_run(f))
  expect_false("coding_from_not_reached" %in% names(baseline))
  expect_equal(baseline$code_id, c(-99, -99, 0, -96, -96))
  for (provenance in c(FALSE, NA)) {
    f$coded$coding_from_not_reached <- provenance
    expect_no_warning(out <- nr_evidence_run(f))
    expect_identical(dplyr::select(out, -coding_from_not_reached), baseline)
  }

  f$coded$coding_from_not_reached <- f$coded$variable_id == "Marker"
  f$coded$value[f$coded$variable_id == "Marker"] <- "0"
  expect_equal(nr_evidence_run(f)$code_id, c(-96, -96, 0, -96, -96))
  f$coded$coding_from_not_reached <- FALSE
  expect_equal(nr_evidence_run(f)$code_id, c(-99, -99, 0, -96, -96))

  f$coded$code_status[f$coded$variable_id == "Marker"] <- "NOT_REACHED"
  expect_equal(nr_evidence_run(f)$code_id, c(-99, -99, 0, -96, -96))
})

test_that("the same variable can remain evidence for another person's response", {
  f <- nr_evidence_fixture()
  other <- f$coded
  other$login_code <- "P2"
  other$coding_from_not_reached <- FALSE
  other_design <- f$design
  other_design$login_code <- "P2"
  f$coded <- dplyr::bind_rows(f$coded, other)
  f$design <- dplyr::bind_rows(f$design, other_design)
  out <- nr_evidence_run(f)
  expect_equal(out$code_id[out$login_code == "P"], c(-96, -96, 0, -96, -96))
  expect_equal(out$code_id[out$login_code == "P2"], c(-99, -99, 0, -96, -96))
})

test_that("a later genuine response still establishes the boundary", {
  f <- nr_evidence_fixture(
    c("O", "NR", "A", "NR", "NR", "A"),
    c("EarlierO", "EarlierNR", "Marker", "Reached", "Item", "Later")
  )
  out <- nr_evidence_run(f)
  expect_equal(out$code_id, c(-99, -99, 0, -99, -99, 1))
  expect_identical(out$code_status, f$coded$code_status)
  f$coded$coding_from_not_reached <- FALSE
  baseline <- nr_evidence_run(f)
  expect_identical(dplyr::select(out, -coding_from_not_reached),
                   dplyr::select(baseline, -coding_from_not_reached))
})

test_that("NR promotion provenance is not propagated to valid derived results", {
  f <- nr_evidence_fixture()
  item <- f$coded$variable_id == "Item"
  f$coded$code_type[item] <- "FULL_CREDIT"
  f$coded$code_status[item] <- "CODING_COMPLETE"
  f$coded$code_id[item] <- f$coded$code_score[item] <- 1
  f$coded$value[item] <- "derived"
  for (preserve in c(FALSE, TRUE)) {
    out <- nr_evidence_run(f, preserve = preserve)
    expect_equal(out$code_id, c(-99, if (preserve) -96 else -99, 0,
                                if (preserve) -96 else -99, 1))
    expect_equal(nr_evidence_row(out, "Item")$code_type, "FULL_CREDIT")
    expect_false(nr_evidence_row(out, "Item")$coding_from_not_reached)
    expect_equal(nr_evidence_row(out, "Item")$code_score, 1)
  }
})

test_that("newly completed rows retain unknown provenance and technical status", {
  f <- nr_evidence_fixture()
  f$coded <- f$coded[!f$coded$variable_id %in% c("EarlierNR", "Reached"), ]
  out <- nr_evidence_run(f)
  expect_equal(out$code_id, c(-96, -96, 0, -96, -96))
  completed <- out$variable_id %in% c("EarlierNR", "Reached")
  expect_true(all(is.na(out$coding_from_not_reached[completed])))
  expect_true(all(is.na(out$code_status[completed])))
  expect_true(all(out$id_used))
})

test_that("the missing scheme applies without changing regular promoted codes", {
  f <- nr_evidence_fixture()
  missings <- tibble::tribble(
    ~code_id, ~code_status, ~code_score, ~code_type,
    -196, "NOT_REACHED", NA_real_, "MISSING_NOT_REACHED",
    -197, "CODING_ERROR", NA_real_, "MISSING_CODING_IMPOSSIBLE",
    -198, "INVALID", 0, "MISSING_INVALID_RESPONSE",
    -199, "DISPLAYED", 5, "MISSING_BY_OMISSION"
  )
  for (recode in c(FALSE, TRUE)) {
    out <- nr_evidence_run(f, recode = recode, missings = missings)
    expect_equal(out$code_id, c(if (recode) -196 else -199, -196, 0, -196, -196))
    expect_equal(out$code_score, c(if (recode) NA_real_ else 5, NA, 0, NA, NA))
    expect_identical(out$code_status, f$coded$code_status)
  }
})

test_that("provenance is validated and comes only from coded data", {
  f <- nr_evidence_fixture()
  for (bad in list(rep(1, nrow(f$coded)), rep("TRUE", nrow(f$coded)),
                   rep(list(TRUE), nrow(f$coded)))) {
    f$coded$coding_from_not_reached <- bad
    expect_error(nr_evidence_run(f), "coding_from_not_reached")
  }
  f <- nr_evidence_fixture()
  expected <- nr_evidence_run(f)
  f$design$coding_from_not_reached <- FALSE
  expect_identical(nr_evidence_run(f), expected)
  f$coded$coding_from_not_reached <- NULL
  baseline <- nr_evidence_run(f)
  f$design$coding_from_not_reached <- TRUE
  expect_identical(nr_evidence_run(f), baseline)
  expect_false("coding_from_not_reached" %in% names(baseline))
})
