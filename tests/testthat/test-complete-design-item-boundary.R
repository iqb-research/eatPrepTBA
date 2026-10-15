item_boundary_fixture <- function(states, items = seq_along(states),
                                  ids = paste0("V", seq_along(states)), key = "U") {
  n <- length(states)
  lookup <- tibble::tribble(
    ~state, ~code_type, ~code_status, ~code_id, ~code_score, ~value,
    "A", "FULL_CREDIT", "CODING_COMPLETE", 1, 1, "answer",
    "O", "MISSING_BY_OMISSION", "DISPLAYED", -99, 0, NA_character_,
    "NR", "MISSING_NOT_REACHED", "NOT_REACHED", -96, NA_real_, NA_character_,
    "NC", "NO_CODING", "NO_CODING", -93, NA_real_, "Berlin",
    "E", "MISSING_INVALID_RESPONSE", "DERIVE_ERROR", -98, 0, "invalid",
    "NEW", NA_character_, NA_character_, NA_real_, NA_real_, NA_character_
  )
  design <- tibble::tibble(
    login_code = "P", booklet_id = "book", booklet_no = 1L,
    testlet_no = 1L, unit_key = key, unit_alias = key,
    unit_booklet_no = 1L, variable_id = ids
  )
  coded <- dplyr::bind_cols(
    dplyr::select(design, login_code, booklet_id, unit_key, unit_alias, variable_id),
    lookup[match(states, lookup$state), c("code_type", "code_status", "code_id", "code_score", "value")]
  )
  codes <- tibble::tibble(
    variable_id = ids, variable_ref = paste0("ref", seq_len(n)),
    variable_source_type = "BASE", variable_level = 0L,
    variable_page = seq_len(n), variable_section = 1L,
    variable_page_always_visible = FALSE,
    variable_sources = rep(list(NULL), n),
    variable_codes = rep(list(tibble::tibble(
      code_id = integer(), code_type = character(), code_score = numeric()
    )), n)
  )
  units <- tibble::tibble(
    unit_key = key, unit_codes = list(codes),
    items_list = list(tibble::tibble(variable_id = ids[items], item_id = paste0("I", items)))
  )
  list(coded = coded, units = units, design = design)
}

item_boundary_run <- function(fixture, recode = TRUE, preserve = TRUE, ...) {
  rlang::local_options(lifecycle_verbosity = "quiet")
  complete_design(
    coded = fixture$coded, units = fixture$units, design = fixture$design,
    identifiers = "login_code", recode_omissions_to_not_reached = recode,
    preserve_not_reached = preserve, ...
  )
}

item_boundary_capture <- function(fixture, ...) {
  warnings <- list()
  result <- withCallingHandlers(item_boundary_run(fixture, ...), warning = function(w) {
    warnings[[length(warnings) + 1L]] <<- w
    invokeRestart("muffleWarning")
  })
  list(result = result, warnings = warnings)
}

item_boundary_row <- function(result, id, key = "U") {
  result[result$variable_id == id & result$unit_key == key, , drop = FALSE]
}

item_boundary_raw_units <- function(score_technical = FALSE) {
  ids <- c("ItemBefore", "audio_5", "ItemAfter")
  refs <- c("answer_before", "play_count", "answer_after")
  variables <- lapply(seq_along(ids), function(i) {
    list(id = refs[i], alias = ids[i], sourceType = "BASE",
      sourceParameters = list(processing = list(), solverExpression = NULL),
      deriveSources = list(), processing = list(), codeModel = "basic",
      codes = if (i != 2L || score_technical) {
        list(list(id = 1, type = "FULL_CREDIT", score = 1,
                  ruleSetOperatorAnd = TRUE, ruleSets = list()))
      } else list())
  })
  tibble::tibble(
    ws_id = 1L, unit_id = 1L, unit_key = "U",
    coding_scheme = as.character(jsonlite::toJSON(
      list(variableCodings = variables), auto_unbox = TRUE, null = "null")),
    unit_variables = list(tibble::tibble(
      variable_ref = refs, variable_type = c("text", "integer", "text"),
      variable_format = "", variable_multiple = FALSE, variable_page = 1L
    )),
    variable_pages = list(tibble::tibble(
      variable_ref = refs, variable_page = 1L, variable_section = 1:3,
      variable_element = 1:3, variable_content = NA_integer_,
      variable_page_always_visible = FALSE
    )),
    items_list = list(tibble::tibble(
      variable_id = ids[c(1, 3)], variable_ref = refs[c(1, 3)],
      item_id = c("I1", "I2")
    ))
  )
}

test_that("real scheme preparation retains technical zero omissions between mapped items", {
  f <- item_boundary_fixture(c("A", "O", "A"), items = c(1, 3),
                             ids = c("ItemBefore", "audio_5", "ItemAfter"))
  # No prebuilt unit_codes: complete_design must prepare the actual JSON scheme.
  f$units <- item_boundary_raw_units()
  f$coded$value[2] <- "0"
  for (supplied_omission in c(FALSE, TRUE)) {
    f$coded$code_id[2] <- if (supplied_omission) -99 else NA_real_
    f$coded$code_type[2] <- if (supplied_omission) "MISSING_BY_OMISSION" else NA_character_
    f$coded$code_score[2] <- if (supplied_omission) 0 else NA_real_
    for (recode in c(FALSE, TRUE)) for (preserve in c(FALSE, TRUE)) {
      expect_no_warning(out <- item_boundary_run(f, recode, preserve))
      expect_identical(out$variable_id, f$design$variable_id)
      expect_identical(out$variable_position, 1:3)
      expect_equal(out$code_id, c(1, -99, 1))
      expect_equal(out$code_score, c(1, 0, 1))
      expect_identical(out$code_type, c("FULL_CREDIT", "MISSING_BY_OMISSION", "FULL_CREDIT"))
      expect_identical(out$value, f$coded$value)
      expect_identical(out$code_status, f$coded$code_status)
      expect_identical(add_item_id(out, f$units)$item_id, c("I1", NA_character_, "I2"))
    }
  }
  # The same raw zero with a genuine NO_CODING status remains unscored.
  f$coded$code_status[2] <- "NO_CODING"
  f$coded$code_type[2] <- NA_character_
  f$coded$code_id[2] <- f$coded$code_score[2] <- NA_real_
  expect_no_warning(out <- item_boundary_run(f))
  technical <- item_boundary_row(out, "audio_5")
  expect_equal(technical$code_id, -93)
  expect_identical(technical$code_type, "NO_CODING")
  expect_true(is.na(technical$code_score))
  expect_identical(technical$value, "0")
  expect_identical(technical$code_status, "NO_CODING")
})

test_that("real scheme preparation excludes independent scored technical variables from the boundary", {
  f <- item_boundary_fixture(c("O", "A", "NR"), items = c(1, 3),
                             ids = c("ItemBefore", "audio_5", "ItemAfter"))
  f$units <- item_boundary_raw_units(score_technical = TRUE)
  f$coded$value[2] <- "1"
  expect_no_warning(out <- item_boundary_run(f))
  expect_equal(out$code_id, c(-96, 1, -96))
  expect_equal(out$code_score, c(NA_real_, 1, NA_real_))
  expect_identical(out$code_status, f$coded$code_status)
  expect_identical(out$value, f$coded$value)

  # Only changing the mapping makes that same later response protect the omission.
  f$units$items_list[[1]] <- dplyr::bind_rows(f$units$items_list[[1]],
    tibble::tibble(variable_id = "audio_5", variable_ref = "play_count", item_id = "I3"))
  expect_no_warning(mapped <- item_boundary_run(f))
  expect_equal(mapped$code_id, c(-99, 1, -96))
  expect_equal(mapped$code_score, c(0, 1, NA_real_))
})

test_that("only mapped items contribute boundaries under all missing options", {
  f <- item_boundary_fixture(c("NR", "O", "A"), items = 1:2)
  for (recode in c(FALSE, TRUE)) for (preserve in c(FALSE, TRUE)) {
    result <- item_boundary_capture(f, recode, preserve)
    expect_length(result$warnings, 0L)
    expected <- c(if (recode || preserve) -96 else -99, if (recode) -96 else -99, 1)
    expect_equal(result$result$code_id, expected)
    expect_equal(result$result$code_score, ifelse(expected == -96, NA_real_, ifelse(expected == 1, 1, 0)))
    expect_identical(result$result$code_status, f$coded$code_status)
    expect_identical(result$result$value, f$coded$value)
    expect_identical(result$result$variable_position, 1:3)
    expect_true(all(result$result$id_used))
  }
  # A later item still prevents an earlier omission from becoming NR.
  f$units$items_list[[1]] <- tibble::tibble(variable_id = c("V1", "V2", "V3"), item_id = c("I1", "I2", "I3"))
  for (recode in c(FALSE, TRUE)) for (preserve in c(FALSE, TRUE)) {
    result <- item_boundary_run(f, recode, preserve)
    expect_equal(result$code_id, c(if (preserve) -96 else -99, -99, 1))
  }
})

test_that("NO_CODING is neutral even on an item and is not a recoding candidate", {
  f <- item_boundary_fixture(c("NR", "O", "NC"))
  f$coded$code_type[3] <- NA_character_
  f$coded$code_id[3] <- NA_real_
  for (recode in c(FALSE, TRUE)) for (preserve in c(FALSE, TRUE)) {
    result <- item_boundary_run(f, recode, preserve)
    expect_equal(result$code_id, c(if (recode || preserve) -96 else -99, if (recode) -96 else -99, -93))
    nc <- item_boundary_row(result, "V3")
    expect_identical(nc$code_type, "NO_CODING")
    expect_identical(nc$code_status, "NO_CODING")
    expect_identical(nc$value, "Berlin")
    expect_true(is.na(nc$code_score))
  }
  # A technical status must not suppress an explicitly supplied regular type.
  f$coded$code_type[3] <- "FULL_CREDIT"
  f$coded$code_id[3] <- 1
  f$coded$code_score[3] <- 1
  expect_equal(item_boundary_run(f)$code_id, c(-96, -99, 1))
})

test_that("NO_CODING remains neutral under custom missing schemes", {
  f <- item_boundary_fixture(c("O", "NC"))
  missings <- tibble::tribble(
    ~code_id, ~code_status, ~code_score, ~code_type,
    -196, "NOT_REACHED", NA_real_, "MISSING_NOT_REACHED",
    -199, "DISPLAYED", 0, "MISSING_BY_OMISSION",
    -193, "custom technical status", 7, "NO_CODING"
  )
  result <- item_boundary_run(f, missings = missings)
  expect_equal(result$code_id, c(-196, -193))
  expect_equal(result$code_score, c(NA_real_, 7))
  expect_identical(result$code_type, c("MISSING_NOT_REACHED", "NO_CODING"))
  expect_identical(result$code_status, f$coded$code_status)
})

test_that("transitive item sources count without their own scoring metadata", {
  for (source_format in c("id", "ref", "derive", "derive_empty_list")) {
    f <- item_boundary_fixture(c("O", "A", "NR", "NR", "A"), items = c(1, 4),
                               ids = c("Earlier", "Part", "Intermediate", "Total", "Audio"))
    codes <- f$units$unit_codes[[1]]
    codes$variable_source_type[3:4] <- "COPY_VALUE"
    codes$variable_page[3:4] <- 99L
    if (source_format %in% c("derive", "derive_empty_list")) {
      codes$derive_sources <- list(character(), character(), "ref2", "ref3", character())
      if (source_format == "derive_empty_list") codes$variable_sources[3:4] <- list(list(), list())
    } else {
      column <- if (source_format == "id") "variable_source_id" else "variable_source_ref"
      for (i in 3:4) {
        source <- if (source_format == "id") codes$variable_id[i - 1L] else codes$variable_ref[i - 1L]
        codes$variable_sources[[i]] <- stats::setNames(tibble::tibble(source), column)
        codes$variable_sources[[i]]$variable_source_direct <- TRUE
      }
    }
    f$units$unit_codes[[1]] <- codes
    result <- item_boundary_capture(f)
    expect_length(result$warnings, 0L)
    expect_equal(item_boundary_row(result$result, "Earlier")$code_id, -99, info = source_format)
    expect_equal(result$result$variable_id, c("Earlier", "Part", "Intermediate", "Total", "Audio"))
    expect_identical(result$result$variable_position, 1:5)
    # Ignoring a genuine NO_CODING source also removes its boundary evidence.
    f$coded$code_type[2] <- "NO_CODING"
    f$coded$code_status[2] <- "NO_CODING"
    f$coded$code_id[2] <- -93
    f$coded$code_score[2] <- NA_real_
    f$coded$value[2] <- "Berlin"
    changed <- item_boundary_capture(f)
    expect_length(changed$warnings, 0L)
    expect_equal(item_boundary_row(changed$result, "Earlier")$code_id, -96)
    expect_identical(item_boundary_row(changed$result, "Part")$value, "Berlin")
    expect_equal(item_boundary_row(changed$result, "Part")$code_id, -93)
  }
})

test_that("source evidence uses source position rather than the final item position", {
  f <- item_boundary_fixture(c("A", "O", "NR", "NR"), items = c(2, 4),
                             ids = c("EarlyPart", "OtherItem", "LatePart", "Total"))
  codes <- f$units$unit_codes[[1]]
  codes$variable_source_type[4] <- "SOLVER"
  codes$variable_sources[[4]] <- tibble::tibble(
    variable_source_ref = c("ref1", "ref3"), variable_source_direct = TRUE
  )
  f$units$unit_codes[[1]] <- codes
  result <- item_boundary_capture(f)
  expect_length(result$warnings, 0L)
  expect_equal(item_boundary_row(result$result, "OtherItem")$code_id, -96)
  expect_identical(result$result$variable_position, 1:4)
})

test_that("item metadata fallback and column precedence match add_item_id", {
  f <- item_boundary_fixture(c("O", "A"), items = 1)
  f$units$item_metadata <- f$units$items_list
  f$units$item_metadata[[1]] <- tibble::tibble(variable_id = "V2", item_id = "second")
  expect_equal(item_boundary_run(f)$code_id, c(-96, 1))
  f$units$items_list <- NULL
  expect_equal(item_boundary_run(f)$code_id, c(-99, 1))
  f$units$item_metadata[[1]] <- tibble::tibble(variable_id = "V1", item_id = "first")
  expect_equal(item_boundary_run(f)$code_id, c(-96, 1))
  # Repeated identical entries are not an ambiguous mapping.
  f$units$item_metadata[[1]] <- f$units$item_metadata[[1]][c(1, 1), ]
  expect_length(item_boundary_capture(f)$warnings, 0L)
})

test_that("non-item results remain in the data and are still normalized", {
  f <- item_boundary_fixture(c("O", "A", "E", "NC", "NEW"), items = 1)
  f$design$variable_position <- 100L
  f$coded$variable_position <- 200L
  # Deliberately leave normalization of the non-item error to complete_design.
  f$coded$code_type[3] <- NA_character_
  f$coded$code_id[3] <- NA_real_
  original <- f
  result <- item_boundary_run(f)
  expect_equal(result$code_id, c(-96, 1, -98, -93, -96))
  expect_identical(result$code_status, f$coded$code_status)
  expect_identical(result$value, f$coded$value)
  expect_identical(result$variable_position, 1:5)
  expect_equal(nrow(result), nrow(f$design))
  expect_identical(f, original)
  expect_false(any(grepl("boundary|eligib|assessment", names(result))))
})

test_that("id_used retains evidence from a non-item technical response", {
  f <- item_boundary_fixture(c("NEW", "NC"), items = 1)
  f$coded <- f$coded[2, ]
  result <- item_boundary_run(f)
  expect_true(all(result$id_used))
  expect_true(is.na(item_boundary_row(result, "V1")$code_status))
  expect_equal(result$code_id, c(-96, -93))
})

test_that("unreliable metadata falls back for the whole unit with one warning", {
  variants <- list(
    missing_column = function(f) { f$units$items_list <- NULL; f },
    empty_items = function(f) { f$units$items_list[[1]] <- f$units$items_list[[1]][0, ]; f },
    placeholder_items = function(f) {
      f$units$items_list[[1]] <- tibble::tibble(variable_id = NA_character_, item_id = NA_character_); f
    },
    malformed_items = function(f) { f$units$items_list[[1]] <- tibble::tibble(other = "V1"); f },
    unknown_item_variable = function(f) { f$units$items_list[[1]]$variable_id <- "absent"; f },
    unknown_design_variable = function(f) {
      f$units$unit_codes[[1]] <- f$units$unit_codes[[1]][c(1, 3), ]; f
    },
    conflicting_items = function(f) {
      f$units$items_list[[1]] <- tibble::tibble(variable_id = c("V1", "V1"), item_id = c("I1", "I2")); f
    },
    unknown_source = function(f) {
      f$units$unit_codes[[1]]$variable_source_type[1] <- "COPY_VALUE"
      f$units$unit_codes[[1]]$variable_sources[[1]] <- tibble::tibble(variable_source_ref = "absent"); f
    },
    ambiguous_source = function(f) {
      f$units$unit_codes[[1]]$variable_ref[2:3] <- "ambiguous"
      f$units$unit_codes[[1]]$variable_source_type[1] <- "COPY_VALUE"
      f$units$unit_codes[[1]]$variable_sources[[1]] <- tibble::tibble(variable_source_ref = "ambiguous"); f
    },
    conflicting_source_keys = function(f) {
      f$units$unit_codes[[1]]$variable_source_type[1] <- "COPY_VALUE"
      f$units$unit_codes[[1]]$variable_sources[[1]] <- tibble::tibble(
        variable_source_id = "V3", variable_source_ref = "ref2"
      ); f
    },
    malformed_direct_flag = function(f) {
      f$units$unit_codes[[1]]$variable_source_type[1] <- "COPY_VALUE"
      f$units$unit_codes[[1]]$variable_sources[[1]] <- tibble::tibble(
        variable_source_ref = "ref2", variable_source_direct = "unknown"
      ); f
    },
    cycle = function(f) {
      f$units$unit_codes[[1]]$variable_source_type[1:2] <- "COPY_VALUE"
      f$units$unit_codes[[1]]$variable_sources[[1]] <- tibble::tibble(variable_source_ref = "ref2")
      f$units$unit_codes[[1]]$variable_sources[[2]] <- tibble::tibble(variable_source_ref = "ref1"); f
    }
  )
  for (name in names(variants)) {
    f <- variants[[name]](item_boundary_fixture(c("O", "NR", "A"), items = 1))
    result <- item_boundary_capture(f)
    expect_length(result$warnings, 1L)
    if (length(result$warnings)) {
      expect_s3_class(result$warnings[[1]], "eatPrepTBA_item_boundary_fallback")
      expect_match(conditionMessage(result$warnings[[1]]), "U", fixed = TRUE, info = name)
    }
    expect_equal(item_boundary_row(result$result, "V1")$code_id, -99, info = name)
  }
})

test_that("fallback is unit-local, aggregated, and still ignores NO_CODING", {
  good <- item_boundary_fixture(c("O", "A"), items = 1, key = "GOOD")
  bad <- item_boundary_fixture(c("O", "A"), items = integer(), key = "BAD")
  nc <- item_boundary_fixture(c("O", "NC"), items = integer(), key = "NC")
  for (i in seq_along(list(good, bad, nc))) {
    # Separate testlets make a boundary in one fixture irrelevant to the others.
    if (i == 1L) good$design$testlet_no <- i
    if (i == 2L) bad$design$testlet_no <- i
    if (i == 3L) nc$design$testlet_no <- i
  }
  f <- list(
    coded = dplyr::bind_rows(good$coded, bad$coded, nc$coded),
    units = dplyr::bind_rows(good$units, bad$units, nc$units),
    design = dplyr::bind_rows(good$design, bad$design, nc$design)
  )
  result <- item_boundary_capture(f)
  expect_length(result$warnings, 1L)
  expect_s3_class(result$warnings[[1]], "eatPrepTBA_item_boundary_fallback")
  expect_match(conditionMessage(result$warnings[[1]]), "BAD", fixed = TRUE)
  expect_match(conditionMessage(result$warnings[[1]]), "NC", fixed = TRUE)
  expect_equal(item_boundary_row(result$result, "V1", "GOOD")$code_id, -96)
  expect_equal(item_boundary_row(result$result, "V1", "BAD")$code_id, -99)
  expect_equal(item_boundary_row(result$result, "V1", "NC")$code_id, -96)
  expect_equal(item_boundary_row(result$result, "V2", "NC")$code_id, -93)
})

test_that("missing units fall back without discarding design rows", {
  f <- item_boundary_fixture(c("O", "A"), items = 1, key = "ABSENT")
  other <- item_boundary_fixture("A", key = "OTHER")
  f$units <- other$units
  result <- item_boundary_capture(f)
  expect_length(result$warnings, 1L)
  expect_s3_class(result$warnings[[1]], "eatPrepTBA_item_boundary_fallback")
  expect_match(conditionMessage(result$warnings[[1]]), "ABSENT", fixed = TRUE)
  expect_equal(nrow(result$result), 2L)
  # The legacy rule cannot locate later work within a unit without positions.
  expect_equal(result$result$code_id, c(-96, 1))
  expect_true(all(is.na(result$result$variable_position)))
})

test_that("NULL coding metadata falls back and base source placeholders are harmless", {
  f <- item_boundary_fixture(c("O", "A"), items = 1)
  f$units$unit_codes[1] <- list(NULL)
  result <- item_boundary_capture(f)
  expect_length(result$warnings, 1L)
  expect_s3_class(result$warnings[[1]], "eatPrepTBA_item_boundary_fallback")
  expect_equal(result$result$code_id, c(-96, 1))
  expect_true(all(is.na(result$result$variable_position)))
  expect_equal(nrow(result$result), 2L)

  f <- item_boundary_fixture(c("O", "A"), items = 1)
  f$units$unit_codes[[1]]$variable_sources <- rep(list(tibble::tibble(
    variable_source_id = NA_character_, variable_source_ref = NA_character_,
    variable_source_direct = NA
  )), 2)
  result <- item_boundary_capture(f)
  expect_length(result$warnings, 0L)
  expect_equal(result$result$code_id, c(-96, 1))
  expect_identical(result$result$variable_position, 1:2)
})

test_that("units consisting solely of non-items do not block earlier item units", {
  first <- item_boundary_fixture("O", key = "FIRST")
  second <- item_boundary_fixture(c("NR", "A"), items = 1, key = "SECOND")
  second$design$unit_booklet_no <- 2L
  # Only the technical variable appears in this design; its unit has valid metadata.
  second$design <- second$design[2, ]
  second$coded <- second$coded[2, ]
  f <- list(
    coded = dplyr::bind_rows(first$coded, second$coded),
    units = dplyr::bind_rows(first$units, second$units),
    design = dplyr::bind_rows(first$design, second$design)
  )
  result <- item_boundary_capture(f)
  expect_length(result$warnings, 0L)
  expect_equal(item_boundary_row(result$result, "V1", "FIRST")$code_id, -96)
  expect_equal(item_boundary_row(result$result, "V2", "SECOND")$code_id, 1)
})

test_that("factor keys and unit metadata attributes are not mutated", {
  f <- item_boundary_fixture(c("O", "A"), items = 1)
  f$design$variable_id <- factor(f$design$variable_id, levels = c("unused", "V2", "V1"))
  f$coded$variable_id <- factor(f$coded$variable_id, levels = levels(f$design$variable_id))
  f$units$unit_codes[[1]]$variable_id <- factor(c("V1", "V2"), levels = levels(f$design$variable_id))
  attr(f$units, "metadata_origin") <- list(ws = 42L, note = "keep")
  original <- f
  result <- item_boundary_run(f)
  expect_true(is.factor(result$variable_id))
  expect_identical(levels(result$variable_id), levels(f$design$variable_id))
  expect_identical(f, original)
  expect_identical(attr(f$units, "metadata_origin"), list(ws = 42L, note = "keep"))
})
