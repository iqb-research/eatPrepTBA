preserve_nr_fixture <- function(types = c("NR", "O", "A", "O", "NR")) {
  ids <- paste0("V", seq_along(types))
  codes <- tibble::tibble(
    variable_id = ids, variable_source_type = "BASE", variable_level = 0L,
    variable_page = 1L, variable_section = 1L,
    variable_page_always_visible = FALSE
  )
  design <- tibble::tibble(
    login_code = "P1", booklet_id = "B1", booklet_no = 1L,
    testlet_no = 1L, unit_booklet_no = 1L, unit_key = "U1",
    unit_alias = "first", variable_id = ids
  )
  type_map <- c(NR = "MISSING_NOT_REACHED", O = "MISSING_BY_OMISSION",
                A = "FULL_CREDIT", I = "MISSING_INVALID_RESPONSE",
                E = "MISSING_CODING_IMPOSSIBLE")
  id_map <- c(NR = -96, O = -99, A = 1, I = -98, E = -97)
  status_map <- c(NR = "NOT_REACHED", O = "DISPLAYED", A = "CODING_COMPLETE",
                  I = "INVALID", E = "CODING_ERROR")
  score_map <- c(NR = NA_real_, O = 0, A = 1, I = 0, E = NA_real_)
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

preserve_nr_complete <- function(f, preserve = FALSE, recode = FALSE, ...) {
  boundary_warnings <- character()
  out <- withCallingHandlers(suppressMessages(complete_design(
    f$coded, f$units, f$design, identifiers = "login_code",
    recode_omissions_to_not_reached = recode,
    preserve_not_reached = preserve, ...
  )), eatPrepTBA_item_boundary_fallback = function(w) {
    boundary_warnings <<- c(boundary_warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_length(boundary_warnings, 0L)
  out
}

test_that("NR preservation and trailing-omission recoding are independent options", {
  f <- preserve_nr_fixture()
  for (preserve in c(FALSE, TRUE)) {
    for (recode in c(FALSE, TRUE)) {
      out <- preserve_nr_complete(f, preserve, recode)
      expect_equal(out$code_id,
                   c(if (preserve) -96 else -99, -99, 1,
                     if (recode) -96 else -99, -96))
      expect_equal(out$code_score,
                   c(if (preserve) NA_real_ else 0, 0, 1,
                     if (recode) NA_real_ else 0, NA_real_))
      expect_identical(out$code_status, f$coded$code_status)
      expect_identical(out$variable_position, seq_len(nrow(f$design)))
      expect_identical(out$variable_id, f$design$variable_id)
      expect_false(any(grepl("^\\.", names(out))))
    }
  }
  for (recode in c(FALSE, TRUE)) {
    default <- suppressMessages(complete_design(
      f$coded, f$units, f$design, identifiers = "login_code",
      recode_omissions_to_not_reached = recode
    ))
    expect_identical(default, preserve_nr_complete(f, FALSE, recode))
  }
})

test_that("NR from analytical types, technical statuses, and completed rows is protected", {
  f <- preserve_nr_fixture(c(rep("NR", 6), "A"))
  f$coded$code_status <- c("NOT_REACHED", "DISPLAYED", NA_character_,
                          "NOT_REACHED", NA_character_, "NOT_REACHED",
                          "CODING_COMPLETE")
  f$coded$code_type[4:5] <- NA_character_
  f$coded$code_id[4:5] <- NA_real_
  f$coded <- f$coded[-6, ]
  expected_status <- c(f$coded$code_status[1:5], NA_character_, "CODING_COMPLETE")
  for (recode in c(FALSE, TRUE)) {
    out <- preserve_nr_complete(f, TRUE, recode)
    expect_equal(out$code_id, c(rep(-96, 6), 1))
    expect_equal(out$code_type, c(rep("MISSING_NOT_REACHED", 6), "FULL_CREDIT"))
    expect_equal(out$code_score, c(rep(NA_real_, 6), 1))
    expect_identical(out$code_status, expected_status)
    expect_equal(preserve_nr_complete(f, FALSE, recode)$code_id,
                 c(rep(-99, 6), 1))
  }
})

test_that("preservation applies across units without changing testlet boundaries", {
  f <- preserve_nr_fixture(c("NR", "A", "NR", "A"))
  f$design$unit_key <- paste0("U", 1:4)
  f$design$unit_alias <- paste0("occurrence", 1:4)
  f$design$unit_booklet_no <- 1:4
  f$design$testlet_no <- c(1L, 1L, 2L, 3L)
  f$coded$unit_key <- f$design$unit_key
  f$coded$unit_alias <- f$design$unit_alias
  f$units <- tibble::tibble(
    unit_key = f$design$unit_key,
    unit_codes = lapply(1:4, function(i) f$units$unit_codes[[1]][i, ]),
    items_list = lapply(1:4, function(i) f$units$items_list[[1]][i, ])
  )
  for (recode in c(FALSE, TRUE)) {
    ordinary <- preserve_nr_complete(f, FALSE, recode)
    protected <- preserve_nr_complete(f, TRUE, recode)
    expect_equal(ordinary$code_id, c(-99, 1, -96, 1))
    expect_equal(protected$code_id, c(-96, 1, -96, 1))
    expect_identical(protected$code_status, ordinary$code_status)
    expect_identical(protected$variable_position, rep(1L, 4))
  }
})

test_that("a later derived result does not overwrite protected NR sources", {
  f <- preserve_nr_fixture(c("NR", "A"))
  codes <- f$units$unit_codes[[1]]
  codes$variable_source_type[2] <- "SUM_CODE"
  codes$variable_level[2] <- 1L
  codes$variable_sources <- list(
    tibble::tibble(), tibble::tibble(variable_source_id = "V1")
  )
  # The dependency overrides this earlier physical page of the derived result.
  codes$variable_page <- c(2L, 1L)
  f$units$unit_codes[[1]] <- codes
  for (recode in c(FALSE, TRUE)) {
    ordinary <- preserve_nr_complete(f, FALSE, recode)
    protected <- preserve_nr_complete(f, TRUE, recode)
    expect_equal(ordinary$code_id, c(-99, 1))
    expect_equal(protected$code_id, c(-96, 1))
    expect_identical(protected$variable_id, c("V1", "V2"))
    expect_identical(protected$variable_position, ordinary$variable_position)
    expect_identical(protected$code_status, ordinary$code_status)
  }
})

test_that("derived NR retains its classification and source-based position", {
  f <- preserve_nr_fixture(c("NR", "NR", "A"))
  codes <- f$units$unit_codes[[1]]
  codes$variable_source_type[2] <- "SUM_CODE"
  codes$variable_sources <- list(
    tibble::tibble(), tibble::tibble(variable_source_id = "V1"), tibble::tibble()
  )
  codes$variable_page <- c(2L, 1L, 3L)
  f$units$unit_codes[[1]] <- codes
  f$coded$code_status[2] <- "DISPLAYED"
  for (recode in c(FALSE, TRUE)) {
    out <- preserve_nr_complete(f, TRUE, recode)
    expect_equal(out$code_id, c(-96, -96, 1))
    expect_identical(out$variable_position, 1:3)
    expect_identical(out$code_status, f$coded$code_status)
    expect_equal(preserve_nr_complete(f, FALSE, recode)$code_id, c(-99, -99, 1))
  }
})

test_that("existing analytical results take precedence over NOT_REACHED status", {
  f <- preserve_nr_fixture(c("A", "I", "E", "O", "A"))
  f$coded$code_status[1:4] <- "NOT_REACHED"
  for (recode in c(FALSE, TRUE)) {
    expect_identical(preserve_nr_complete(f, TRUE, recode),
                     preserve_nr_complete(f, FALSE, recode))
  }
  out <- preserve_nr_complete(f, TRUE)
  expect_equal(out$code_id, c(1, -98, -97, -99, 1))
  expect_identical(out$code_status, f$coded$code_status)
})

test_that("custom missing codes and scores apply to protected analytical NR", {
  f <- preserve_nr_fixture(c("NR", "A", "O"))
  f$coded$code_id[1] <- -196
  f$coded$code_status[1] <- "DISPLAYED"
  missings <- tibble::tribble(
    ~code_id, ~code_status, ~code_score, ~code_type,
    -196, "CUSTOM_NR", 0.5, "MISSING_NOT_REACHED",
    -199, "CUSTOM_O", 0.25, "MISSING_BY_OMISSION"
  )
  for (recode in c(FALSE, TRUE)) {
    out <- preserve_nr_complete(f, TRUE, recode, missings = missings)
    expect_equal(out$code_id, c(-196, 1, if (recode) -196 else -199))
    expect_equal(out$code_score, c(0.5, 1, if (recode) 0.5 else 0.25))
    expect_identical(out$code_status, f$coded$code_status)
    expect_equal(preserve_nr_complete(f, FALSE, recode, missings = missings)$code_id,
                 c(-199, 1, if (recode) -196 else -199))
  }
})

test_that("numeric-only NR uses the configured missing code without overriding a known type", {
  f <- preserve_nr_fixture(c("O", "A"))
  f$coded$code_type[1] <- NA_character_
  f$coded$code_id[1] <- -96
  expect_equal(preserve_nr_complete(f, TRUE)$code_id, c(-96, 1))
  expect_equal(preserve_nr_complete(f)$code_id, c(-99, 1))

  f <- preserve_nr_fixture(c("O", "O", "O", "A"))
  f$coded$code_type[1:2] <- NA_character_
  f$coded$code_id[1:3] <- c(-196, -96, -196)
  missings <- tibble::tribble(
    ~code_id, ~code_status, ~code_score, ~code_type,
    -196, "CUSTOM_NR", NA_real_, "MISSING_NOT_REACHED",
    -96, "CUSTOM_O", 0, "MISSING_BY_OMISSION"
  )
  for (recode in c(FALSE, TRUE)) {
    out <- preserve_nr_complete(f, TRUE, recode, missings = missings)
    expect_equal(out$code_id, c(-196, -96, -96, 1))
    expect_equal(out$code_type, c("MISSING_NOT_REACHED", rep("MISSING_BY_OMISSION", 2),
                                 "FULL_CREDIT"))
    expect_equal(out$code_score, c(NA_real_, 0, 0, 1))
    expect_identical(out$code_status, f$coded$code_status)
  }

  f <- preserve_nr_fixture(c("O", "A"))
  f$coded$code_type[1] <- NA_character_
  f$coded$code_id[1] <- -96
  # A partial schema without an NR entry retains the standard numeric fallback.
  partial <- dplyr::filter(missings, code_type == "MISSING_BY_OMISSION")
  partial$code_id <- -199
  out <- preserve_nr_complete(f, TRUE, missings = partial)
  expect_equal(out$code_id, c(-96, 1))
  expect_equal(out$code_type, c("MISSING_NOT_REACHED", "FULL_CREDIT"))
})

test_that("preserve_not_reached requires one nonmissing logical value", {
  f <- preserve_nr_fixture()
  for (invalid in list(NA, logical(), c(TRUE, FALSE), 1, "TRUE", list(TRUE))) {
    expect_error(preserve_nr_complete(f, preserve = invalid))
  }
})
