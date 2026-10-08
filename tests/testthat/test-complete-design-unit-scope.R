unit_scope_fixture <- function() {
  units <- minimal_units()
  design <- tibble::tibble(
    login_code = "P1", booklet_id = "B1", booklet_no = 1L,
    testlet_no = 1L, unit_booklet_no = 1L,
    unit_key = "U1", unit_alias = "first", variable_id = "V1"
  )
  coded <- dplyr::mutate(
    design, code_status = "CODING_COMPLETE", value = "A",
    code_id = 1, code_type = "FULL_CREDIT", code_score = 1
  )
  list(units = units, design = design, coded = coded)
}

unit_scope_call <- function(fixture, units = fixture$units,
                            design = fixture$design, policy = "eatPrepTBA",
                            recode = FALSE, overwrite = FALSE) {
  complete_design(
    fixture$coded, units, design, identifiers = "login_code",
    missing_policy = policy, not_reached_scope = "testlet",
    recode_omissions_to_not_reached = recode,
    overwrite = overwrite, diagnostics = "none"
  )
}

unit_scope_unused <- function(fixture) {
  dplyr::mutate(fixture$units, unit_key = "UNUSED", unit_id = 99)
}

test_that("unused malformed prepared coding metadata cannot block any completion mode", {
  f <- unit_scope_fixture()
  malformed <- unit_scope_unused(f)
  malformed$unit_codes[[1]] <- tibble::tibble(unrelated = "broken")

  ambiguous <- unit_scope_unused(f)
  ambiguous$unit_codes[[1]] <- dplyr::bind_rows(
    minimal_unit_codes(),
    dplyr::mutate(minimal_unit_codes(), variable_id = "V2")
  )

  for (policy in c("eatPrepTBA", "coding_box")) {
    for (recode in list(NULL, FALSE, TRUE)) {
      expected <- unit_scope_call(f, policy = policy, recode = recode)
      for (bad in list(malformed, ambiguous)) {
        supplied <- dplyr::bind_rows(f$units, bad)
        actual <- unit_scope_call(f, supplied, policy = policy, recode = recode)
        expect_equal(actual, expected)
      }
    }
  }

  for (recode in list(NULL, FALSE, TRUE)) {
    used_bad <- dplyr::mutate(malformed, unit_key = "U1")
    expect_error(unit_scope_call(f, used_bad, recode = recode), "variable_id")
    used_ambiguous <- dplyr::mutate(ambiguous, unit_key = "U1")
    expect_error(unit_scope_call(f, used_ambiguous, recode = recode),
                 "ambiguous")
  }
})

test_that("unused raw schemas are excluded before preparation and overwrite", {
  f <- unit_scope_fixture()
  malformed <- unit_scope_unused(f)
  malformed$coding_scheme <- "{ this is not a coding scheme"

  for (policy in c("eatPrepTBA", "coding_box")) {
    for (recode in list(NULL, FALSE, TRUE)) {
      for (overwrite in c(FALSE, TRUE)) {
        good <- f$units
        bad <- malformed
        if (!overwrite) {
          good$unit_codes <- NULL
          bad$unit_codes <- NULL
        }
        expected <- unit_scope_call(f, good, policy = policy, recode = recode,
                                     overwrite = overwrite)
        actual <- unit_scope_call(f, dplyr::bind_rows(good, bad),
                                   policy = policy, recode = recode,
                                   overwrite = overwrite)
        expect_equal(actual, expected)
      }
    }
  }
  used_bad <- dplyr::mutate(malformed, unit_key = "U1")
  expect_error(unit_scope_call(f, used_bad, overwrite = TRUE))
})

test_that("VOMD validation and unresolved diagnostics concern only design units", {
  f <- unit_scope_fixture()
  bad_position <- unit_scope_unused(f)
  bad_position$items_list[[1]]$item_no <- -1L
  unmapped <- unit_scope_unused(f)
  unmapped$items_list[[1]]$variable_id <- "UNKNOWN"
  unmapped$items_list[[1]]$variable_ref <- "unknown"

  for (policy in c("eatPrepTBA", "coding_box")) {
    for (recode in list(NULL, FALSE, TRUE)) {
      expected <- unit_scope_call(f, policy = policy, recode = recode)
      for (bad in list(bad_position, unmapped)) {
        actual <- unit_scope_call(f, dplyr::bind_rows(f$units, bad),
                                   policy = policy, recode = recode)
        expect_equal(actual, expected)
      }
    }
    for (recode in list(FALSE, TRUE)) {
      used_bad <- dplyr::mutate(bad_position, unit_key = "U1")
      expect_error(unit_scope_call(f, used_bad, policy = policy, recode = recode),
                   "Invalid VOMD.*item_no")
    }
  }
  used_unmapped <- dplyr::mutate(unmapped, unit_key = "U1")
  expect_error(unit_scope_call(f, used_unmapped, policy = "coding_box"),
               "VOMD|mapped|resolve")
})

test_that("empty designs need no unused raw unit preparation", {
  f <- unit_scope_fixture()
  f$coded <- f$coded[0, ]
  f$design <- f$design[0, ]
  unused <- tibble::tibble(unit_key = "UNUSED", coding_scheme = "broken")
  for (policy in c("eatPrepTBA", "coding_box")) {
    for (recode in list(NULL, FALSE, TRUE)) {
      for (overwrite in c(FALSE, TRUE)) {
        actual <- unit_scope_call(f, unused, policy = policy, recode = recode,
                                   overwrite = overwrite)
        expect_equal(nrow(actual), 0L)
        expect_type(actual$response_present, "logical")
        expect_true(all(c("variable_id", "code_status", "code_id",
                          "code_score", "code_type") %in% names(actual)))
        if (!is.null(recode)) expect_type(actual$variable_order, "integer")
      }
    }
  }
})

test_that("design-only units and all their active variables remain in scope", {
  f <- unit_scope_fixture()
  second <- dplyr::mutate(f$units, unit_key = "U2", unit_id = 11)
  second$unit_codes[[1]] <- dplyr::bind_rows(
    minimal_unit_codes(),
    dplyr::mutate(minimal_unit_codes(), variable_id = "V2", variable_ref = "v2",
                  variable_page = 2)
  )
  second$items_list[[1]] <- dplyr::bind_rows(
    second$items_list[[1]],
    tibble::tibble(item_no = 2L, item_id = "I2", variable_id = "V2",
                   variable_ref = "v2")
  )
  second_design <- dplyr::mutate(f$design, unit_key = "U2", unit_alias = "second",
                                unit_booklet_no = 2L)
  design <- dplyr::bind_rows(f$design, second_design)
  units <- dplyr::bind_rows(f$units, second)
  bad <- unit_scope_unused(f)
  bad$unit_codes[[1]] <- tibble::tibble(unrelated = "broken")
  units <- dplyr::bind_rows(units, bad)

  for (policy in c("eatPrepTBA", "coding_box")) {
    for (recode in list(NULL, FALSE, TRUE)) {
      for (variable_subset in c(FALSE, TRUE)) {
        expected_design <- design
        if (!variable_subset) expected_design$variable_id <- NULL
        actual <- unit_scope_call(f, units, expected_design,
                                   policy = policy, recode = recode)
        expect_equal(nrow(actual), 3L)
        added <- actual[actual$unit_key == "U2", ]
        expect_setequal(added$variable_id, c("V1", "V2"))
        expect_false(any(added$response_present))
        expect_true(all(is.na(added$code_status)))
        if (is.null(recode)) {
          expect_true(all(is.na(added$code_id)))
          expect_true(all(is.na(added$code_type)))
        } else {
          expect_equal(added$code_type, rep("MISSING_NOT_REACHED", 2))
          expect_equal(added$code_id, rep(-96, 2))
          expect_true(all(is.na(added$code_score)))
        }
      }
    }
  }
})

test_that("missing used metadata and conflicting used versions still fail", {
  f <- unit_scope_fixture()
  second_design <- dplyr::mutate(f$design, unit_key = "U2", unit_alias = "second",
                                unit_booklet_no = 2L)
  design <- dplyr::bind_rows(f$design, second_design)
  conflict <- f$units
  conflict$unit_id <- 11
  conflict$unit_codes[[1]]$variable_ref <- "conflicting-v1"
  for (policy in c("eatPrepTBA", "coding_box")) {
    for (recode in list(NULL, FALSE, TRUE)) {
      expect_error(unit_scope_call(f, design = design, policy = policy,
                                   recode = recode), "U2")
      expect_error(unit_scope_call(f, dplyr::bind_rows(f$units, conflict),
                                   policy = policy, recode = recode),
                   "Conflicting|conflicting|version|duplicate")
    }
  }
})

test_that("internal ordering scopes raw and flat prepared unit metadata", {
  f <- unit_scope_fixture()
  malformed <- unit_scope_unused(f)
  malformed$unit_codes[[1]] <- tibble::tibble(unrelated = "broken")
  expected <- eatPrepTBA:::get_design_order(f$design, f$units,
                                           order_method = "hybrid")
  actual <- eatPrepTBA:::get_design_order(
    f$design, dplyr::bind_rows(f$units, malformed), order_method = "hybrid"
  )
  expect_equal(actual, expected)

  prepared <- eatPrepTBA:::design_order_metadata(f$units)
  unused <- dplyr::mutate(prepared, unit_key = "UNUSED")
  unused <- dplyr::bind_rows(
    unused, dplyr::mutate(unused, variable_ref = "conflicting-v1")
  )
  expected <- eatPrepTBA:::get_design_order(f$design, prepared,
                                           order_method = "structure")
  actual <- eatPrepTBA:::get_design_order(
    f$design, dplyr::bind_rows(prepared, unused), order_method = "structure"
  )
  expect_equal(actual, expected)
})

test_that("internal classification ignores metadata outside its complete data scope", {
  f <- unit_scope_fixture()
  data <- unit_scope_call(f, recode = NULL)
  malformed <- unit_scope_unused(f)
  malformed$unit_codes[[1]] <- tibble::tibble(unrelated = "broken")
  for (policy in c("eatPrepTBA", "coding_box")) {
    method <- if (policy == "coding_box") "vomd" else "hybrid"
    positions <- eatPrepTBA:::get_design_order(f$design, f$units,
                                               order_method = method)
    for (supplied_positions in list(NULL, positions)) {
      expected <- eatPrepTBA:::recode_missings_impl(
        data, f$units, positions = supplied_positions, identifiers = "login_code",
        missing_policy = policy
      )
      actual <- eatPrepTBA:::recode_missings_impl(
        data, dplyr::bind_rows(f$units, malformed), positions = supplied_positions,
        identifiers = "login_code", missing_policy = policy
      )
      expect_equal(actual, expected)
    }
  }
})
