variable_metadata_scheme <- function(extra_id = "V_OFF", extra_type = "BASE_NO_VALUE") {
  parsed <- jsonlite::parse_json(minimal_coding_scheme())
  extra <- parsed$variableCodings[[1L]]
  extra$id <- tolower(extra_id)
  extra$alias <- extra_id
  extra$label <- paste("Variable", extra_id)
  extra$sourceType <- extra_type
  extra$codes <- list()
  parsed$variableCodings[[2L]] <- extra
  as.character(jsonlite::toJSON(parsed, auto_unbox = TRUE, null = "null"))
}

variable_metadata_fixture <- function(cache = "all") {
  units <- minimal_units()
  units$coding_scheme <- variable_metadata_scheme()
  inactive <- dplyr::mutate(
    minimal_unit_codes(), variable_id = "V_OFF", variable_ref = "v_off",
    variable_source_type = "BASE_NO_VALUE", variable_label = "Inactive"
  )
  units$unit_codes[[1L]] <- dplyr::bind_rows(minimal_unit_codes(), inactive)
  if (cache == "active") units$unit_codes[[1L]] <- minimal_unit_codes()
  if (cache == "none") units$unit_codes <- NULL
  design <- tibble::tibble(
    login_code = c("P1", "P1", "P2", "P1", "P2"), booklet_id = "B1",
    booklet_no = 1L, testlet_no = c(1L, 1L, 1L, 2L, 2L),
    unit_booklet_no = c(1L, 1L, 1L, 2L, 2L), unit_key = "U1",
    unit_alias = c("first", "first", "first", "second", "second"),
    variable_id = c("V1", "V_OFF", "V_OFF", "V_OFF", "V_OFF"),
    booklet_label = "Booklet 1",
    testlet_label = c("Testlet 1", "Testlet 1", "Testlet 1", "Testlet 2", "Testlet 2"),
    session_label = c("Session 1", "Session 1", "Session 1", "Session 2", "Session 2"),
    variable_note = c("Active variable", rep("Inactive variable", 4L))
  )
  coded <- dplyr::mutate(
    design[1L, ], code_status = "CODING_COMPLETE", value = "A",
    code_id = 1, code_type = "FULL_CREDIT", code_score = 1
  )
  list(units = units, design = design, coded = coded)
}

variable_metadata_call <- function(fixture, policy = "eatPrepTBA", recode = FALSE,
                                   overwrite = FALSE) {
  complete_design(
    fixture$coded, fixture$units, fixture$design, identifiers = "login_code",
    missing_policy = policy, not_reached_scope = "testlet",
    recode_omissions_to_not_reached = recode,
    overwrite = overwrite, diagnostics = "none"
  )
}

variable_metadata_warnings <- function(expr) {
  warnings <- list()
  value <- withCallingHandlers(expr, warning = function(condition) {
    warnings[[length(warnings) + 1L]] <<- condition
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warnings)
}

test_that("confirmed inactive variables are excluded while their occurrences survive", {
  for (cache in c("all", "active", "none")) {
    f <- variable_metadata_fixture(cache)
    for (policy in c("eatPrepTBA", "coding_box")) {
      for (recode in list(NULL, FALSE, TRUE)) {
        captured <- variable_metadata_warnings(variable_metadata_call(
          f, policy = policy, recode = recode
        ))
        expect_length(captured$warnings, 1L)
        expect_match(conditionMessage(captured$warnings[[1L]]), "U1/V_OFF", fixed = TRUE)
        expect_equal(captured$warnings[[1L]]$variables,
                     tibble::tibble(unit_key = "U1", variable_id = "V_OFF"))
        out <- captured$value
        expect_equal(nrow(out), 4L)
        expect_equal(out$variable_id, rep("V1", 4L))
        expect_setequal(paste(out$login_code, out$unit_alias),
                        c("P1 first", "P2 first", "P1 second", "P2 second"))
        expect_equal(out$booklet_label, rep("Booklet 1", 4L))
        expect_equal(out$testlet_label[out$unit_alias == "second"], rep("Testlet 2", 2L))
        expect_equal(out$session_label[out$unit_alias == "second"], rep("Session 2", 2L))
        supplied <- out$login_code == "P1" & out$unit_alias == "first"
        expect_equal(out$variable_note[supplied], "Active variable")
        expect_true(all(is.na(out$variable_note[!supplied])))
        expect_equal(out$response_present[supplied], TRUE)
        expect_false(any(out$response_present[!supplied]))
        expect_equal(out$code_status[supplied], "CODING_COMPLETE")
        expect_true(all(is.na(out$code_status[!supplied])))
        expect_equal(out$code_id[supplied], 1)
        expect_equal(out$code_score[supplied], 1)
        if (is.null(recode)) {
          expect_true(all(is.na(out$code_id[!supplied])))
          expect_true(all(is.na(out$code_type[!supplied])))
        } else {
          expect_true(all(out$code_type[!supplied] == "MISSING_NOT_REACHED"))
          expect_true(all(out$code_id[!supplied] == -96))
          expect_true(all(is.na(out$code_score[!supplied])))
        }
      }
    }
  }
})

test_that("inactive response rows never become active completed responses", {
  f <- variable_metadata_fixture()
  inactive_response <- dplyr::mutate(
    f$coded, variable_id = "V_OFF", value = "Unused", code_id = 99,
    code_type = "RESIDUAL", code_score = 0
  )
  f$coded <- dplyr::bind_rows(f$coded, inactive_response)
  out <- variable_metadata_warnings(variable_metadata_call(f))$value
  expect_false("V_OFF" %in% out$variable_id)
  expect_equal(sum(out$response_present), 1L)
  expect_equal(out$code_id[out$response_present], 1)
})

test_that("inactive VOMD references block Box classification but not completion", {
  f <- variable_metadata_fixture()
  f$units$items_list[[1L]] <- dplyr::bind_rows(
    f$units$items_list[[1L]],
    tibble::tibble(item_no = 2L, item_id = "I_OFF",
                   variable_id = "V_OFF", variable_ref = "v_off")
  )
  for (policy in c("eatPrepTBA", "coding_box")) {
    for (recode in list(NULL, FALSE, TRUE)) {
      captured <- variable_metadata_warnings(tryCatch(
        variable_metadata_call(f, policy = policy, recode = recode),
        error = identity
      ))
      expect_length(captured$warnings, 1L)
      expect_s3_class(captured$warnings[[1L]], "eatPrepTBA_inactive_design_variables")
      out <- captured$value
      if (policy == "coding_box" && !is.null(recode)) {
        expect_s3_class(out, "error")
        expect_match(conditionMessage(out),
                     "coding_box policy cannot resolve 1 VOMD item mappings", fixed = TRUE)
      } else {
        expect_equal(out$variable_id, rep("V1", 4L))
        expect_equal(sum(out$response_present), 1L)
        expect_equal(out$code_id[out$response_present], 1)
        if (is.null(recode)) {
          expect_true(all(is.na(out$code_type[!out$response_present])))
          expect_null(attr(out, "vomd_unresolved"))
        } else {
          expect_true(all(out$code_type[!out$response_present] == "MISSING_NOT_REACHED"))
          expect_equal(attr(out, "vomd_unresolved"), tibble::tibble(
            unit_key = "U1", item_id = "I_OFF", variable_id = "V_OFF",
            variable_ref = "v_off", item_position = 2
          ))
        }
      }
    }
  }
})

test_that("BASE variables without numerical codes remain active", {
  f <- variable_metadata_fixture()
  f$design <- f$design[1L, ]
  f$units$unit_codes[[1L]] <- minimal_unit_codes()
  f$units$unit_codes[[1L]]$variable_codes[[1L]] <- tibble::tibble(
    code_id = integer(), code_type = character(), code_score = double()
  )
  parsed <- jsonlite::parse_json(minimal_coding_scheme())
  parsed$variableCodings[[1L]]$codes <- list()
  f$units$coding_scheme <- as.character(jsonlite::toJSON(
    parsed, auto_unbox = TRUE, null = "null"
  ))
  for (policy in c("eatPrepTBA", "coding_box")) {
    for (recode in list(NULL, FALSE, TRUE)) {
      captured <- variable_metadata_warnings(variable_metadata_call(
        f, policy = policy, recode = recode
      ))
      expect_length(captured$warnings, 0L)
      expect_equal(captured$value$variable_id, "V1")
      expect_equal(captured$value$code_status, "CODING_COMPLETE")
      expect_equal(captured$value$code_id, 1)
    }
  }
})

test_that("existing active metadata wins over contrary inactive raw definitions", {
  f <- variable_metadata_fixture("active")
  f$design <- f$design[1L, ]
  parsed <- jsonlite::parse_json(minimal_coding_scheme())
  parsed$variableCodings[[1L]]$sourceType <- "BASE_NO_VALUE"
  f$units$coding_scheme <- as.character(jsonlite::toJSON(
    parsed, auto_unbox = TRUE, null = "null"
  ))
  for (policy in c("eatPrepTBA", "coding_box")) {
    captured <- variable_metadata_warnings(variable_metadata_call(f, policy = policy))
    expect_length(captured$warnings, 0L)
    expect_equal(captured$value$variable_id, "V1")
    expect_equal(captured$value$code_id, 1)
  }
})

test_that("unknown design variables report distinct pairs and actionable repairs", {
  f <- variable_metadata_fixture("active")
  f$design <- dplyr::bind_rows(
    f$design[1L, ],
    dplyr::mutate(f$design[1L, ], variable_id = "TYPO"),
    dplyr::mutate(f$design[1L, ], login_code = "P2", variable_id = "TYPO"),
    dplyr::mutate(f$design[1L, ], variable_id = "v1")
  )
  for (policy in c("eatPrepTBA", "coding_box")) {
    for (recode in list(NULL, FALSE, TRUE)) {
      error <- tryCatch(variable_metadata_call(f, policy = policy, recode = recode),
                        error = identity)
      expect_s3_class(error, "error")
      expect_match(conditionMessage(error), "U1/TYPO", fixed = TRUE)
      expect_match(conditionMessage(error), "overwrite", ignore.case = TRUE)
      expect_match(conditionMessage(error), "design", ignore.case = TRUE)
      expect_s3_class(error$variables, "tbl_df")
      expect_equal(error$variables, tibble::tibble(
        unit_key = c("U1", "U1"), variable_id = c("TYPO", "v1")
      ))
      expect_equal(error$available_variables,
                   tibble::tibble(unit_key = "U1", variable_id = "V1"))
    }
  }
})

test_that("an unusable raw scheme cannot prove that an unknown alias is inactive", {
  for (raw_scheme in c("malformed raw JSON", NA_character_, "")) {
    f <- variable_metadata_fixture("active")
    f$units$coding_scheme <- raw_scheme
    error <- tryCatch(variable_metadata_call(f), error = identity)
    expect_s3_class(error, "eatPrepTBA_design_metadata_error")
    expect_match(conditionMessage(error), "U1/V_OFF", fixed = TRUE)
    expect_equal(error$variables,
                 tibble::tibble(unit_key = "U1", variable_id = "V_OFF"))
  }
})

test_that("an active variable omitted by stale cache is not treated as inactive", {
  f <- variable_metadata_fixture("active")
  f$units$coding_scheme <- variable_metadata_scheme("V2", "BASE")
  f$design <- dplyr::mutate(f$design[1L, ], variable_id = "V2")
  error <- tryCatch(variable_metadata_call(f), error = identity)
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "U1/V2", fixed = TRUE)
  expect_match(conditionMessage(error), "overwrite", ignore.case = TRUE)
  expect_equal(error$variables,
               tibble::tibble(unit_key = "U1", variable_id = "V2"))
  rebuilt <- variable_metadata_warnings(variable_metadata_call(f, overwrite = TRUE))
  expect_length(rebuilt$warnings, 0L)
  expect_setequal(rebuilt$value$variable_id, c("V1", "V2"))
})

test_that("a used unit with no active variables still fails", {
  f <- variable_metadata_fixture()
  f$units$unit_codes[[1L]] <- f$units$unit_codes[[1L]][2L, ]
  f$design <- f$design[2L, ]
  expect_error(variable_metadata_call(f), "No active variable metadata.*U1")
})

test_that("conflicting active metadata cannot be hidden by inactive design rows", {
  f <- variable_metadata_fixture()
  conflict <- dplyr::mutate(minimal_unit_codes(), variable_ref = "conflicting-v1")
  f$units$unit_codes[[1L]] <- dplyr::bind_rows(f$units$unit_codes[[1L]], conflict)
  expect_error(variable_metadata_call(f), "Conflicting|conflicting")
})

test_that("get_design excludes cached inactive variables without rebuilding codes", {
  testthat::local_mocked_bindings(
    get_testtakers = function(workspace, files = NULL) {
      tibble::tibble(
        group_id = "G1", login_name = "L1", login_mode = "run-hot-return",
        login_code = "P1", booklet_id = "B1", booklet_no = 1L
      )
    },
    get_booklets = function(workspace, files = NULL) {
      tibble::tibble(
        booklet_id = "B1", booklet_label = "Booklet 1", testlet_no = 1L,
        unit_key = "U1", unit_alias = "first", unit_booklet_no = 1L
      )
    },
    .package = "eatPrepTBA"
  )
  f <- variable_metadata_fixture()
  f$units$coding_scheme <- "An unused raw scheme must not be parsed"
  out <- suppressMessages(get_design(fake_testcenter_workspace(), units = f$units))
  expect_equal(out$unit_key, "U1")
  expect_equal(out$variable_id, "V1")
  expect_equal(out$booklet_label, "Booklet 1")
})
