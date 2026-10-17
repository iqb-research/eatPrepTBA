test_that("the real coder transports required true, false and null provenance", {
  scheme <- autocoder_contract_scheme(list(autocoder_contract_variable("a")))
  input <- tibble::tibble(
    case = c("nr", "worked", "unknown"),
    responses = c(
      '[{"id":"a","status":"NOT_REACHED","value":false}]',
      '[{"id":"A","status":"VALUE_CHANGED","value":false}]',
      '[{"id":"A","status":null,"value":false}]'
    )
  )
  out <- autocoder_contract_compare(scheme, input)
  expect_identical(out$id, rep("A", 3L))
  expect_identical(autocoder_contract_flags(out), c(TRUE, FALSE, NA))
  expect_identical(out$case, input$case)
})

test_that("the real coder preserves aliases and separate subform provenance", {
  scheme <- autocoder_contract_scheme(list(autocoder_contract_variable("a")))
  input <- tibble::tibble(responses = paste0(
    '[{"id":"a","subform":"one","status":"NOT_REACHED","value":2},',
    '{"id":"A","subform":"two","status":"VALUE_CHANGED","value":3}]'
  ))
  out <- autocoder_contract_compare(scheme, input)
  flags <- autocoder_contract_flags(out)
  supplied <- !is.na(out$subform)
  expect_identical(out$id[supplied], c("A", "A"))
  expect_identical(out$subform[supplied], c("one", "two"))
  expect_identical(flags[supplied], c(TRUE, FALSE))
  # The coder may also generate a scheme-level record without a subform.
  expect_true(all(is.na(flags[!supplied])))
})

test_that("geometry and manual overlays retain absent versus null status semantics", {
  scheme <- autocoder_contract_scheme(list(autocoder_contract_variable("a")))
  for (column in c("geometry_variables", "manual")) {
    input <- tibble::tibble(
      case = c("inherit", "unknown", "worked", "nr"),
      responses = rep('[{"id":"A","status":"NOT_REACHED","value":2}]', 4L)
    )
    input[[column]] <- c(
      '[{"id":"A","value":3,"coding_from_not_reached":false}]',
      '[{"id":"A","status":null,"value":3,"coding_from_not_reached":true}]',
      '[{"id":"A","status":"CODING_COMPLETE","code":1,"score":1,"coding_from_not_reached":true}]',
      '[{"id":"A","status":"NOT_REACHED","value":3,"coding_from_not_reached":false}]'
    )
    out <- autocoder_contract_compare(scheme, input)
    expect_identical(autocoder_contract_flags(out), c(TRUE, NA, FALSE, TRUE))
    expect_identical(out$code[3L], 1L)
    expect_equal(out$score[3L], 1)
  }
})

test_that("derived types retain the existing evidence policy for NR and mixed sources", {
  rlang::local_options(lifecycle_verbosity = "error")
  for (type in c("COPY_VALUE", "SUM_CODE", "SUM_SCORE", "CONCAT_CODE", "UNIQUE_VALUES", "SOLVER")) {
    fixture <- autocoder_contract_fixture(type)
    for (mixed in c(FALSE, TRUE)) {
      responses <- autocoder_contract_responses(mixed)
      # Annotation must not alter analytical outputs of the installed coder.
      autocoder_contract_compare(fixture$units$coding_scheme, responses)
      coded <- suppressMessages(code_responses(responses, fixture$units, prepare = TRUE))
      expect_identical(coded$coding_from_not_reached[coded$variable_id == "A"], !mixed)
      expect_identical(coded$coding_from_not_reached[coded$variable_id == "B"], TRUE)
      derived <- coded[coded$variable_id == "Derived", ]
      expect_equal(nrow(derived), 1L, info = type)
      expect_identical(derived$code_status, "CODING_COMPLETE", info = type)
      expect_equal(derived$code_id, 0L, info = type)
      expect_identical(derived$coding_from_not_reached, NA, info = type)
      for (recode in c(FALSE, TRUE)) {
        for (preserve in c(FALSE, TRUE)) {
          out <- autocoder_contract_complete(coded, fixture$units, recode, preserve)
          expect_equal(out$code_id[out$variable_id == "Earlier"], -99, info = type)
          expect_equal(out$code_score[out$variable_id == "Earlier"], 0, info = type)
          result <- out[out$variable_id == "Derived", names(derived)]
          expect_equal(result, derived, info = type)
          control <- autocoder_contract_complete(coded, fixture$units, recode, preserve,
                                                  include_derived = FALSE)
          expect_equal(control$code_id[control$variable_id == "Earlier"],
                       if (recode && !mixed) -96 else -99, info = type)
        }
      }
    }
  }
})

test_that("multi-stage derivations and existing NR keep their documented boundaries", {
  rlang::local_options(lifecycle_verbosity = "error")
  fixture <- autocoder_contract_fixture("SUM_CODE", chain = TRUE)
  for (mixed in c(FALSE, TRUE)) {
    coded <- suppressMessages(code_responses(
      autocoder_contract_responses(mixed, earlier = "NOT_REACHED"),
      fixture$units, prepare = TRUE
    ))
    generated <- coded$variable_id %in% c("Middle", "Derived")
    expect_true(all(is.na(coded$coding_from_not_reached[generated])))
    expect_true(all(coded$code_status[generated] == "CODING_COMPLETE"))
    for (recode in c(FALSE, TRUE)) {
      for (preserve in c(FALSE, TRUE)) {
        out <- autocoder_contract_complete(coded, fixture$units, recode, preserve)
        expect_equal(out$code_id[out$variable_id == "Earlier"], if (preserve) -96 else -99)
        expect_equal(out$code_score[out$variable_id == "Earlier"], if (preserve) NA_real_ else 0)
      }
    }
  }
})

test_that("a manual derived result remains independent of its NR sources", {
  fixture <- autocoder_contract_fixture("SUM_SCORE")
  responses <- autocoder_contract_responses()
  responses$manual <- '[{"id":"Derived","status":"CODING_COMPLETE","code":1,"score":1}]'
  out <- autocoder_contract_compare(fixture$units$coding_scheme, responses)
  derived <- out[out$id == "Derived", ]
  expect_equal(nrow(derived), 1L)
  expect_identical(derived$status, "CODING_COMPLETE")
  expect_equal(derived$code, 1L)
  expect_equal(derived$score, 1)
  expect_identical(autocoder_contract_flags(derived), FALSE)
})
