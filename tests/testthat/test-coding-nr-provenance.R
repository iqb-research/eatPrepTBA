nr_provenance_variable <- function(id = "ref", alias = "alias", source_type = "BASE",
                                   transform = TRUE, sources = list()) {
  list(
    id = id, alias = alias, sourceType = source_type,
    sourceParameters = list(processing = if (transform) {
      list("TAKE_NOT_REACHED_AS_VALUE_CHANGED", "TAKE_EMPTY_AS_VALID")
    } else list("TAKE_EMPTY_AS_VALID")),
    deriveSources = sources,
    codes = list(list(id = 0L, type = "RESIDUAL_AUTO", score = 0L))
  )
}

nr_provenance_scheme <- function(variables = list(nr_provenance_variable())) {
  as.character(jsonlite::toJSON(list(variableCodings = variables), auto_unbox = TRUE,
                               null = "null"))
}

nr_provenance_annotate <- function(payload, scheme = nr_provenance_scheme()) {
  result <- annotate_nr_response_payloads(scheme, tibble::tibble(responses = payload))
  jsonlite::parse_json(result$responses[[1]])
}

test_that("NR provenance follows input status and schema without inspecting values", {
  payload <- paste0('[', paste(c(
    '{"id":"ref","subform":"a","status":"NOT_REACHED","value":null}',
    '{"id":"alias","subform":"b","status":"NOT_REACHED","value":false}',
    '{"id":"alias","subform":"c","status":"NOT_REACHED","value":[]}',
    '{"id":"alias","subform":"d","status":"VALUE_CHANGED","value":null}',
    '{"id":"alias","subform":"e","status":"DISPLAYED","value":null}',
    '{"id":"alias","subform":"f","status":"CODING_COMPLETE","code":1,"score":1}',
    '{"id":"unknown","status":"NOT_REACHED","coding_from_not_reached":true}',
    '{"id":"alias","subform":"g","value":null,"coding_from_not_reached":true}'
  ), collapse = ','), ']')
  records <- nr_provenance_annotate(payload)
  flags <- vapply(records, function(x) if (is.null(x$coding_from_not_reached)) NA else {
    x$coding_from_not_reached
  }, logical(1))
  expect_identical(flags, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA))
  original <- jsonlite::parse_json(payload)
  for (i in seq_along(records)) {
    records[[i]]$coding_from_not_reached <- NULL
    original[[i]]$coding_from_not_reached <- NULL
  }
  expect_identical(records, original)
})

test_that("NR provenance handles alias precedence and the derived-shadow exception", {
  scheme <- nr_provenance_scheme(list(
    nr_provenance_variable("base", "input"),
    nr_provenance_variable("derived", "base", "COPY_VALUE", sources = list("base")),
    nr_provenance_variable("ordinary", "ref", transform = FALSE),
    nr_provenance_variable("ref", "different")
  ))
  records <- nr_provenance_annotate(paste0('[', paste(c(
    '{"id":"base","status":"NOT_REACHED"}',
    '{"id":"derived","status":"NOT_REACHED"}',
    '{"id":"ref","status":"NOT_REACHED"}'
  ), collapse = ','), ']'), scheme)
  expect_identical(vapply(records, `[[`, logical(1), "coding_from_not_reached"),
                   c(TRUE, FALSE, FALSE))
})

test_that("ambiguous raw responses or schema identities never confirm NR provenance", {
  records <- nr_provenance_annotate(paste0(
    '[{"id":"ref","status":"NOT_REACHED"},',
    '{"id":"alias","status":"VALUE_CHANGED"}]'
  ))
  expect_null(records[[1]]$coding_from_not_reached)
  expect_null(records[[2]]$coding_from_not_reached)

  for (variables in list(
    list(nr_provenance_variable(), nr_provenance_variable("other", "alias")),
    list(nr_provenance_variable(), nr_provenance_variable("ref", "other"))
  )) {
    records <- nr_provenance_annotate(
      '[{"id":"alias","status":"NOT_REACHED"}]', nr_provenance_scheme(variables)
    )
    expect_null(records[[1]]$coding_from_not_reached)
  }
})

test_that("ordinary payloads retain their representation and stale markers are reset", {
  scheme <- nr_provenance_scheme(list(nr_provenance_variable(transform = FALSE)))
  payload <- '[ { "id": "alias", "value": null, "status": "NOT_REACHED" } ]'
  input <- tibble::tibble(responses = payload, row_identifier = 10L)
  expect_identical(annotate_nr_response_payloads(scheme, input), input)
  records <- nr_provenance_annotate(
    '[{"id":"alias","status":"NOT_REACHED","coding_from_not_reached":true}]', scheme
  )
  expect_false(records[[1]]$coding_from_not_reached)
})

test_that("malformed and absent payloads retain the existing coder error behavior", {
  input <- tibble::tibble(responses = c(NA_character_, 'invalid', 'null', '{}'))
  expect_identical(annotate_nr_response_payloads(nr_provenance_scheme(), input), input)
  expect_identical(annotate_nr_response_payloads('invalid', input), input)
  records <- nr_provenance_annotate(
    '[{"id":"alias","status":"NOT_REACHED","coding_from_not_reached":true}]',
    'invalid'
  )
  expect_null(records[[1]]$coding_from_not_reached)
})

test_that("duplicate JSON fields cannot retain an unverified provenance marker", {
  records <- nr_provenance_annotate(paste0(
    '[{"id":"alias","status":"NOT_REACHED","status":"VALUE_CHANGED",',
    '"coding_from_not_reached":true,"coding_from_not_reached":true}]'
  ))
  expect_false(records[[1]]$coding_from_not_reached)
  expect_equal(sum(names(records[[1]]) == "coding_from_not_reached"), 1L)
})

test_that("provenance survives the real coder without transferring to derived variables", {
  scheme <- nr_provenance_scheme(list(
    nr_provenance_variable(),
    nr_provenance_variable("derived", "item", "COPY_VALUE", sources = list("ref"))
  ))
  input <- tibble::tibble(
    row_identifier = c("first", "second"),
    responses = c(
      '[{"id":"ref","status":"NOT_REACHED","value":null,"code":7,"score":9}]',
      '[{"id":"alias","subform":"a","status":"NOT_REACHED","value":false},{"id":"alias","subform":"b","status":"VALUE_CHANGED","value":false}]'
    )
  )
  output <- eatAutoCode::code_responses_array(
    scheme, annotate_nr_response_payloads(scheme, input)
  )
  base <- output[output$id == "alias", ]
  expect_identical(base$coding_from_not_reached, c(TRUE, TRUE, FALSE))
  expect_true(all(base$status == "CODING_COMPLETE"))
  expect_true(all(base$code == 0L))
  expect_true(all(base$score == 0L))
  expect_identical(base$row_identifier, c("first", "second", "second"))
  expect_true(all(is.na(output$coding_from_not_reached[output$id == "item"])))
})

test_that("derived aliases replacing a base response do not inherit its NR marker", {
  scheme <- nr_provenance_scheme(list(
    nr_provenance_variable("base", "base"),
    nr_provenance_variable("derived", "base", "COPY_VALUE", sources = list("base"))
  ))
  input <- tibble::tibble(responses = '[{"id":"base","status":"NOT_REACHED","value":null}]')
  output <- eatAutoCode::code_responses_array(
    scheme, annotate_nr_response_payloads(scheme, input)
  )
  expect_identical(output$id, "base")
  expect_identical(output$status, "CODING_COMPLETE")
  expect_false("coding_from_not_reached" %in% names(output))
})

test_that("geometry overlays preserve inherited status and replace explicit status provenance", {
  scheme <- nr_provenance_scheme()
  input <- tibble::tibble(
    row_identifier = 1:5,
    responses = c(rep('[{"id":"alias","status":"NOT_REACHED","value":null}]', 3),
                  '[]', '[{"id":"alias","status":"NOT_REACHED","value":null}]'),
    geometry_variables = c(
      '[{"id":"alias","status":"VALUE_CHANGED","value":false}]',
      '[{"id":"alias","value":false,"coding_from_not_reached":false}]',
      '[{"id":"alias","status":"NOT_REACHED","value":false}]',
      '[{"id":"alias","value":false,"coding_from_not_reached":true}]',
      '[{"id":"alias","status":null,"value":false,"coding_from_not_reached":true}]'
    )
  )
  output <- eatAutoCode::code_responses_array(
    scheme, annotate_nr_response_payloads(scheme, input)
  )
  expect_identical(output$coding_from_not_reached, c(FALSE, TRUE, TRUE, NA, NA))
  expect_true(all(output$status[1:3] == "CODING_COMPLETE"))
  expect_true(all(is.na(output$status[4:5])))
})

test_that("existing manual overlays recompute provenance and preserve their codes", {
  scheme <- nr_provenance_scheme()
  input <- tibble::tibble(
    row_identifier = 1:5,
    responses = rep('[{"id":"alias","status":"NOT_REACHED","value":null}]', 5),
    manual = c(
      '[{"id":"alias","status":"CODING_COMPLETE","code":5,"score":3,"coding_from_not_reached":true}]',
      '[{"id":"alias","status":"NOT_REACHED","code":-96,"score":0,"coding_from_not_reached":false}]',
      '[{"id":"alias","value":false,"coding_from_not_reached":false}]',
      '[{"id":"alias","status":null,"code":5,"score":3,"coding_from_not_reached":true}]',
      '[{"id":"alias","status":"VALUE_CHANGED","value":false,"coding_from_not_reached":true}]'
    )
  )
  output <- eatAutoCode::code_responses_array(
    scheme, annotate_nr_response_payloads(scheme, input)
  )
  expect_identical(output$coding_from_not_reached, c(FALSE, TRUE, TRUE, NA, FALSE))
  expect_identical(output$status, c(rep("CODING_COMPLETE", 3), NA_character_, "CODING_COMPLETE"))
  expect_identical(output$code, c(5L, 0L, 0L, 5L, 0L))
  expect_identical(output$score, c(3L, 0L, 0L, 3L, 0L))
})

test_that("manual provenance prepared before missing-status promotion is retained", {
  scheme <- nr_provenance_scheme()
  input <- tibble::tibble(
    responses = '[{"id":"alias","status":"NOT_REACHED","value":null}]',
    manual = '[{"id":"alias","status":"CODING_COMPLETE","code":-96,"score":0,"coding_from_not_reached":true}]'
  )
  annotated <- annotate_nr_response_payloads(scheme, input, manual_provenance_prepared = TRUE)
  expect_identical(annotated$manual, input$manual)
  output <- eatAutoCode::code_responses_array(scheme, annotated)
  expect_identical(output$coding_from_not_reached, TRUE)
  expect_identical(output$status, "CODING_COMPLETE")
  expect_identical(output$code, -96L)
  expect_identical(output$score, 0L)
})

test_that("annotating provenance preserves numeric values and exact coding comparisons", {
  variable <- nr_provenance_variable()
  variable$codes <- c(list(list(
    id = 1L, type = "FULL_CREDIT", score = 1L,
    ruleSets = list(list(rules = list(list(
      method = "NUMERIC_MATCH", parameters = list("1.2345678901234567")
    ))))
  )), variable$codes)
  scheme <- nr_provenance_scheme(list(variable))
  numbers <- c(
    "1.2345678901234567", "1.2345678901234567e-200", "1.2345678901234567e200",
    "0.000000000000000012345678901234567", "9007199254740993",
    "1.7976931348623157e308", "5e-324", "2.2250738585072014e-308", "-0.0",
    "[1.2345678901234567,1.2345678901234567e-200,9007199254740993]"
  )
  input <- tibble::tibble(responses = paste0(
    '[{"id":"alias","status":"VALUE_CHANGED","value":', numbers, '}]'
  ))
  expected <- eatAutoCode::code_responses_array(scheme, input)
  actual <- eatAutoCode::code_responses_array(
    scheme, annotate_nr_response_payloads(scheme, input)
  )
  expect_identical(actual[names(expected)], expected)
  expect_identical(actual$code[1], 1L)
  expect_identical(actual$score[1], 1L)
  expect_true(all(actual$coding_from_not_reached == FALSE))
})
