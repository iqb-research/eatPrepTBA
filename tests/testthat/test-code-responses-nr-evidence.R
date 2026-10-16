nr_evidence_units <- function(derived = FALSE) {
  units <- minimal_units()
  scheme <- jsonlite::fromJSON(units$coding_scheme, simplifyVector = FALSE)
  processing <- c("TAKE_NOT_REACHED_AS_VALUE_CHANGED", "TAKE_EMPTY_AS_VALID")
  scheme$variableCodings[[1]]$sourceParameters$processing <- as.list(processing)
  scheme$variableCodings[[1]]$codes <- list(
    list(id = 1L, type = "FULL_CREDIT", score = 1L, ruleSets = list()),
    list(id = 0L, type = "RESIDUAL_AUTO", score = 0L)
  )
  codes <- units$unit_codes[[1]]
  codes$variable_source_processing <- list(processing)
  codes$variable_codes <- list(tibble::tibble(
    code_id = c(1L, 0L), code_type = c("FULL_CREDIT", "RESIDUAL_AUTO"),
    code_score = c(1L, 0L)
  ))
  if (derived) {
    coding <- scheme$variableCodings[[1]]
    coding$id <- "derived_ref"
    coding$alias <- "D1"
    coding$sourceType <- "COPY_VALUE"
    coding$deriveSources <- list("v1")
    scheme$variableCodings[[2]] <- coding
    derived_codes <- codes
    derived_codes$variable_id <- "D1"
    derived_codes$variable_ref <- "derived_ref"
    derived_codes$variable_source_type <- "COPY_VALUE"
    derived_codes$variable_level <- 1L
    derived_codes$variable_sources <- list(tibble::tibble(
      variable_source_id = "V1", variable_source_ref = "v1",
      variable_source_level = 0L, variable_source_direct = TRUE
    ))
    codes <- dplyr::bind_rows(codes, derived_codes)
  }
  units$coding_scheme <- as.character(jsonlite::toJSON(
    scheme, auto_unbox = TRUE, null = "null"
  ))
  units$unit_codes <- list(codes)
  units
}

nr_evidence_responses <- function(payloads) {
  tibble::tibble(
    group_id = "G1", login_name = "L1", login_code = paste0("P", seq_along(payloads)),
    booklet_id = "B1", unit_key = "U1", unit_alias = "U1", responses = payloads
  )
}

nr_evidence_manual <- function(responses, code_id) {
  dplyr::mutate(
    dplyr::select(responses, group_id, booklet_id, login_code, login_name, unit_key),
    variable_id = "V1", code_id = code_id
  )
}

test_that("actual automatic coding retains NR origin independently of response value", {
  units <- nr_evidence_units()
  responses <- nr_evidence_responses(c(
    '[{"id":"v1","status":"NOT_REACHED","value":null}]',
    '[{"id":"V1","status":"NOT_REACHED","value":false}]',
    '[{"id":"v1","status":"VALUE_CHANGED","value":false}]',
    '[{"id":"V1","status":"VALUE_CHANGED","value":[false,true]}]'
  ))
  original <- responses
  for (prepare in c(FALSE, TRUE)) {
    out <- suppressMessages(code_responses(responses, units, prepare = prepare))
    out <- dplyr::arrange(out, login_code)
    expect_equal(nrow(out), 4L)
    expect_identical(out$coding_from_not_reached, c(TRUE, TRUE, FALSE, FALSE))
    expect_equal(out$variable_id, rep("V1", 4))
    expect_equal(out$code_status, rep("CODING_COMPLETE", 4))
    expect_equal(out$code_id, rep(0L, 4))
    expect_equal(out$code_score, rep(0, 4))
    expect_true(is.na(out$value[1]))
    expect_equal(out$value[2:3], c("false", "false"))
    expect_match(out$value[4], "false", fixed = TRUE)
    expect_match(out$value[4], "true", fixed = TRUE)
    if (prepare) expect_equal(out$code_type, rep("RESIDUAL_AUTO", 4))
  }
  expect_identical(responses, original)
})

test_that("derived automatic results do not inherit their source's NR origin", {
  units <- nr_evidence_units(derived = TRUE)
  responses <- nr_evidence_responses(
    '[{"id":"v1","status":"NOT_REACHED","value":false}]'
  )
  for (prepare in c(FALSE, TRUE)) {
    out <- suppressMessages(code_responses(responses, units, prepare = prepare))
    expect_setequal(out$variable_id, c("V1", "D1"))
    expect_identical(out$coding_from_not_reached[out$variable_id == "V1"], TRUE)
    expect_identical(out$coding_from_not_reached[out$variable_id == "D1"], NA)
  }
})

test_that("effective manual codes determine NR origin before status promotion", {
  units <- nr_evidence_units()
  # Manual insertion matches raw response IDs, before the coder maps references
  # to aliases. Use the alias here so the override actually takes effect.
  responses <- nr_evidence_responses(c(
    '[{"id":"V1","status":"NOT_REACHED","value":false}]',
    '[{"id":"V1","status":"VALUE_CHANGED","value":true}]'
  ))
  manual <- nr_evidence_manual(responses, c(1L, -96L))
  for (prepare in c(FALSE, TRUE)) {
    out <- suppressMessages(code_responses(
      responses, units, prepare = prepare, codes_manual = manual
    ))
    out <- dplyr::arrange(out, login_code)
    expect_identical(out$coding_from_not_reached, c(FALSE, TRUE))
    expect_equal(out$code_id, c(1L, -96L))
    expect_equal(out$code_score[1], 1)
    expect_equal(out$code_status, rep("CODING_COMPLETE", 2))
    expect_equal(out$value, c("false", "true"))
  }

  manual$code_id <- c(1L, -196L)
  missings <- tibble::tibble(
    code_id = -196L, code_status = "NOT_REACHED", code_score = NA_real_,
    code_type = "MISSING_NOT_REACHED"
  )
  out <- suppressMessages(code_responses(
    responses, units, prepare = TRUE, codes_manual = manual, missings = missings
  ))
  out <- dplyr::arrange(out, login_code)
  expect_identical(out$coding_from_not_reached, c(FALSE, TRUE))
  expect_equal(out$code_id, c(1L, -196L))

  # A manual alias does not match a response using its internal ID in the
  # current coder. Provenance must follow the override that actually happened.
  unmatched <- responses[1, ]
  unmatched$responses <- '[{"id":"v1","status":"NOT_REACHED","value":false}]'
  out <- suppressMessages(code_responses(
    unmatched, units, prepare = TRUE, codes_manual = manual[1, ]
  ))
  expect_identical(out$coding_from_not_reached, TRUE)
  expect_equal(out$variable_id, "V1")
  expect_true(is.null(out[["code_id"]]) || all(is.na(out[["code_id"]])))
  expect_true(is.null(out[["code_score"]]) || all(is.na(out[["code_score"]])))
})

test_that("actual coding provenance prevents artificial work in design completion", {
  units <- nr_evidence_units()
  scheme <- jsonlite::fromJSON(units$coding_scheme, simplifyVector = FALSE)
  earlier <- scheme$variableCodings[[1]]
  earlier$id <- "earlier_ref"
  earlier$alias <- "Earlier"
  earlier$sourceParameters$processing <- list()
  scheme$variableCodings <- c(list(earlier), scheme$variableCodings)
  units$coding_scheme <- as.character(jsonlite::toJSON(
    scheme, auto_unbox = TRUE, null = "null"
  ))
  marker_codes <- units$unit_codes[[1]]
  marker_codes$variable_page <- 2L
  earlier_codes <- marker_codes
  earlier_codes$variable_id <- "Earlier"
  earlier_codes$variable_ref <- "earlier_ref"
  earlier_codes$variable_page <- 1L
  earlier_codes$variable_source_processing <- list(character())
  units$unit_codes <- list(dplyr::bind_rows(earlier_codes, marker_codes))
  units$items_list <- list(tibble::tibble(
    variable_id = c("Earlier", "V1"), variable_ref = c("earlier_ref", "v1"),
    item_id = c("I0", "I1")
  ))

  for (status in c("NOT_REACHED", "VALUE_CHANGED")) {
    payload <- list(
      list(id = "earlier_ref", status = "DISPLAYED", value = NULL),
      list(id = "v1", status = status, value = FALSE)
    )
    responses <- nr_evidence_responses(as.character(jsonlite::toJSON(
      payload, auto_unbox = TRUE, null = "null"
    )))
    coded <- suppressMessages(code_responses(responses, units, prepare = TRUE))
    design <- dplyr::mutate(
      dplyr::select(coded, group_id, login_name, login_code, booklet_id,
                    unit_key, unit_alias, variable_id),
      booklet_no = 1L, testlet_no = 1L, unit_booklet_no = 1L
    )
    out <- suppressMessages(complete_design(
      coded, units, design, preserve_not_reached = TRUE,
      recode_omissions_to_not_reached = TRUE
    ))
    earlier <- out[out$variable_id == "Earlier", ]
    marker <- out[out$variable_id == "V1", ]
    expect_equal(earlier$code_id, if (status == "NOT_REACHED") -96 else -99)
    expect_equal(earlier$code_score, if (status == "NOT_REACHED") NA_real_ else 0)
    expect_identical(earlier$code_status, "DISPLAYED")
    expect_equal(marker$code_id, 0L)
    expect_equal(marker$code_score, 0)
    expect_equal(marker$code_type, "RESIDUAL_AUTO")
    expect_equal(marker$value, "false")
    expect_identical(marker$code_status, "CODING_COMPLETE")
    expect_identical(marker$coding_from_not_reached, status == "NOT_REACHED")
  }
})

test_that("existing manual JSON overrides raw NR provenance with its effective status", {
  units <- nr_evidence_units()
  responses <- nr_evidence_responses(
    '[{"id":"V1","status":"NOT_REACHED","value":false}]'
  )
  responses$manual <-
    '[{"id":"V1","status":"CODING_COMPLETE","code":1,"score":1}]'
  for (prepare in c(FALSE, TRUE)) {
    out <- suppressMessages(code_responses(responses, units, prepare = prepare))
    expect_identical(out$coding_from_not_reached, FALSE)
    expect_equal(out$code_id, 1L)
    expect_equal(out$code_score, 1)
    expect_identical(out$code_status, "CODING_COMPLETE")
    expect_identical(out$value, "false")
  }
})

test_that("missing payloads and absent schemes retain a logical provenance column", {
  for (prepare in c(FALSE, TRUE)) {
    responses <- nr_evidence_responses(NA_character_)
    missing <- suppressMessages(code_responses(
      responses, tibble::tibble(unit_key = "U1"), prepare = prepare
    ))
    expect_equal(nrow(missing), 0L)
    expect_identical(missing$coding_from_not_reached, logical())

    units <- nr_evidence_units()
    units$coding_scheme <- NA_character_
    responses$responses <- "[]"
    no_scheme <- suppressMessages(code_responses(responses, units, prepare = prepare))
    expect_equal(nrow(no_scheme), 0L)
    expect_identical(no_scheme$coding_from_not_reached, logical())

    empty <- suppressMessages(code_responses(
      responses, nr_evidence_units(), prepare = prepare
    ))
    expect_type(empty$coding_from_not_reached, "logical")
    expect_true(all(is.na(empty$coding_from_not_reached)))
  }
})
