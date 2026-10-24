autocoder_completion_fixture <- function(type = "SUM_SCORE", processing = character(),
                                         manual = FALSE) {
  code <- function(id, kind, score, method, match) {
    list(id = id, type = kind, score = score, ruleSets = list(list(
      rules = list(list(method = method, parameters = list(match)))
    )))
  }
  base <- function(id) {
    list(id = id, alias = toupper(id), sourceType = "BASE",
         sourceParameters = list(processing = list()), deriveSources = list(),
         processing = list(), codeModel = "RULES_ONLY", codes = list(
           code(1L, "FULL_CREDIT", 1L, "NUMERIC_MATCH", "1"),
           list(id = 0L, type = "RESIDUAL_AUTO", score = 0L)
         ))
  }
  variables <- list(base("a"), base("b"))
  if (manual) {
    variables[[1]]$codeModel <- "MANUAL_ONLY"
    variables[[1]]$codes <- list(list(
      id = 1L, type = "FULL_CREDIT", score = 1L,
      manualInstruction = "Human coding required."
    ))
  }
  concat <- type == "CONCAT_CODE"
  variables[[3]] <- list(
    id = "d", alias = "D", sourceType = type, deriveSources = list("a", "b"),
    sourceParameters = list(processing = as.list(processing)), processing = list(),
    codeModel = "RULES_ONLY", codes = list(
      code(1L, "FULL_CREDIT", 2L, if (concat) "MATCH" else "NUMERIC_MATCH",
           if (concat) "1_1" else "2"),
      code(2L, "PARTIAL_CREDIT", 1L, if (concat) "MATCH" else "NUMERIC_MATCH",
           if (concat) "1_x" else "1"),
      list(id = 0L, type = "RESIDUAL_AUTO", score = 0L)
    )
  )
  units <- minimal_units()
  units$unit_codes <- NULL
  units$coding_scheme <- autocoder_contract_json(list(version = "3.5", variableCodings = variables))
  units$unit_variables <- list(tibble::tibble(
    variable_id = c("A", "B"), variable_ref = c("a", "b"),
    variable_type = "integer", variable_format = "", variable_multiple = FALSE,
    variable_page = 0:1,
    variable_values = rep(list(tibble::tibble(value = NA, value_label = NA)), 2L)
  ))
  units$items_list <- list(tibble::tibble(
    item_no = 1L, item_id = "I1", variable_id = "D", variable_ref = "d"
  ))
  suppressMessages(add_coding_scheme(units))
}

autocoder_completion_responses <- function(status, value, person = "P") {
  tibble::tibble(
    group_id = "G", login_name = person, login_code = person, booklet_id = "B",
    unit_key = "U1", unit_alias = "U1",
    responses = autocoder_contract_json(unname(Map(function(id, status, value) {
      list(id = id, status = status, value = value)
    }, c("a", "b"), status, value)))
  )
}

autocoder_completion_code <- function(responses, units, prepare) {
  messages <- testthat::capture_messages(
    expect_no_warning(out <- code_responses(responses, units, prepare = prepare))
  )
  expect_false(any(grepl("Preparation could not be completed", messages, fixed = TRUE)))
  out
}

test_that("partial-credit derivations retain their results through completion", {
  options <- c(DISPLAYED = "TAKE_DISPLAYED_AS_ZERO_IF_PARTIAL",
               INVALID = "TAKE_EMPTY_AS_ZERO_IF_PARTIAL")
  for (type in c("SUM_SCORE", "SUM_CODE", "CONCAT_CODE")) {
    for (status in names(options)) {
      responses <- autocoder_completion_responses(c("VALUE_CHANGED", status), list(1L, ""))
      for (prepare in c(FALSE, TRUE)) {
        sources_without_option <- NULL
        for (enabled in c(FALSE, TRUE)) {
          units <- autocoder_completion_fixture(type, if (enabled) options[[status]] else character())
          coded <- autocoder_completion_code(responses, units, prepare)
          derived <- coded[coded$variable_id == "D", ]
          expect_equal(nrow(derived), 1L)
          expect_identical(derived$code_status, if (enabled) "CODING_COMPLETE" else "INVALID")
          expect_equal(derived$code_id, if (enabled) 2L else NA_integer_)
          expect_equal(derived$code_score, if (enabled) 1 else NA_real_)
          sources <- coded[match(c("A", "B"), coded$variable_id),
                           c("variable_id", "code_status", "value", "coding_from_not_reached")]
          expect_identical(sources$code_status, c("CODING_COMPLETE", status))
          expect_equal(unlist(sources$value, use.names = FALSE), c("1", ""))
          # These schemes have no NR-promotion rule or supplied provenance.
          expect_identical(sources$coding_from_not_reached, c(NA, NA))
          expect_identical(derived$coding_from_not_reached, NA)
          if (enabled) expect_equal(sources, sources_without_option) else sources_without_option <- sources
          if (prepare) {
            completed <- autocoder_contract_complete(coded, units, recode = TRUE, preserve = TRUE)
            result <- completed[completed$variable_id == "D", ]
            expect_identical(result$code_status, derived$code_status)
            expect_equal(result$code_id, if (enabled) 2 else -98)
            expect_equal(result$code_score, if (enabled) 1 else 0)
            expect_identical(result$code_type, if (enabled) "PARTIAL_CREDIT" else "MISSING_INVALID_RESPONSE")
          }
        }
      }
    }
  }
})

test_that("whole calls without numeric codes remain preparable and completable", {
  cases <- list(
    displayed = list(status = rep("DISPLAYED", 2L), value = list(NULL, NULL),
                     result_status = rep("DISPLAYED", 3L),
                     codes = rep(-99, 3L), scores = rep(0, 3L),
                     types = rep("MISSING_BY_OMISSION", 3L)),
    not_reached = list(status = rep("NOT_REACHED", 2L), value = list(NULL, NULL),
                       result_status = rep("NOT_REACHED", 3L),
                       codes = rep(-96, 3L), scores = rep(NA_real_, 3L),
                       types = rep("MISSING_NOT_REACHED", 3L)),
    invalid = list(status = rep("INVALID", 2L), value = list("", ""),
                   result_status = rep("INVALID", 3L),
                   codes = rep(-98, 3L), scores = rep(0, 3L),
                   types = rep("MISSING_INVALID_RESPONSE", 3L)),
    pending = list(status = c("VALUE_CHANGED", "DISPLAYED"), value = list(1L, NULL),
                   result_status = c("CODING_INCOMPLETE", "DISPLAYED", "DERIVE_PENDING"),
                   codes = c(-90, -99, -90), scores = c(NA_real_, 0, NA_real_),
                   types = c("CODING_INCOMPLETE", "MISSING_BY_OMISSION", "DERIVE_PENDING"))
  )
  for (name in names(cases)) {
    case <- cases[[name]]
    units <- autocoder_completion_fixture(
      processing = c("TAKE_DISPLAYED_AS_ZERO_IF_PARTIAL", "TAKE_EMPTY_AS_ZERO_IF_PARTIAL"),
      manual = name == "pending"
    )
    responses <- autocoder_completion_responses(case$status, case$value)
    unprepared <- NULL
    for (prepare in c(FALSE, TRUE)) {
      coded <- autocoder_completion_code(responses, units, prepare)
      coded <- coded[match(c("A", "B", "D"), coded$variable_id), ]
      expect_identical(coded$code_id, rep(NA_integer_, 3L), info = name)
      expect_identical(coded$code_score, rep(NA_real_, 3L), info = name)
      expect_identical(coded$code_status, case$result_status, info = name)
      if (!prepare) {
        unprepared <- coded
      } else {
        expect_identical(coded$code_type, rep(NA_character_, 3L), info = name)
        expect_equal(coded$coding_from_not_reached, unprepared$coding_from_not_reached)
        expect_equal(unlist(coded$value, use.names = FALSE), unlist(unprepared$value, use.names = FALSE))
        completed <- autocoder_contract_complete(coded, units, recode = FALSE, preserve = TRUE)
        expect_identical(completed$code_status, case$result_status, info = name)
        expect_equal(completed$code_id, case$codes, info = name)
        expect_equal(completed$code_score, case$scores, info = name)
        expect_identical(completed$code_type, case$types, info = name)
      }
    }
  }
})

test_that("uncoded people alongside coded people preserve actual codes and scores", {
  units <- autocoder_completion_fixture()
  responses <- dplyr::bind_rows(
    autocoder_completion_responses(rep("DISPLAYED", 2L), list(NULL, NULL), "missing"),
    autocoder_completion_responses(rep("VALUE_CHANGED", 2L), list(1L, 1L), "worked")
  )
  for (prepare in c(FALSE, TRUE)) {
    coded <- autocoder_completion_code(responses, units, prepare)
    missing <- coded[coded$login_code == "missing", ]
    worked <- coded[coded$login_code == "worked", ]
    expect_true(all(is.na(missing$code_id)))
    expect_true(all(is.na(missing$code_score)))
    expect_equal(worked$code_id, rep(1L, 3L))
    expect_equal(worked$code_score, c(1, 1, 2))
    if (prepare) {
      completed <- autocoder_contract_complete(coded, units, recode = FALSE, preserve = TRUE)
      expect_equal(completed$code_id[completed$login_code == "missing"], rep(-99, 3L))
      expect_equal(completed$code_score[completed$login_code == "missing"], rep(0, 3L))
      expect_equal(completed$code_id[completed$login_code == "worked"], worked$code_id)
      expect_equal(completed$code_score[completed$login_code == "worked"], worked$code_score)
    }
  }
})
