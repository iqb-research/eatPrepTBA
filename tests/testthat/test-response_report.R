response_slots <- function(ids) {
  lapply(
    ids,
    function(id) list(id = id, content = "[]", ts = "1")
  )
}

direct_responses <- function(ids) {
  lapply(
    ids,
    function(id) list(id = id, status = "VALUE_CHANGED", value = 1)
  )
}

subform_responses <- function(ids) {
  lapply(
    ids,
    function(id) {
      list(
        id = id,
        subForm = id,
        responseType = "state",
        ts = 0,
        content = "[{\"id\":\"value\",\"value\":\"1\",\"status\":\"CODING_COMPLETE\"}]"
      )
    }
  )
}

unknown_response_entries <- function(ids) {
  lapply(
    ids,
    function(id) list(id = id, something = TRUE)
  )
}

test_that("response reports keep units with empty nested response data", {
  resp <- list(
    list(
      list(
        groupname = "group",
        loginname = "login",
        code = "7kfe",
        bookletname = "booklet",
        unitname = "SB_2631",
        responses = list(
          list(id = "elementCodes", content = "[]", ts = "1")
        ),
        laststate = NA_character_
      ),
      list(
        groupname = "group",
        loginname = "login",
        code = "7kfe",
        bookletname = "booklet",
        unitname = "SB_2632",
        responses = list(),
        laststate = NA_character_
      )
    )
  )

  responses_raw <- response_report_to_tibble(resp)

  expect_equal(responses_raw$unitname, c("SB_2631", "SB_2632"))
  expect_equal(length(responses_raw$responses[[1]]), 1)
  expect_equal(length(responses_raw$responses[[2]]), 0)
})

test_that("response slot diagnostics accept the current Testcenter slot ids", {
  responses_raw <- tibble::tibble(
    responses = list(response_slots(c(
      "elementCodes",
      "stateVariableCodes",
      "geometryVariableCodes"
    )))
  )

  diagnostics <- response_slot_diagnostics(responses_raw)

  expect_equal(diagnostics$n_payloads, 1L)
  expect_equal(diagnostics$n_wrapper_payloads, 1L)
  expect_equal(diagnostics$n_direct_payloads, 0L)
  expect_equal(diagnostics$missing_required, character())
  expect_equal(diagnostics$missing_optional, character())
  expect_equal(diagnostics$unknown_wrapper, character())
})

test_that("response slot diagnostics detect missing required slots", {
  responses_raw <- tibble::tibble(
    responses = list(
      response_slots(c("stateVariableCodes", "geometryVariableCodes")),
      response_slots(c("elementCodes", "geometryVariableCodes")),
      direct_responses(c("question_0", "sums"))
    )
  )

  diagnostics <- response_slot_diagnostics(responses_raw)

  expect_equal(diagnostics$n_payloads, 3L)
  expect_equal(diagnostics$n_wrapper_payloads, 2L)
  expect_equal(diagnostics$n_direct_payloads, 1L)
  expect_equal(diagnostics$missing_required, c("elementCodes", "stateVariableCodes"))
  expect_equal(unname(diagnostics$missing_required_counts[diagnostics$missing_required]), c(1L, 1L))
})

test_that("response slot diagnostics treat geometry variables as optional", {
  responses_raw <- tibble::tibble(
    responses = list(response_slots(c("elementCodes", "stateVariableCodes")))
  )

  diagnostics <- response_slot_diagnostics(responses_raw)

  expect_equal(diagnostics$missing_required, character())
  expect_equal(diagnostics$missing_optional, "geometryVariableCodes")
})

test_that("compact response slot announcements report standard slots as OK", {
  responses_raw <- tibble::tibble(
    responses = list(response_slots(c("elementCodes", "stateVariableCodes")))
  )

  announced <- announce_response_slot_diagnostics(responses_raw, diagnostics = "compact")

  expect_identical(announced, responses_raw)
})

test_that("full response slot examples are not shortened", {
  ids <- paste0("question_", 0:12)
  examples <- format_response_slot_examples(ids, n = Inf)

  expect_true(all(ids %in% strsplit(examples, ", ")[[1]]))
  expect_false(grepl("more", examples, fixed = TRUE))
})

test_that("compact response slot examples point to full diagnostics when shortened", {
  ids <- paste0("question_", 0:12)
  examples <- format_response_slot_examples(ids, n = 3, full_hint = TRUE)

  expect_match(examples, "and 10 more", fixed = TRUE)
  expect_match(examples, "diagnostics = \"full\"", fixed = TRUE)
})

test_that("standard response slot coverage includes required and optional slots", {
  coverage <- format_standard_response_slot_counts(
    c(elementCodes = 0L, stateVariableCodes = 2L),
    c(geometryVariableCodes = 1L),
    total = 3L
  )

  expect_match(
    coverage,
    "elementCodes present in 3/3 standard wrapper payloads",
    fixed = TRUE
  )
  expect_match(
    coverage,
    "stateVariableCodes present in 1/3 standard wrapper payloads",
    fixed = TRUE
  )
  expect_match(
    coverage,
    "geometryVariableCodes present in 2/3 standard wrapper payloads",
    fixed = TRUE
  )
  expect_false(grepl("absent", coverage, fixed = TRUE))
})

test_that("response preparation progress keeps elapsed time in diagnostic modes", {
  progress <- response_preparation_progress(
    "Preparing responses",
    "Prepared responses",
    diagnostics = "compact"
  )

  expect_equal(progress$show_after, 0)
  expect_false(progress$clear)
  expect_match(progress$format_done, "Prepared responses in", fixed = TRUE)
  expect_match(progress$format_done, "pb_elapsed", fixed = TRUE)
})

test_that("response slot diagnostics accept progress settings", {
  responses_raw <- tibble::tibble(
    responses = list(response_slots(c("elementCodes", "stateVariableCodes")))
  )

  diagnostics <- response_slot_diagnostics(responses_raw, progress = FALSE)

  expect_equal(diagnostics$n_wrapper_payloads, 1L)
  expect_equal(diagnostics$missing_required, character())
})

test_that("response preparation progress is quieter when diagnostics are suppressed", {
  progress <- response_preparation_progress(
    "Preparing responses",
    "Prepared responses",
    diagnostics = "none"
  )

  expect_equal(progress$show_after, 0)
  expect_true(progress$clear)
  expect_null(progress$format_done)
  expect_false(grepl("pb_bar", progress$format, fixed = TRUE))
})

test_that("missing response payload announcements can be suppressed", {
  responses <- tibble::tibble(responses = NA_character_)

  announced <- expect_silent(
    announce_missing_response_payloads(responses, diagnostics = "none")
  )

  expect_identical(announced, responses)
})

test_that("response slot diagnostics detect unknown and suspicious wrapper ids", {
  responses_raw <- tibble::tibble(
    responses = list(response_slots(c(
      "elementCodes",
      "stateVariableCodes",
      "newVariableCodes",
      "responses"
    )))
  )

  diagnostics <- response_slot_diagnostics(responses_raw)

  expect_equal(diagnostics$missing_required, character())
  expect_equal(diagnostics$unknown_wrapper, c("newVariableCodes", "responses"))
})

test_that("response slot diagnostics classify direct response ids separately", {
  responses_raw <- tibble::tibble(
    responses = list(direct_responses(c(
      "question_0",
      "sums",
      "activeQuestionIndex"
    )))
  )

  diagnostics <- response_slot_diagnostics(responses_raw)

  expect_equal(diagnostics$n_wrapper_payloads, 0L)
  expect_equal(diagnostics$n_direct_payloads, 1L)
  expect_equal(diagnostics$direct_observed, c("question_0", "sums", "activeQuestionIndex"))
  expect_equal(diagnostics$missing_required, character())
  expect_equal(diagnostics$unknown_wrapper, character())
})

test_that("subform response containers are not treated as standard wrapper slots", {
  responses_raw <- tibble::tibble(
    responses = list(subform_responses(c(
      "question_0",
      "sums",
      "activeQuestionIndex"
    )))
  )

  diagnostics <- response_slot_diagnostics(responses_raw)

  expect_equal(diagnostics$n_wrapper_payloads, 0L)
  expect_equal(diagnostics$n_subform_payloads, 1L)
  expect_equal(diagnostics$n_direct_payloads, 0L)
  expect_equal(diagnostics$subform_observed, c("question_0", "sums", "activeQuestionIndex"))
  expect_equal(diagnostics$unknown_wrapper, character())
  expect_equal(diagnostics$missing_required, character())
})

test_that("content entries with slot-like ids are treated as wrapper slots", {
  responses_raw <- tibble::tibble(
    responses = list(response_slots(c(
      "elementCodes",
      "stateVariableCodes",
      "newVariableCodes"
    )))
  )

  diagnostics <- response_slot_diagnostics(responses_raw)

  expect_equal(diagnostics$n_wrapper_payloads, 1L)
  expect_equal(diagnostics$n_direct_payloads, 0L)
  expect_equal(diagnostics$unknown_wrapper, "newVariableCodes")
})

test_that("response slot diagnostics detect mixed and unrecognized payloads", {
  responses_raw <- tibble::tibble(
    responses = list(
      c(
        response_slots("elementCodes"),
        direct_responses("question_0")
      ),
      unknown_response_entries("strange")
    )
  )

  diagnostics <- response_slot_diagnostics(responses_raw)

  expect_equal(diagnostics$n_mixed_payloads, 1L)
  expect_equal(diagnostics$n_unknown_payloads, 1L)
  expect_equal(diagnostics$unknown_entry_ids, "strange")
})

test_that("response slot diagnostics inspect unparsed response payloads", {
  responses_raw <- tibble::tibble(
    responses = as.character(jsonlite::toJSON(
      response_slots(c("elementCodes", "stateVariableCodes")),
      auto_unbox = TRUE
    ))
  )

  diagnostics <- response_slot_diagnostics(responses_raw, is_parsed = FALSE)

  expect_equal(diagnostics$missing_required, character())
  expect_equal(diagnostics$missing_optional, "geometryVariableCodes")
  expect_equal(diagnostics$unknown_wrapper, character())
})

test_that("response slot announcements do not modify response data", {
  responses_raw <- tibble::tibble(
    responses = list(response_slots(c(
      "elementCodes",
      "stateVariableCodes",
      "geometryVariableCodes"
    )))
  )

  announced <- announce_response_slot_diagnostics(responses_raw)

  expect_identical(announced, responses_raw)
})

test_that("response slot diagnostics can be suppressed", {
  responses_raw <- tibble::tibble(
    responses = list(response_slots("newVariableCodes"))
  )

  announced <- expect_silent(
    announce_response_slot_diagnostics(responses_raw, diagnostics = "none")
  )

  expect_identical(announced, responses_raw)
})

test_that("empty parsed responses are retained as empty element codes", {
  responses <- unnest_responses(list(), is_parsed = TRUE)

  expect_equal(responses$id, "elementCodes")
  expect_true(is.na(responses$content))
})

test_that("empty file response payloads are retained as empty element codes", {
  responses <- unnest_responses(NA_character_, is_parsed = FALSE)

  expect_equal(responses$id, "elementCodes")
  expect_true(is.na(responses$content))
})

test_that("read_responses keeps mixed rows with missing response payloads", {
  response_file <- tempfile(fileext = ".csv")
  responses_raw <- tibble::tibble(
    groupname = "group",
    loginname = "login",
    code = "7kfe",
    bookletname = "booklet",
    unitname = c("SB_2631", "SB_2632"),
    responses = c(
      '[{"id":"elementCodes","content":"[]","ts":"1"}]',
      NA_character_
    ),
    laststate = NA_character_
  )

  readr::write_delim(responses_raw, response_file, delim = ";")

  responses <- read_responses(response_file)

  expect_equal(responses$unit_key, c("SB_2631", "SB_2632"))
  expect_equal(responses$responses[responses$unit_key == "SB_2631"], "[]")
  expect_true(is.na(responses$responses[responses$unit_key == "SB_2632"]))
})

test_that("read_responses keeps all rows when all response payloads are missing", {
  response_file <- tempfile(fileext = ".csv")
  responses_raw <- tibble::tibble(
    groupname = "group",
    loginname = "login",
    code = c("7kfe", "q3u4"),
    bookletname = "booklet",
    unitname = c("SB_2631", "SB_2632"),
    responses = NA_character_,
    laststate = NA_character_
  )

  readr::write_delim(responses_raw, response_file, delim = ";")

  responses <- read_responses(response_file)

  expect_equal(responses$unit_key, c("SB_2631", "SB_2632"))
  expect_true(all(is.na(responses$responses)))
})

test_that("code_responses ignores rows with missing response payloads", {
  units <- tibble::tibble(
    ws_id = 1,
    ws_label = "workspace",
    unit_key = "UNIT_1",
    unit_id = 1,
    unit_label = "Unit 1",
    coding_scheme = list(list(variableCodings = list())),
    unit_variables = list(list())
  )

  responses <- tibble::tibble(
    group_id = "group",
    login_name = "login",
    login_code = "code",
    booklet_id = "booklet",
    unit_key = "UNIT_1",
    responses = NA_character_
  )

  coded <- NULL

  coded <- code_responses(responses, units, prepare = FALSE)
  expect_equal(nrow(coded), 0)
})

test_that("code_responses returns structured empty output for missing payloads with preparation", {
  units <- tibble::tibble(
    ws_id = 1,
    ws_label = "workspace",
    unit_key = "UNIT_1",
    unit_id = 1,
    unit_label = "Unit 1",
    coding_scheme = list(list(variableCodings = list())),
    unit_variables = list(list())
  )

  responses <- tibble::tibble(
    group_id = "group",
    login_name = "login",
    login_code = "code",
    booklet_id = "booklet",
    unit_key = "UNIT_1",
    responses = NA_character_
  )

  coded <- NULL

  coded <- code_responses(responses, units, prepare = TRUE)
  expect_equal(nrow(coded), 0)
  expect_true(all(c(
    "group_id", "login_name", "login_code", "booklet_id", "unit_key",
    "unit_alias", "variable_id", "code_status", "code_type"
  ) %in% names(coded)))
})

test_that("parsed laststate objects from response reports are prepared", {
  laststate <- unnest_laststate(list(list(
    PLAYER = "RUNNING",
    RESPONSE_PROGRESS = "complete"
  )))

  expect_equal(laststate$PLAYER, "RUNNING")
  expect_equal(laststate$RESPONSE_PROGRESS, "complete")
})

test_that("coded empty response payloads are retained as responses", {
  response_file <- tempfile(fileext = ".csv")
  responses_raw <- tibble::tibble(
    groupname = "group",
    loginname = "login",
    code = "7kfe",
    bookletname = "booklet",
    unitname = "SB_2631",
    responses = as.character(jsonlite::toJSON(
      list(list(id = "responses", content = "[]", ts = "1")),
      auto_unbox = TRUE
    )),
    laststate = NA_character_
  )

  readr::write_delim(responses_raw, response_file, delim = ";")

  responses <- read_responses(response_file)

  expect_equal(responses$unit_key, "SB_2631")
  expect_equal(responses$coded, "[]")
  expect_equal(responses$responses, "[]")
  expect_equal(responses$responses_ts, "1")
})

test_that("code_responses handles payloads when no coding schemes are available", {
  units <- tibble::tibble(
    ws_id = 1,
    ws_label = "workspace",
    unit_key = "UNIT_1",
    unit_id = 1,
    unit_label = "Unit 1",
    coding_scheme = NA,
    unit_variables = list(list())
  )

  responses <- tibble::tibble(
    group_id = "group",
    login_name = "login",
    login_code = "code",
    booklet_id = "booklet",
    unit_key = "UNIT_1",
    responses = "[]"
  )

  coded <- NULL

  coded <- code_responses(responses, units, prepare = TRUE)
  expect_equal(nrow(coded), 0)
  expect_true(all(c(
    "group_id", "login_name", "login_code", "booklet_id", "unit_key",
    "unit_alias", "variable_id", "code_status", "code_type"
  ) %in% names(coded)))
})

test_that("response unit filters are applied to raw report rows", {
  responses_raw <- tibble::tibble(
    unitname = c("keep", "drop_alias", "alias"),
    originalUnitId = c("keep", "original", "drop_original")
  )

  filtered <- filter_response_units(
    responses_raw,
    units_filter_off = c("drop_alias", "drop_original")
  )

  expect_equal(filtered$unitname, "keep")
})

test_that("empty nested payloads are announced for raw reports", {
  responses_raw <- tibble::tibble(
    unitname = c("SB_2631", "SB_2632"),
    responses = list(
      list(list(id = "elementCodes", content = "[]")),
      list()
    )
  )

  announced <- announce_empty_nested_response_payloads(responses_raw)
  expect_equal(announced, responses_raw)
})

test_that("unit filters announce when all response rows are removed", {
  expect_null(
    announce_response_unit_filter(
      n_before = 2,
      n_after = 0,
      units_filter_off = "drop"
    )
  )
})
