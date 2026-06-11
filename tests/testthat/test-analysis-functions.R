test_that("compute_sizes totals dependencies for units, booklets, and testtakers", {
  data <- tibble::tibble(
    type = c("Unit", "Booklet", "Testtakers", "Resource"),
    name = c("U1.xml", "B1.xml", "TT.xml", "res.vocs"),
    size = c(10, 5, 3, 2),
    dependencies = list(
      list(list(relationship_type = "isDefinedBy", object_name = "U1.xml")),
      list(list(relationship_type = "uses", object_name = "res.vocs")),
      list(),
      list()
    )
  )

  out <- compute_sizes(data)

  expect_equal(out$total_size[out$type == "Unit"], 10)
  expect_equal(out$total_size[out$type == "Booklet"], 2 / 1024^2)
  expect_equal(out$total_size[out$type == "Testtakers"], 3)
})

test_that("code_responses delegates unit coding and returns normalized columns", {
  testthat::local_mocked_bindings(
    code_responses_array = function(coding_scheme, unit_responses) {
      tibble::tibble(
        id = "V1",
        code = 1L,
        score = 1,
        status = "CODING_COMPLETE",
        value = list("A")
      )
    },
    .package = "eatAutoCode"
  )

  responses <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    unit_key = "U1",
    responses = "[{}]"
  )
  units <- minimal_units()

  out <- code_responses(responses, units, prepare = FALSE)

  expect_equal(out$unit_key, "U1")
  expect_equal(out$variable_id, "V1")
  expect_equal(out$code_id, 1L)
  expect_equal(out$code_status, "CODING_COMPLETE")
})

test_that("code_responses with no payloads does not require coding-scheme columns", {
  responses <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    unit_key = "U1",
    responses = NA_character_
  )
  units <- tibble::tibble(unit_key = "U1")

  out <- code_responses(responses, units, prepare = TRUE)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
})

test_that("code_responses_legacy accepts optional NULL coding inputs", {
  testthat::local_mocked_bindings(
    code_responses = function(coding_scheme, responses) {
      tibble::tibble(
        id = "V1",
        code = 1L,
        value = list("A")
      )
    },
    .package = "eatAutoCode"
  )

  responses <- tibble::tibble(
    unitname = "U1",
    groupname = "G1",
    loginname = "L1",
    code = "C1",
    bookletname = "B1",
    responses = list(list(id = "elementCodes", content = "[{}]"))
  )
  units <- tibble::tibble(
    unit_key = "U1",
    coding_scheme = "scheme"
  )

  expect_no_error(out <- code_responses_legacy(responses, units))
  expect_s3_class(out, "tbl_df")
  expect_equal(out$unit_key, "U1")
})

issue9_units <- function(unit_keys, variable_ids) {
  tibble::tibble(
    unit_key = unit_keys,
    unit_codes = Map(
      function(variable_id) {
        tibble::tibble(
          variable_id = variable_id,
          variable_source_type = "BASE",
          variable_level = 0L,
          variable_page = 1L,
          variable_section = 1L,
          variable_page_always_visible = FALSE
        )
      },
      variable_ids
    )
  )
}

test_that("complete_design fills missing responses according to booklet order", {
  testthat::local_mocked_bindings(
    add_coding_scheme = function(units, overwrite = FALSE, filter_has_codes = TRUE) units,
    .package = "eatPrepTBA"
  )

  units <- minimal_units()
  design <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    booklet_no = 1L,
    testlet_no = 1L,
    unit_booklet_no = 1L,
    unit_key = "U1",
    unit_alias = "U1",
    variable_id = "V1"
  )
  coded <- tibble::tibble(
    group_id = character(),
    login_name = character(),
    login_code = character(),
    booklet_id = character(),
    unit_key = character(),
    unit_alias = character(),
    variable_id = character(),
    code_status = character(),
    code_type = character(),
    code_id = integer(),
    code_score = numeric(),
    value = character()
  )

  out <- complete_design(coded, units, design)

  expect_equal(out$code_type, "MISSING_NOT_REACHED")
  expect_equal(out$code_id, -96)
  expect_false(out$id_used)
})

test_that("complete_design preserves trailing omissions by default", {
  testthat::local_mocked_bindings(
    add_coding_scheme = function(units, overwrite = FALSE, filter_has_codes = TRUE) units,
    .package = "eatPrepTBA"
  )

  units <- issue9_units(c("U1", "U2"), c("V1", "V2"))
  design <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    booklet_no = 1L,
    testlet_no = 1L,
    unit_booklet_no = c(1L, 2L),
    unit_key = c("U1", "U2"),
    unit_alias = c("U1", "U2"),
    variable_id = c("V1", "V2")
  )
  coded <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    unit_key = c("U1", "U2"),
    unit_alias = c("U1", "U2"),
    variable_id = c("V1", "V2"),
    code_status = "DISPLAYED",
    code_type = NA_character_,
    code_id = -99L,
    code_score = 0,
    value = NA_character_
  )

  out <- complete_design(coded, units, design)

  expect_equal(out$code_type, c("MISSING_BY_OMISSION", "MISSING_BY_OMISSION"))
  expect_equal(out$code_id, c(-99, -99))
})

test_that("complete_design can recode trailing omissions as not reached", {
  testthat::local_mocked_bindings(
    add_coding_scheme = function(units, overwrite = FALSE, filter_has_codes = TRUE) units,
    .package = "eatPrepTBA"
  )

  units <- issue9_units(c("U1", "U2"), c("V1", "V2"))
  design <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    booklet_no = 1L,
    testlet_no = 1L,
    unit_booklet_no = c(1L, 2L),
    unit_key = c("U1", "U2"),
    unit_alias = c("U1", "U2"),
    variable_id = c("V1", "V2")
  )
  coded <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    unit_key = c("U1", "U2"),
    unit_alias = c("U1", "U2"),
    variable_id = c("V1", "V2"),
    code_status = "DISPLAYED",
    code_type = NA_character_,
    code_id = -99L,
    code_score = 0,
    value = NA_character_
  )

  out <- complete_design(coded, units, design, recode_omissions_to_not_reached = TRUE)

  expect_equal(out$code_type, c("MISSING_NOT_REACHED", "MISSING_NOT_REACHED"))
  expect_equal(out$code_id, c(-96, -96))
})

test_that("complete_design keeps not-reached detection within testlets", {
  testthat::local_mocked_bindings(
    add_coding_scheme = function(units, overwrite = FALSE, filter_has_codes = TRUE) units,
    .package = "eatPrepTBA"
  )

  units <- issue9_units(c("U1", "U2"), c("V1", "V2"))
  design <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    booklet_no = 1L,
    testlet_no = c(1L, 2L),
    unit_booklet_no = c(1L, 2L),
    unit_key = c("U1", "U2"),
    unit_alias = c("U1", "U2"),
    variable_id = c("V1", "V2")
  )
  coded <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    unit_key = c("U1", "U2"),
    unit_alias = c("U1", "U2"),
    variable_id = c("V1", "V2"),
    code_status = c("DISPLAYED", "CODING_COMPLETE"),
    code_type = c(NA_character_, "FULL_CREDIT"),
    code_id = c(-99L, 1L),
    code_score = c(0, 1),
    value = c(NA_character_, "A")
  )

  out <- complete_design(coded, units, design, recode_omissions_to_not_reached = TRUE)

  expect_equal(out$code_type, c("MISSING_NOT_REACHED", "FULL_CREDIT"))
  expect_equal(out$code_id, c(-96, 1))
})

test_that("complete_design only requires identifiers available in design", {
  testthat::local_mocked_bindings(
    add_coding_scheme = function(units, overwrite = FALSE, filter_has_codes = TRUE) units,
    .package = "eatPrepTBA"
  )

  units <- minimal_units()
  design <- tibble::tibble(
    login_code = "C1",
    booklet_id = "B1",
    booklet_no = 1L,
    testlet_no = 1L,
    unit_booklet_no = 1L,
    unit_key = "U1",
    unit_alias = "U1",
    variable_id = "V1"
  )
  coded <- tibble::tibble(
    login_code = character(),
    booklet_id = character(),
    unit_key = character(),
    unit_alias = character(),
    variable_id = character(),
    code_status = character(),
    code_type = character(),
    code_id = integer(),
    code_score = numeric(),
    value = character()
  )

  out <- complete_design(coded, units, design)

  expect_equal(out$code_type, "MISSING_NOT_REACHED")
  expect_equal(out$login_code, "C1")
})

test_that("complete_design applies custom missing metadata", {
  testthat::local_mocked_bindings(
    add_coding_scheme = function(units, overwrite = FALSE, filter_has_codes = TRUE) units,
    .package = "eatPrepTBA"
  )

  units <- minimal_units()
  design <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    booklet_no = 1L,
    testlet_no = 1L,
    unit_booklet_no = 1L,
    unit_key = "U1",
    unit_alias = "U1",
    variable_id = "V1"
  )
  coded <- tibble::tibble(
    group_id = character(),
    login_name = character(),
    login_code = character(),
    booklet_id = character(),
    unit_key = character(),
    unit_alias = character(),
    variable_id = character(),
    code_status = character(),
    code_type = character(),
    code_id = integer(),
    code_score = numeric(),
    value = character()
  )
  missings <- tibble::tibble(
    code_id = c(-196L, -197L, -198L, -199L),
    code_status = c("CUSTOM_NOT_REACHED", "CUSTOM_ERROR", "CUSTOM_INVALID", "CUSTOM_OMISSION"),
    code_score = c(0.5, NA_real_, 0, 0),
    code_type = c(
      "MISSING_NOT_REACHED",
      "MISSING_CODING_IMPOSSIBLE",
      "MISSING_INVALID_RESPONSE",
      "MISSING_BY_OMISSION"
    )
  )

  out <- complete_design(coded, units, design, missings = missings)

  expect_equal(out$code_type, "MISSING_NOT_REACHED")
  expect_equal(out$code_id, -196L)
  expect_equal(out$code_status, "CUSTOM_NOT_REACHED")
  expect_equal(out$code_score, 0.5)
})

test_that("estimate_unit_times computes unit play and loading intervals", {
  logs <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    unit_key = "U1",
    unit_alias = "U1",
    ts = c(1000, 2000, 5000),
    log_entry = c("PLAYER = LOADING", "PLAYER = RUNNING", "END")
  )

  out <- estimate_unit_times(logs)

  expect_equal(out$unit_key, "U1")
  expect_equal(out$unit_time, 3000)
  expect_equal(out$unit_loadtime, 1000)
  expect_equal(out$unit_n_play, 1L)
})

test_that("evaluate_psychometrics summarizes code and category frequencies", {
  testthat::local_mocked_bindings(
    add_coding_scheme = function(units, overwrite = FALSE, filter_has_codes = TRUE) units,
    .package = "eatPrepTBA"
  )

  units <- minimal_units()
  design_coded <- tibble::tibble(
    group_id = c("G1", "G1"),
    login_name = c("L1", "L2"),
    login_code = c("C1", "C2"),
    unit_key = c("U1", "U1"),
    variable_id = c("V1", "V1"),
    variable_source_type = c("BASE", "BASE"),
    id_used = c(TRUE, TRUE),
    code_id = c(1L, 0L),
    code_score = c(1, 0),
    code_type = c("FULL_CREDIT", "RESIDUAL"),
    value = c("A", "B")
  )

  out <- evaluate_psychometrics(
    design_coded,
    units,
    domains = tibble::tibble(domain = "D1", unit_key = "U1")
  )

  expect_true(all(c("code_p_total", "category_p_total", "domain") %in% names(out)))
  expect_equal(sort(out$code_id), c(0L, 1L))
  expect_equal(unique(out$domain), "D1")
})

test_that("small psychometric helpers concatenate categories and correlations", {
  values <- tibble::tibble(
    category_id = list(
      tibble::tibble(id = "A"),
      tibble::tibble(id = c("A", "B"))
    )
  )
  corr_data <- tibble::tibble(
    id = c("P1", "P2", "P3", "P4"),
    code_id = c(0, 1, 1, 0),
    domain_score = c(0, 1, 1, 0)
  )

  expect_equal(
    eatPrepTBA:::concatenate_character(values$category_id),
    c("A", "[[[A;;;B]]]")
  )

  corr <- eatPrepTBA:::category_correlation(corr_data, identifiers = "id")
  expect_equal(corr$code_pbc[corr$code_id == "1"], 1)
})

test_that("layout helpers format durations and build reactable metadata", {
  data <- tibble::tibble(
    link = "https://example/",
    unit_key = "U1",
    unit_label = "Unit 1",
    unit_estimated = 60,
    unit_median = 50,
    unit_q90 = 70,
    unit_q95 = 80,
    unit_diff = 10,
    unit_diff95 = 20,
    SPF = "nein"
  )

  expect_equal(eatPrepTBA:::to_stamp(65), "01:05")
  expect_equal(eatPrepTBA:::to_stamp(-5), "-00:05")
  expect_equal(eatPrepTBA:::to_stamp(NA), "-")
  expect_type(eatPrepTBA:::colUnit(data), "list")
  expect_s3_class(eatPrepTBA:::generate_checkbox("Show", id = "tbl", columns = "unit_q90"), "shiny.tag")
})

test_that("compute_staytime_tables reports missing required columns for invalid inputs", {
  expect_error(
    compute_staytime_tables(
      fach = "D",
      log_times = tibble::tibble(),
      unit_domains = tibble::tibble(unit_key = "U1", subject = "D", domain = "D1"),
      final_responses = tibble::tibble(),
      units_cs = tibble::tibble(),
      unit_meta = tibble::tibble(),
      students_select = NULL,
      FS_marker = "S",
      output_path = tempdir()
    )
  )
})

test_that("test_coding_scheme returns the available check list", {
  old_reporter <- testthat::get_reporter()
  on.exit(testthat::set_reporter(old_reporter), add = TRUE)

  out <- test_coding_scheme(tibble::tibble(variable_source_type = character()), name_list = TRUE)
  testthat::set_reporter(old_reporter)

  expect_s3_class(out, "tbl_df")
  expect_true(nrow(out) >= 10)
  expect_true(all(c("Nr", "Testname") %in% names(out)))
})
