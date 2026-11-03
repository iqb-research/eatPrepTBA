local_design_testcenter <- function(unit_keys, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    get_testtakers = function(workspace) {
      tibble::tibble(
        group_id = "G1", login_name = c("P1", "P2", "Preview"),
        login_code = c("C1", "C2", "Preview"),
        login_mode = c("run-hot-return", "run-hot-return", "run-review"),
        booklet_id = "B1", booklet_no = 1L
      )
    },
    get_booklets = function(workspace) {
      tibble::tibble(
        booklet_id = "B1", testlet_id = "T1", testlet_no = 1L,
        unit_key = unit_keys, unit_alias = paste0(unit_keys, "_alias"),
        unit_testlet_no = seq_along(unit_keys),
        unit_booklet_no = seq_along(unit_keys)
      )
    },
    .package = "eatPrepTBA", .env = .env
  )
}

design_unit_without_scheme <- function(key = "S1", scheme = NA_character_) {
  tibble::tibble(
    ws_id = 1L, unit_id = 20L, unit_key = key,
    coding_scheme = list(scheme),
    unit_variables = list(tibble::tibble(
      variable_id = c("SCORE", "UNOBSERVED"),
      variable_ref = c("score-ref", "unobserved-ref"),
      variable_type = c("coded", "string"), variable_format = ""
    ))
  )
}

design_unit_with_scheme <- function(only_excluded = FALSE) {
  unit <- minimal_units()
  unit$unit_codes <- NULL
  coding <- jsonlite::parse_json(minimal_coding_scheme())$variableCodings[[1]]
  derived <- coding
  derived$id <- "derived-ref"
  derived$alias <- "DERIVED"
  derived$sourceType <- "COPY_VALUE"
  derived$deriveSources <- list("v1")
  excluded <- coding
  excluded$id <- "excluded-ref"
  excluded$alias <- "EXCLUDED"
  excluded$sourceType <- "BASE_NO_VALUE"
  codings <- if (only_excluded) list(excluded) else list(coding, derived, excluded)
  unit$coding_scheme <- list(as.character(jsonlite::toJSON(
    list(variableCodings = codings), auto_unbox = TRUE, null = "null"
  )))
  unit$unit_variables <- list(tibble::tibble(
    variable_id = c("V1", "EXCLUDED", "NOT_IN_SCHEME"),
    variable_ref = c("v1", "excluded-ref", "extra-ref"),
    variable_type = "string", variable_format = ""
  ))
  unit
}

test_that("get_design without units remains a unit-level design", {
  local_design_testcenter(c("U1", "S1"))
  design <- get_design(fake_testcenter_workspace())

  expect_false("variable_id" %in% names(design))
  expect_equal(nrow(design), 4L)
  expect_setequal(design$login_name, c("P1", "P2"))
  expect_equal(design$unit_alias, rep(c("U1_alias", "S1_alias"), 2L))

  preview <- get_design(fake_testcenter_workspace(), mode = "run-review")
  expect_equal(preview$login_name, rep("Preview", 2L))
})

test_that("get_design uses declared variables without needing observed responses", {
  local_design_testcenter("S1")
  # Exercise the metadata reader's alias/id normalization as well as the design.
  units <- eatPrepTBA:::read_units(list(list(
    id = 20L, key = "S1", name = "Stored-code unit", scheme = "",
    variables = list(
      list(id = "opaque-ref", alias = "SCORE", type = "CODED"),
      list(id = "UNOBSERVED", type = "STRING")
    )
  )), ws_id = 1L)
  coded <- prepare_coded(tibble::tibble(coded = as.character(jsonlite::toJSON(
    list(list(id = "SCORE", value = "A", code = 1, score = 1,
              status = "CODING_COMPLETE")), auto_unbox = TRUE
  ))))

  design <- get_design(fake_testcenter_workspace(), units = units)

  expect_equal(design$variable_id, rep(c("SCORE", "UNOBSERVED"), 2L))
  expect_true(all(coded$variable_id %in% design$variable_id))
  expect_false("opaque-ref" %in% design$variable_id)
  expect_equal(design$unit_alias, rep("S1_alias", 4L))
  expect_equal(design$unit_booklet_no, rep(1L, 4L))

  preview <- get_design(fake_testcenter_workspace(), units = units, mode = "run-review")
  expect_equal(preview$login_name, rep("Preview", 2L))
  expect_equal(preview$variable_id, c("SCORE", "UNOBSERVED"))
})

test_that("get_design accepts missing and empty coding schemes per unit", {
  local_design_testcenter("S1")
  schemes <- list(NULL, NA_character_, "", "NA", "null", "{}",
                  '{"variableCodings":[]}', '{"variableCodings":null}')
  for (scheme in schemes) {
    units <- design_unit_without_scheme(scheme = scheme)
    design <- get_design(fake_testcenter_workspace(), units = units)
    expect_equal(design$variable_id, rep(c("SCORE", "UNOBSERVED"), 2L))
  }

  units <- design_unit_without_scheme()
  units$coding_scheme <- NULL
  design <- get_design(fake_testcenter_workspace(), units = units)
  expect_equal(design$variable_id, rep(c("SCORE", "UNOBSERVED"), 2L))
})

test_that("get_design combines schemes and metadata without restoring excluded variables", {
  local_design_testcenter(c("U1", "S1", "OTHER"))
  units <- dplyr::bind_rows(design_unit_with_scheme(), design_unit_without_scheme())

  design <- get_design(fake_testcenter_workspace(), units = units)

  expect_equal(nrow(design), 8L)
  expect_equal(design$variable_id[design$unit_key == "U1"],
               rep(c("V1", "DERIVED"), 2L))
  expect_equal(design$variable_id[design$unit_key == "S1"],
               rep(c("SCORE", "UNOBSERVED"), 2L))
  expect_false(any(c("EXCLUDED", "NOT_IN_SCHEME") %in% design$variable_id))
  expect_false("OTHER" %in% design$unit_key)
  expect_setequal(design$login_name, c("P1", "P2"))
})

test_that("get_design does not fall back when all scheme variables are excluded", {
  local_design_testcenter(c("U1", "S1"))
  units <- design_unit_with_scheme(only_excluded = TRUE)

  design <- get_design(fake_testcenter_workspace(), units = units)
  expect_equal(nrow(design), 0L)
  expect_type(design$variable_id, "character")

  mixed <- dplyr::bind_rows(units, design_unit_without_scheme())
  design <- get_design(fake_testcenter_workspace(), units = mixed)
  expect_equal(design$unit_key, rep("S1", 4L))
  expect_equal(design$variable_id, rep(c("SCORE", "UNOBSERVED"), 2L))
})

test_that("get_design honors cached unit codes and overwrite", {
  local_design_testcenter(c("U1", "S1"))
  units <- dplyr::bind_rows(design_unit_with_scheme(), design_unit_without_scheme())
  units$unit_codes <- list(tibble::tibble(variable_id = "CACHED"), NULL)

  cached <- get_design(fake_testcenter_workspace(), units = units)
  expect_equal(cached$variable_id[cached$unit_key == "U1"], rep("CACHED", 2L))
  expect_equal(cached$variable_id[cached$unit_key == "S1"],
               rep(c("SCORE", "UNOBSERVED"), 2L))

  rebuilt <- get_design(fake_testcenter_workspace(), units = units, overwrite = TRUE)
  expect_equal(rebuilt$variable_id[rebuilt$unit_key == "U1"],
               rep(c("V1", "DERIVED"), 2L))
  expect_equal(rebuilt$variable_id[rebuilt$unit_key == "S1"],
               rep(c("SCORE", "UNOBSERVED"), 2L))

  cached_only <- tibble::tibble(unit_key = "S1",
                              unit_codes = list(tibble::tibble(variable_id = "ONLY")))
  design <- get_design(fake_testcenter_workspace(), units = cached_only)
  expect_equal(design$variable_id, rep("ONLY", 2L))

  no_scheme <- design_unit_without_scheme()
  no_scheme$unit_codes <- list(tibble::tibble(variable_id = "STORED"))
  design <- get_design(fake_testcenter_workspace(), units = no_scheme)
  expect_equal(design$variable_id, rep("STORED", 2L))
  design <- get_design(fake_testcenter_workspace(), units = no_scheme, overwrite = TRUE)
  expect_equal(design$variable_id, rep(c("SCORE", "UNOBSERVED"), 2L))

  empty_cache <- design_unit_with_scheme()
  empty_cache$unit_codes <- list(tibble::tibble(variable_id = character()))
  design <- get_design(fake_testcenter_workspace(), units = empty_cache)
  expect_equal(nrow(design), 0L)
})

test_that("get_design deduplicates metadata IDs and omits missing or blank IDs", {
  local_design_testcenter("S1")
  units <- design_unit_without_scheme()
  units$unit_variables <- list(tibble::tibble(
    variable_id = c("SCORE", "SCORE", NA_character_, "", " \t", "UNOBSERVED")
  ))

  design <- get_design(fake_testcenter_workspace(), units = units)
  expect_equal(design$variable_id, rep(c("SCORE", "UNOBSERVED"), 2L))
})

test_that("get_design handles schema-less units with no declared variables", {
  local_design_testcenter("S1")
  for (variables in list(NULL, list(), tibble::tibble(),
                         tibble::tibble(variable_id = character()))) {
    units <- design_unit_without_scheme()
    units$unit_variables <- list(variables)
    design <- get_design(fake_testcenter_workspace(), units = units)
    expect_equal(nrow(design), 0L)
    expect_type(design$variable_id, "character")
  }

  design <- get_design(fake_testcenter_workspace(), units = tibble::tibble(unit_key = character()))
  expect_equal(nrow(design), 0L)
  expect_type(design$variable_id, "character")
})

test_that("get_design does not treat malformed coding schemes as absent", {
  local_design_testcenter("S1")
  units <- design_unit_without_scheme(scheme = "not valid JSON")
  expect_error(get_design(fake_testcenter_workspace(), units = units), "(JSON|json|parse|lexical)")
})
