test_that("prepare_coding_scheme handles null schemes and minimal coding schemes", {
  testthat::local_mocked_bindings(
    get_dependency_tree = function(coding_scheme) {
      list(id = "v1", level = 0, sources = list(list()))
    },
    .package = "eatAutoCode"
  )

  empty <- prepare_coding_scheme(NULL)
  out <- prepare_coding_scheme(minimal_coding_scheme())

  expect_s3_class(empty, "tbl_df")
  expect_equal(nrow(empty), 0)
  expect_equal(unique(out$variable_id), "V1")
  expect_equal(out$code_id, c(1L, 0L))
  expect_equal(out$code_type, c("FULL_CREDIT", "RESIDUAL"))
})

test_that("add_coding_scheme preserves existing unit_codes unless overwrite is requested", {
  units <- minimal_units()

  out <- add_coding_scheme(units, overwrite = FALSE)

  expect_identical(out, units)
})

test_that("add_coding_scheme aborts when coding_scheme column is absent", {
  units <- minimal_units() |>
    dplyr::select(-coding_scheme, -unit_codes)

  expect_error(add_coding_scheme(units), "coding_scheme")
})

test_that("add_coding_scheme can add prepared unit code nests", {
  testthat::local_mocked_bindings(
    prepare_coding_scheme = function(coding_scheme, filter_has_codes = TRUE) {
      tibble::tibble(
        variable_id = "V1",
        variable_ref = "v1",
        variable_label = "Variable 1",
        variable_source_type = "BASE",
        variable_level = 0,
        derive_sources = list(list(NA_character_)),
        variable_sources = list(tibble::tibble()),
        variable_source_processing = list(list()),
        code_id = 1L,
        code_type = "FULL_CREDIT",
        code_label = "Right",
        code_score = 1
      )
    },
    .package = "eatPrepTBA"
  )

  units <- minimal_units() |>
    dplyr::select(-unit_codes)
  attr(units, "custom_attr") <- "kept"

  out <- add_coding_scheme(units, overwrite = TRUE)

  expect_true(tibble::has_name(out, "unit_codes"))
  expect_equal(out$unit_codes[[1]]$variable_id, "V1")
  expect_equal(attr(out, "custom_attr"), "kept")
})

test_that("add_metadata joins item and unit profile labels and preserves attributes", {
  testthat::local_mocked_bindings(
    get_metadata_profile = function(url) {
      if (identical(url, "item-profile")) {
        tibble::tibble(
          profile_name = "Difficulty",
          profile_label = "Difficulty",
          profile_type = "select",
          multiple = FALSE,
          data = list(tibble::tibble(value_id = "D1", value_label = factor("Easy")))
        )
      } else {
        tibble::tibble(
          profile_name = "Topic",
          profile_label = "Topic",
          profile_type = "select",
          multiple = FALSE,
          data = list(tibble::tibble(value_id = "T1", value_label = factor("Algebra")))
        )
      }
    },
    .package = "eatPrepTBA"
  )

  units <- tibble::tibble(
    ws_id = 1,
    unit_id = 10,
    unit_key = "U1",
    items_list = list(tibble::tibble(item_no = 1L, item_id = "I1", variable_id = "V1")),
    unit_profiles = list(tibble::tibble(profile_name = "Topic", value_id = "T1", value_text = NA_character_)),
    items_profiles = list(tibble::tibble(item_no = 1L, profile_name = "Difficulty", value_id = "D1", value_text = NA_character_))
  )
  attr(units, "ws_settings") <- tibble::tibble(
    ws_id = 1,
    item_md_profile = "item-profile",
    unit_md_profile = "unit-profile"
  )
  attr(units, "custom_attr") <- "kept"

  out <- add_metadata(units)

  expect_equal(out$item_metadata[[1]]$Difficulty, "Easy")
  expect_equal(out$unit_metadata[[1]]$Topic, "Algebra")
  expect_equal(attr(out, "custom_attr"), "kept")
  expect_s3_class(attr(out, "item_md_profile"), "tbl_df")
})

test_that("extract_metadata returns the known metadata attributes", {
  units <- tibble::tibble(unit_key = "U1")
  attr(units, "ws_settings") <- tibble::tibble(ws_id = 1)
  attr(units, "item_md_profile") <- tibble::tibble(profile_name = "item")
  attr(units, "unit_md_profile") <- tibble::tibble(profile_name = "unit")

  out <- extract_metadata(units)

  expect_named(out, c("ws_settings", "item_md_profile", "unit_md_profile"))
  expect_equal(out$ws_settings$ws_id, 1)
})

test_that("metadata helpers tolerate empty profiles and item lists", {
  metadata <- eatPrepTBA:::prepare_metadata(list(profiles = list(), items = list()))

  expect_equal(metadata$unit_profiles[[1]]$profile_name, NA_character_)
  expect_equal(metadata$items_list[[1]]$item_id, NA_character_)
  expect_equal(metadata$items_profiles[[1]]$item_no, NA_integer_)
})

test_that("codebook helpers rectangularize nested JSON codebooks and append missing codes", {
  units <- list(
    list(
      key = "U1",
      name = "Unit 1",
      variables = list(
        list(
          id = "V1",
          label = "Variable 1",
          codes = list(list(id = "1", label = "Right", description = "correct"))
        )
      )
    )
  )
  missing_codes <- list(list(id = "-99", label = "Missing", description = "omitted"))

  unit_tbl <- eatPrepTBA:::prepare_codebook_units(units)
  variable_tbl <- eatPrepTBA:::prepare_codebook_variables(unit_tbl$variables[[1]])
  code_tbl <- eatPrepTBA:::prepare_codebook_codes(variable_tbl$codes[[1]], missing_codes = missing_codes)

  expect_equal(unit_tbl$unit_key, "U1")
  expect_equal(variable_tbl$variable_id, "V1")
  expect_equal(code_tbl$code_id, c("1", "-99"))
})

test_that("prepare_codebook reads JSON files produced by download_codebook", {
  testthat::local_mocked_bindings(
    download_codebook = function(workspace, path, format, ...) {
      jsonlite::write_json(
        list(
          list(
            key = "U1",
            name = "Unit 1",
            variables = list(
              list(
                id = "V1",
                label = "Variable 1",
                codes = list(list(id = "1", label = "Right", description = "correct"))
              )
            )
          )
        ),
        file.path(path, "codebook.json"),
        auto_unbox = TRUE
      )
    },
    .package = "eatPrepTBA"
  )

  out <- prepare_codebook(
    fake_studio_workspace(),
    missings = tibble::tibble(id = "-99", label = "Missing", description = "omitted")
  )

  expect_equal(out$unit_key, c("U1", "U1"))
  expect_equal(out$code_id, c("1", "-99"))
})

test_that("source tree, simple coercion, and profile-list helpers are stable", {
  testthat::local_mocked_bindings(
    get_dependency_tree = function(coding_scheme) {
      list(
        id = c("a", "b"),
        level = c(1, 0),
        sources = list("b", character())
      )
    },
    .package = "eatAutoCode"
  )

  source_tree <- eatPrepTBA:::prepare_source_tree("scheme")
  dependencies <- tibble::tibble(
    variable_ref = c("a", "b", "c"),
    variable_level = c(1, 1, 0),
    variable_sources = list("b", "c", list())
  )

  expect_equal(source_tree$variable_ref, c("a", "b"))
  expect_true(tibble::has_name(source_tree, "variable_sources"))
  expect_equal(eatPrepTBA:::fill_source_tree(dependencies, "a"), c("a", "b", "c"))
  expect_equal(eatPrepTBA:::list_to_character(list("x", NULL)), c("x", NA_character_))
  expect_equal(eatPrepTBA:::list_to_integer(list("1", NULL)), c(1L, NA_integer_))
  expect_equal(eatPrepTBA:::coerce_list("x"), list("x"))
  expect_equal(eatPrepTBA:::pad_ids(c("A", "LONG")), c("A   ", "LONG"))

  nested <- list(
    list(id = "A", prefLabel.de = "A", narrower = list(list(id = "B", prefLabel.de = "B")))
  )
  out <- eatPrepTBA:::narrow_profile_list(nested)
  expect_equal(out$id, c("A", "B"))
})

test_that("definition and deepest-element helpers extract nested structure", {
  testthat::local_mocked_bindings(
    extract_variable_location = function(resp_definition, missing = NULL) {
      tibble::tibble(
        variable_ref = "V1",
        variable_page_always_visible = FALSE,
        variable_dependencies = list(tibble::tibble()),
        variable_path = list(
          tibble::tibble(
            pages = 1,
            sections = 2,
            elements = 3,
            content = "item"
          )
        )
      )
    },
    .package = "eatAutoCode"
  )

  definition <- eatPrepTBA:::prepare_definition("definition")
  nested <- list(
    id = "root",
    child = list(id = "child", skip = list(id = "hidden"))
  )

  expect_equal(definition$unit_definition, "definition")
  expect_equal(definition$variable_pages[[1]]$variable_ref, "V1")
  expect_equal(unlist(eatPrepTBA:::get_deepest_elements(nested, "id")), c("root", "child", "hidden"))
  expect_equal(unname(unlist(eatPrepTBA:::get_deepest_elements(nested, "id", no_parent = "skip"))), c("root", "child"))
})

test_that("generate_base_req creates httr2 request builders for Studio and Testcenter", {
  studio_req <- eatPrepTBA:::generate_base_req(
    type = "studio",
    base_url = "https://studio.example/",
    auth_token = "Bearer token",
    app_version = "16.0.2"
  )("GET", c("workspaces", 1), query = list(q = "x"))

  tc_req <- eatPrepTBA:::generate_base_req(
    type = "testcenter",
    base_url = "https://tc.example/",
    auth_token = "token",
    insecure = TRUE
  )("POST", c("workspace", 7, "files"))

  expect_s3_class(studio_req, "httr2_request")
  expect_s3_class(tc_req, "httr2_request")
  expect_equal(studio_req$method, "GET")
  expect_equal(tc_req$method, "POST")
})
