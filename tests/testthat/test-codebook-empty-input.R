mock_codebook_units <- function() {
  state <- new.env(parent = emptyenv())
  state$requests <- list()
  testthat::local_mocked_bindings(
    list_units = function(workspace) {
      list(
        list(ws_id = 1, ws_label = "Workspace 1",
             units = list(list(unit_id = 10L, unit_key = "U1"))),
        list(ws_id = 2, ws_label = "Workspace 2",
             units = list(list(unit_id = 20L, unit_key = "U2")))
      )
    }, .package = "eatPrepTBA", .env = parent.frame()
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      state$requests[[length(state$requests) + 1L]] <- req
      invisible(NULL)
    }, .package = "httr2", .env = parent.frame()
  )
  state
}

test_that("unknown unit keys fail before any codebook export", {
  state <- mock_codebook_units()
  for (keys in list("u1", "Unit 1", "10", c("U1", "unknown"))) {
    expect_error(prepare_codebook(fake_studio_workspace(), unit_keys = keys),
                 "Unknown.*unit_keys")
  }
  expect_length(state$requests, 0)
})

test_that("empty and invalid unit selections have argument errors", {
  state <- mock_codebook_units()
  for (keys in list(character(), "", NA_character_, 10, list("U1"))) {
    expect_error(prepare_codebook(fake_studio_workspace(), unit_keys = keys),
                 "unit_keys")
  }
  expect_length(state$requests, 0)
})

test_that("unit selection spans workspaces and skips workspaces without matches", {
  state <- mock_codebook_units()
  download_codebook(fake_studio_workspace(), tempdir(), unit_keys = "U2", format = "json")
  expect_length(state$requests, 1)
  expect_equal(state$requests[[1]]$endpoint, c("workspaces", "2", "units", "coding-book"))
  expect_equal(state$requests[[1]]$query$id, "20")

  download_codebook(fake_studio_workspace(), tempdir(), unit_keys = c("U1", "U2"), format = "json")
  expect_length(state$requests, 3)
  expect_equal(state$requests[[2]]$query$id, "10")
  expect_equal(state$requests[[3]]$query$id, "20")
})

test_that("workspaces without units fail before downloading", {
  state <- mock_codebook_units()
  testthat::local_mocked_bindings(
    list_units = function(workspace) {
      list(list(ws_id = 1, ws_label = "Workspace 1", units = list()))
    }, .package = "eatPrepTBA"
  )
  expect_error(prepare_codebook(fake_studio_workspace()), "No units are available")
  expect_length(state$requests, 0)
})

test_that("download failures retain the API error and stop partial preparation", {
  state <- mock_codebook_units()
  paths <- character()
  testthat::local_mocked_bindings(
    req_perform = function(req, path, ...) {
      paths <<- c(paths, path)
      if (length(paths) == 2L) stop("HTTP 403 Forbidden")
      jsonlite::write_json(list(list(key = "U1", name = "Unit 1", variables = list())), path)
    }, .package = "httr2"
  )
  error <- tryCatch(prepare_codebook(fake_studio_workspace()), error = identity)
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "Workspace 2.*could not be downloaded")
  expect_match(conditionMessage(error$parent), "HTTP 403 Forbidden")
  expect_length(paths, 2)
  expect_false(any(dir.exists(dirname(paths))))
})

test_that("preparation explains a missing download and removes its temporary directory", {
  download_path <- NULL
  testthat::local_mocked_bindings(
    download_codebook = function(workspace, path, ...) {
      download_path <<- path
      invisible(NULL)
    }, .package = "eatPrepTBA"
  )
  expect_error(prepare_codebook(fake_studio_workspace()), "No JSON codebook was downloaded")
  expect_false(dir.exists(download_path))
})

test_that("empty Studio exports identify the empty level", {
  for (case in c("units", "variables", "codes")) {
    variable <- list(id = "V1", label = "Variable 1", codes = list())
    unit <- list(key = "U1", name = "Unit 1", variables = list(variable))
    payload <- switch(case,
                      units = list(),
                      variables = list(within(unit, variables <- list())),
                      codes = list(unit))
    testthat::local_mocked_bindings(
      download_codebook = function(workspace, path, ...) {
        jsonlite::write_json(payload, file.path(path, "codebook.json"), auto_unbox = TRUE)
      }, .package = "eatPrepTBA"
    )
    expect_error(prepare_codebook(fake_studio_workspace(), only_coded = TRUE),
                 paste("contains no", case))
  }
})

test_that("empty exports and units can accompany nonempty codebooks", {
  testthat::local_mocked_bindings(
    download_codebook = function(workspace, path, ...) {
      variable <- list(id = "V1", label = "Variable 1",
                       codes = list(list(id = "1", label = "Correct", description = "Correct answer")))
      jsonlite::write_json(list(), file.path(path, "empty.json"))
      jsonlite::write_json(
        list(list(key = "U0", name = "Empty unit", variables = list()),
             list(key = "U1", name = "Unit 1", variables = list(variable))),
        file.path(path, "codebook.json"), auto_unbox = TRUE
      )
    }, .package = "eatPrepTBA"
  )
  out <- prepare_codebook(fake_studio_workspace())
  expect_equal(out$unit_key, "U1")
  expect_equal(out$variable_id, "V1")
  expect_equal(out$code_id, "1")
})
