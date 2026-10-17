studio_property_workspace <- function() {
  base_req <- eatPrepTBA:::generate_base_req(
    type = "studio",
    base_url = "https://studio.example/",
    auth_token = "test-token",
    app_version = "21.0.0"
  )
  fake_studio_workspace(fake_studio_login(base_req = base_req))
}

test_that("duplicate key conflicts reject the whole settings request and can be caught", {
  requests <- list()
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      requests[[length(requests) + 1L]] <<- req
      response <- httr2::response(status_code = 409L, url = req$url)
      httr2::resp_check_status(response)
    },
    .package = "httr2"
  )

  error <- tryCatch(
    eatPrepTBA:::change_unit_settings(
      studio_property_workspace(),
      unit_id = 10,
      unit_key = "Existing_Key",
      unit_name = "Renamed unit",
      description = "New description",
      editor = "3.2.1"
    ),
    eatPrepTBA_unit_key_conflict = identity
  )

  expect_s3_class(error, "eatPrepTBA_unit_key_conflict")
  expect_s3_class(error$parent, "httr2_http_409")
  expect_equal(error$status, 409L)
  expect_equal(error$unit_id, 10)
  expect_equal(error$unit_key, "Existing_Key")
  expect_equal(error$ws_id, 1)
  expect_match(conditionMessage(error), "already in use in workspace 1")
  expect_match(conditionMessage(error), "unit 10; HTTP 409", fixed = TRUE)
  expect_match(conditionMessage(error), "None of the settings in this request were changed", fixed = TRUE)
  expect_length(requests, 1L)
  expect_equal(requests[[1]]$method, "PATCH")
  expect_equal(requests[[1]]$url, "https://studio.example/api/workspaces/1/units/10/properties")
  expect_equal(requests[[1]]$body$data, list(
    id = 10,
    key = "Existing_Key",
    name = "Renamed unit",
    description = "New description",
    editor = glue::glue("iqb-editor-aspect@3.2")
  ))
})

test_that("successful settings changes use a single server-validated PATCH", {
  requests <- list()
  response <- httr2::response(status_code = 200L)
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      requests[[length(requests) + 1L]] <<- req
      response
    },
    .package = "httr2"
  )

  result <- eatPrepTBA:::change_unit_settings(
    studio_property_workspace(),
    unit_id = 10,
    unit_key = "existing_key",
    unit_name = "Updated name"
  )

  expect_identical(result, response)
  expect_length(requests, 1L)
  expect_equal(requests[[1]]$method, "PATCH")
  expect_equal(requests[[1]]$body$data$key, "existing_key")
  expect_equal(requests[[1]]$body$data$name, "Updated name")
})

test_that("conflicts without a submitted valid key retain generic error handling", {
  requests <- list()
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      requests[[length(requests) + 1L]] <<- req
      httr2::resp_check_status(httr2::response(status_code = 409L, url = req$url))
    },
    .package = "httr2"
  )

  for (unit_key in list(NULL, "A")) {
    messages <- testthat::capture_messages(
      expect_no_error(eatPrepTBA:::change_unit_settings(
        studio_property_workspace(),
        unit_id = 10,
        unit_key = unit_key,
        unit_name = "Updated name"
      ))
    )
    expect_match(paste(messages, collapse = " "), "Settings could not be changed")
    expect_match(paste(messages, collapse = " "), "409")
    expect_false(any(grepl("already in use", messages, fixed = TRUE)))
  }
  expect_length(requests, 2L)
  expect_true(all(vapply(requests, function(req) is.null(req$body$data$key), logical(1))))
})

test_that("other HTTP failures retain generic error handling even with a key", {
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::resp_check_status(httr2::response(status_code = 403L, url = req$url))
    },
    .package = "httr2"
  )

  messages <- testthat::capture_messages(
    expect_no_error(eatPrepTBA:::change_unit_settings(
      studio_property_workspace(),
      unit_id = 10,
      unit_key = "Existing_Key",
      unit_name = "Updated name"
    ))
  )
  expect_match(paste(messages, collapse = " "), "Settings could not be changed")
  expect_match(paste(messages, collapse = " "), "403")
  expect_false(any(grepl("already in use", messages, fixed = TRUE)))
})
