studio_version_response <- function(req, status = 200L) {
  body <- if (status != 200L) {
    list(statusCode = status, message = "Unexpected app version")
  } else if (endsWith(req$url, "/api/login")) {
    list(accessToken = "test-token")
  } else {
    list(userId = 99, userName = "tester", userLongName = "Test User",
         workspaces = list())
  }
  httr2::response(
    status_code = status,
    url = req$url,
    headers = list(`content-type` = "application/json"),
    body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
  )
}

test_that("Studio login keeps the exact instance version on every request", {
  testthat::local_mocked_bindings(
    get_credentials = function(...) list(name = "user", password = "pw"),
    .package = "eatPrepTBA"
  )
  requests <- list()
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      requests[[length(requests) + 1L]] <<- req
      studio_version_response(req)
    },
    .package = "httr2"
  )

  for (version in c("20.1.0", "21.0.0-beta.1", "21.0.0")) {
    requests <- list()
    login <- login_studio("https://studio.example/", app_version = version,
                          dialog = FALSE)
    expect_length(requests, 2L)
    expect_equal(vapply(requests, function(req) req$headers[["app-version"]], ""),
                 rep(version, 2L))
    expect_equal(login@app_version, version)
    subsequent <- login@base_req("GET", c("workspaces", 1, "units"))
    expect_equal(subsequent$headers[["app-version"]], version)
  }
})

test_that("Studio HTTP 521 identifies a version mismatch at either login step", {
  testthat::local_mocked_bindings(
    get_credentials = function(...) list(name = "user", password = "pw"),
    .package = "eatPrepTBA"
  )
  failed_endpoint <- NULL
  requests <- list()
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      requests[[length(requests) + 1L]] <<- req
      status <- if (endsWith(req$url, failed_endpoint)) 521L else 200L
      response <- studio_version_response(req, status)
      httr2::resp_check_status(response)
      response
    },
    .package = "httr2"
  )

  for (endpoint in c("/api/login", "/api/auth-data")) {
    failed_endpoint <- endpoint
    requests <- list()
    error <- expect_error(
      login_studio("https://studio.example/", app_version = "20.1.0",
                   dialog = FALSE),
      "exact version",
      class = "eatPrepTBA_studio_version_mismatch"
    )
    expect_equal(error$app_version, "20.1.0")
    expect_equal(error$base_url, "https://studio.example/")
    expect_equal(error$status, 521L)
    expect_s3_class(error$parent, "httr2_http_521")
    expect_length(requests, if (endpoint == "/api/login") 1L else 2L)
  }
})

test_that("Studio authentication errors retain their HTTP classification", {
  testthat::local_mocked_bindings(
    get_credentials = function(...) list(name = "user", password = "pw"),
    .package = "eatPrepTBA"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::resp_check_status(studio_version_response(req, 401L))
    },
    .package = "httr2"
  )
  error <- expect_error(
    login_studio("https://studio.example/", app_version = "21.0.0", dialog = FALSE),
    class = "httr2_http_401"
  )
  expect_false(inherits(error, "eatPrepTBA_studio_version_mismatch"))
})
