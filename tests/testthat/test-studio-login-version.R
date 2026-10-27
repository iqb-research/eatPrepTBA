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
    get_studio_version = function(...) stop("version detection should not run"),
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

test_that("Studio version detection reads the version from the main script", {
  testthat::local_mocked_bindings(
    request = function(base_url) list(url = base_url),
    req_perform = function(req, ...) req,
    resp_body_string = function(resp, ...) {
      if (identical(resp$url, "https://studio.example/")) {
        return('<html><script src="/assets/main-test.js"></script></html>')
      }
      'provide:"APP_VERSION",useValue:hi;var hi="21.0.0";'
    },
    .package = "httr2"
  )

  expect_equal(get_studio_version("https://studio.example/"), "21.0.0")
})

test_that("Studio version detection handles JavaScript declaration variants", {
  script <- NULL
  testthat::local_mocked_bindings(
    request = function(base_url) list(url = base_url),
    req_perform = function(req, ...) req,
    resp_body_string = function(resp, ...) {
      if (identical(resp$url, "https://studio.example/")) {
        return('<html><script src="/assets/main-test.js"></script></html>')
      }
      script
    },
    .package = "httr2"
  )

  scripts <- c(
    'provide:"APP_VERSION",useValue:hi;const hi="21.0.0";',
    'provide:"APP_VERSION",useValue:hi;let hi="21.0.0";',
    'provide:"APP_VERSION",useValue:hi;var other="x",hi="21.0.0";',
    'provide:"APP_VERSION",useValue:$version;const $version="21.0.0";'
  )
  for (variant in scripts) {
    script <- variant
    expect_equal(get_studio_version("https://studio.example/"), "21.0.0")
  }
})

test_that("Studio version detection reads literal versions from older bundles", {
  script <- NULL
  testthat::local_mocked_bindings(
    request = function(base_url) list(url = base_url),
    req_perform = function(req, ...) req,
    resp_body_string = function(resp, ...) {
      if (identical(resp$url, "https://studio.example/")) {
        return('<html><script src="/assets/main-test.js"></script></html>')
      }
      script
    },
    .package = "httr2"
  )

  scripts <- c(
    'provide:"APP_VERSION",useValue:"16.0.0";',
    "provide:'APP_VERSION',useValue:'20.0.0';"
  )
  for (i in seq_along(scripts)) {
    script <- scripts[i]
    expect_equal(
      get_studio_version("https://studio.example/"),
      c("16.0.0", "20.0.0")[i]
    )
  }
})

test_that("Studio version detection reports missing scripts and versions", {
  page <- '<html><script src="/assets/main-test.js"></script></html>'
  script <- 'provide:"APP_VERSION",useValue:hi;var hi="21.0.0";'
  testthat::local_mocked_bindings(
    request = function(base_url) list(url = base_url),
    req_perform = function(req, ...) req,
    resp_body_string = function(resp, ...) {
      if (identical(resp$url, "https://studio.example/")) page else script
    },
    .package = "httr2"
  )

  page <- "<html></html>"
  expect_error(get_studio_version("https://studio.example/"),
               "Keine JavaScript-Hauptdatei")

  page <- '<html><script src="/assets/main-test.js"></script></html>'
  script <- 'provide:"APP_VERSION",useValue:hi;'
  expect_error(get_studio_version("https://studio.example/"),
               "Versionsnummer konnte nicht ausgelesen werden")
})

test_that("detected Studio version is used in login and API headers", {
  testthat::local_mocked_bindings(
    get_studio_version = function(base_url) "16.0.0",
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

  login <- login_studio("https://studio.example/", dialog = FALSE)
  subsequent <- login@base_req("GET", "workspaces")

  expect_equal(login@app_version, "16.0.0")
  expect_equal(vapply(requests, function(req) req$headers[["app-version"]], ""),
               c("16.0.0", "16.0.0"))
  expect_equal(subsequent$headers[["app-version"]], "16.0.0")
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
