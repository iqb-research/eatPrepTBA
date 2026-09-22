mock_codebook_download <- function(profiles = list(list(label = "IQB-Standard"))) {
  state <- new.env(parent = emptyenv())
  state$requests <- list()
  testthat::local_mocked_bindings(
    list_units = function(workspace) {
      list(list(ws_id = 1, ws_label = "Workspace 1",
                units = list(list(unit_id = 10, unit_key = "U1"))))
    },
    .package = "eatPrepTBA", .env = parent.frame()
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      state$requests[[length(state$requests) + 1L]] <- req
      req
    },
    resp_body_json = function(resp, ...) {
      force(resp)
      profiles
    },
    .package = "httr2", .env = parent.frame()
  )
  state
}

test_that("codebook downloads validate profile labels and preserve their case", {
  label <- "IQB-Standard & Project A"
  state <- mock_codebook_download(list(list(label = label)))

  for (format in c("json", "docx")) {
    download_codebook(fake_studio_workspace(), tempdir(), format = format,
                      missings_profile = label)
  }

  expect_length(state$requests, 4)
  expect_equal(state$requests[[1]]$endpoint,
               c("admin", "settings", "missings-profiles"))
  expect_equal(state$requests[[2]]$query$missingsProfile, label)
  expect_equal(state$requests[[4]]$query$missingsProfile, label)
  expect_equal(state$requests[[2]]$query$format, "json")
  expect_equal(state$requests[[4]]$query$format, "docx")
  expect_equal(state$requests[[2]]$query$onlyManual, "true")
})

test_that("default downloads do not query or select a missing-value profile", {
  state <- mock_codebook_download()
  download_codebook(fake_studio_workspace(), tempdir(), format = "json")

  expect_length(state$requests, 1)
  expect_equal(tail(state$requests[[1]]$endpoint, 1), "coding-book")
  expect_false("missingsProfile" %in% names(state$requests[[1]]$query))
})

test_that("unknown profiles fail before exporting rather than silently omitting codes", {
  state <- mock_codebook_download()
  expect_error(download_codebook(fake_studio_workspace(), tempdir(),
                                missings_profile = "iqb-standard"),
               "Unknown Studio missing-value profile.*iqb-standard")
  expect_length(state$requests, 1)
})

test_that("an empty profile registry and invalid arguments have explicit errors", {
  state <- mock_codebook_download(list())
  expect_error(download_codebook(fake_studio_workspace(), tempdir(),
                                missings_profile = "IQB-Standard"),
               "Available profiles: none")
  expect_length(state$requests, 1)
  for (profile in list("", NA_character_, c("a", "b"), 1)) {
    expect_error(download_codebook(fake_studio_workspace(), tempdir(),
                                  missings_profile = profile),
                 "missings_profile")
  }
  expect_length(state$requests, 1)
})

test_that("profile lookup failures do not fall back to an unprofiled download", {
  state <- mock_codebook_download()
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) stop("HTTP 403 Forbidden"),
    .package = "httr2"
  )
  expect_error(download_codebook(fake_studio_workspace(), tempdir(),
                                missings_profile = "IQB-Standard"), "HTTP 403")
})

codebook_unit_with_profile <- function(key, code = -97, variables = c("V1", "V2")) {
  list(
    key = key, name = paste("Unit", key),
    variables = lapply(variables, function(id) {
      list(id = id, label = id,
           codes = list(list(id = "1", label = "Correct", description = "Correct answer")))
    }),
    missings = list(list(id = "technical-profile-entry-id", code = code,
                         label = "Profile missing", description = "From Studio"))
  )
}

test_that("profile codes are added to every variable of their unit with numeric code IDs", {
  testthat::local_mocked_bindings(
    download_codebook = function(workspace, path, format, missings_profile, ...) {
      expect_equal(missings_profile, "IQB-Standard")
      jsonlite::write_json(list(codebook_unit_with_profile("U1"),
                               codebook_unit_with_profile("U2", code = -98)),
                          file.path(path, "codebook.json"), auto_unbox = TRUE)
    }, .package = "eatPrepTBA"
  )
  out <- prepare_codebook(fake_studio_workspace(), missings_profile = "IQB-Standard")
  expect_equal(nrow(out), 8)
  expect_equal(out$code_id[out$unit_key == "U1"], c("1", "-97", "1", "-97"))
  expect_equal(out$code_id[out$unit_key == "U2"], c("1", "-98", "1", "-98"))
  expect_equal(names(out), c("unit_key", "unit_label", "variable_id", "variable_label",
                            "code_id", "code_label", "code_description"))
})

test_that("user missing codes override matching profile codes and supplement the rest", {
  testthat::local_mocked_bindings(
    download_codebook = function(workspace, path, ...) {
      unit <- codebook_unit_with_profile("U1", variables = "V1")
      unit$missings[[2]] <- list(code = -98, label = "Retained", description = "Keep me")
      jsonlite::write_json(list(unit), file.path(path, "codebook.json"), auto_unbox = TRUE)
    }, .package = "eatPrepTBA"
  )
  own <- tibble::tibble(id = c("-97", "-99"), label = c("Own replacement", "Own extra"),
                       description = c("Replace me", "Append me"))
  out <- prepare_codebook(fake_studio_workspace(), missings_profile = "IQB-Standard",
                          missings = own)
  expect_equal(out$code_id, c("1", "-98", "-97", "-99"))
  expect_equal(out$code_label, c("Correct", "Retained", "Own replacement", "Own extra"))
  expect_equal(out$code_description, c("Correct answer", "Keep me", "Replace me", "Append me"))
  expect_false(anyDuplicated(out$code_id) > 0)
})

test_that("empty or absent profile codes preserve ordinary codebooks", {
  for (profile_codes in list(NULL, list())) {
    testthat::local_mocked_bindings(
      download_codebook = function(workspace, path, ...) {
        unit <- codebook_unit_with_profile("U1", variables = "V1")
        unit$missings <- profile_codes
        jsonlite::write_json(list(unit), file.path(path, "codebook.json"), auto_unbox = TRUE)
      }, .package = "eatPrepTBA"
    )
    out <- prepare_codebook(fake_studio_workspace())
    expect_equal(out$code_id, "1")
    expect_equal(out$code_label, "Correct")
  }
})

test_that("variables without regular codes can still receive profile codes", {
  testthat::local_mocked_bindings(
    download_codebook = function(workspace, path, ...) {
      unit <- codebook_unit_with_profile("U1", variables = "V1")
      unit$variables[[1]]$codes <- list()
      jsonlite::write_json(list(unit), file.path(path, "codebook.json"), auto_unbox = TRUE)
    }, .package = "eatPrepTBA"
  )
  out <- prepare_codebook(fake_studio_workspace(), missings_profile = "IQB-Standard")
  expect_equal(out$variable_id, "V1")
  expect_equal(out$code_id, "-97")
})

test_that("each preparation uses its own directory and cleans up even on failure", {
  paths <- character()
  testthat::local_mocked_bindings(
    download_codebook = function(workspace, path, ...) {
      paths <<- c(paths, path)
      unit <- codebook_unit_with_profile("U1", variables = "V1")
      if (length(paths) == 2) unit$missings[[1]]$code <- NULL
      jsonlite::write_json(list(unit), file.path(path, "codebook.json"), auto_unbox = TRUE)
    }, .package = "eatPrepTBA"
  )
  expect_no_error(prepare_codebook(fake_studio_workspace(), missings_profile = "IQB-Standard"))
  expect_error(prepare_codebook(fake_studio_workspace(), missings_profile = "IQB-Standard"),
               "no valid.*code")
  expect_length(unique(paths), 2)
  expect_false(any(dir.exists(paths)))
})

test_that("the full preparation path sends an encoded profile label and consumes Studio JSON", {
  label <- "IQB-Standard & Project A"
  requests <- list()
  login <- fake_studio_login(eatPrepTBA:::generate_base_req(
    "studio", "https://studio.example/", "Bearer test-token", "20.0.1"
  ))
  testthat::local_mocked_bindings(
    list_units = function(workspace) {
      list(list(ws_id = 1, ws_label = "Workspace 1",
                units = list(list(unit_id = 10, unit_key = "U1"))))
    }, .package = "eatPrepTBA"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      requests[[length(requests) + 1L]] <<- req
      if (is.null(path)) {
        body <- jsonlite::toJSON(list(list(label = label)), auto_unbox = TRUE)
        return(httr2::response(status_code = 200,
                               headers = list(`content-type` = "application/json"),
                               body = charToRaw(body)))
      }
      jsonlite::write_json(list(codebook_unit_with_profile("U1", variables = "V1")),
                          path, auto_unbox = TRUE)
      invisible(NULL)
    }, .package = "httr2"
  )
  out <- prepare_codebook(fake_studio_workspace(login), missings_profile = label)
  expect_length(requests, 2)
  expect_match(requests[[1]]$url, "/api/admin/settings/missings-profiles", fixed = TRUE)
  expect_match(requests[[2]]$url, "%26", fixed = TRUE)
  expect_match(curl::curl_unescape(requests[[2]]$url), paste0("missingsProfile=", label),
               fixed = TRUE)
  expect_equal(out$code_id, c("1", "-97"))
})
