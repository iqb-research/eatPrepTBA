test_that("S4 login and workspace classes dispatch access_workspace", {
  studio_login <- fake_studio_login()
  testcenter_login <- fake_testcenter_login()

  studio_workspace <- access_workspace(studio_login, ws_id = c(1, 2))
  testcenter_workspace <- access_workspace(testcenter_login, ws_id = 7)

  expect_s4_class(studio_workspace, "WorkspaceStudio")
  expect_equal(studio_workspace@ws_id, c(1, 2))
  expect_s4_class(testcenter_workspace, "WorkspaceTestcenter")
  expect_equal(testcenter_workspace@ws_label, "TC Workspace")
  expect_error(access_workspace(studio_login, ws_id = 99), "does not match")
})

test_that("show methods emit informative output for S4 objects", {
  expect_no_error(show(fake_studio_login()))
  expect_no_error(show(fake_testcenter_login()))
  expect_no_error(show(fake_studio_workspace()))
  expect_no_error(show(fake_testcenter_workspace()))
})

test_that("login_studio and login_testcenter build invisible login objects from mocked API responses", {
  testthat::local_mocked_bindings(
    get_credentials = function(...) list(name = "user", password = "pw"),
    generate_base_req = function(type, base_url, auth_token, app_version = NULL, insecure = FALSE) {
      function(method, endpoint, query = NULL) list(type = type, method = method, endpoint = endpoint)
    },
    .package = "eatPrepTBA"
  )
  testthat::local_mocked_bindings(
    request = function(base_url) list(base_url = base_url),
    req_url_path_append = function(req, ...) c(req, list(path = list(...))),
    req_headers = function(req, ...) c(req, list(headers = list(...))),
    req_method = function(req, method) c(req, list(method = method)),
    req_body_json = function(req, data, ...) c(req, list(body = data)),
    req_error = function(req, ...) req,
    req_perform = function(req, ...) req,
    resp_status = function(resp) 200L,
    .package = "httr2"
  )

  studio_calls <- 0L
  testthat::local_mocked_bindings(
    resp_body_json = function(resp, ...) {
      studio_calls <<- studio_calls + 1L
      if (studio_calls == 1L) {
        list(accessToken = "token")
      } else {
        list(
          userId = 99,
          userName = "tester",
          userLongName = "Test User",
          workspaces = list(
            list(id = 10, name = "Group", workspaces = list(list(id = 1, name = "Workspace 1")))
          )
        )
      }
    },
    .package = "httr2"
  )

  studio <- login_studio(dialog = FALSE)

  expect_s4_class(studio, "LoginStudio")
  expect_equal(studio@user_key, "tester")
  expect_equal(studio@ws_list[[1]]$ws_id, 1)

  tc_calls <- 0L
  testthat::local_mocked_bindings(
    resp_body_json = function(resp, ...) {
      tc_calls <<- tc_calls + 1L
      if (tc_calls == 1L) {
        list(
          token = "token",
          displayName = "Tester",
          claims = list(workspaceAdmin = list(list(id = 7, label = "TC Workspace")))
        )
      } else {
        list(version = "16.0.2")
      }
    },
    .package = "httr2"
  )

  testcenter <- login_testcenter(dialog = FALSE)

  expect_s4_class(testcenter, "LoginTestcenter")
  expect_equal(testcenter@ws_list[[1]]$ws_id, 7)
  expect_equal(testcenter@app_version, "16.0.2")
})

test_that("solve_altcha_v1_challenge finds a matching number", {
  answer <- 7L
  challenge <- list(
    algorithm = "SHA-256",
    challenge = digest::digest(paste0("salt", answer), algo = "sha256", serialize = FALSE),
    maxNumber = 20L,
    salt = "salt",
    signature = "signature"
  )

  solution <- eatPrepTBA:::solve_altcha_v1_challenge(challenge, timeout = 5)

  expect_equal(solution$number, answer)
  expect_equal(solution$algorithm, "SHA-256")
  expect_equal(solution$signature, "signature")
})

test_that("login_testcenter uses challenge flow when brute-force protection blocks direct login", {
  request_path <- function(req) {
    paste(unlist(req$path, use.names = FALSE), collapse = "/")
  }

  answer <- 4L
  challenge <- list(
    algorithm = "SHA-256",
    challenge = digest::digest(paste0("abc", answer), algo = "sha256", serialize = FALSE),
    maxNumber = 10L,
    salt = "abc",
    signature = "sig"
  )
  performed <- list()

  testthat::local_mocked_bindings(
    get_credentials = function(...) list(name = "user", password = "pw"),
    generate_base_req = function(type, base_url, auth_token, app_version = NULL, insecure = FALSE) {
      function(method, endpoint, query = NULL) {
        list(type = type, method = method, endpoint = endpoint)
      }
    },
    .package = "eatPrepTBA"
  )
  testthat::local_mocked_bindings(
    request = function(base_url) list(base_url = base_url),
    req_url_path_append = function(req, ...) c(req, list(path = list(...))),
    req_headers = function(req, ...) c(req, list(headers = list(...))),
    req_method = function(req, method) c(req, list(method = method)),
    req_body_json = function(req, data, ...) c(req, list(body = data)),
    req_error = function(req, ...) req,
    req_perform = function(req, ...) {
      performed[[length(performed) + 1L]] <<- req
      req
    },
    resp_status = function(resp) {
      if (identical(request_path(resp), "api/session/admin")) {
        return(400L)
      }
      200L
    },
    resp_body_string = function(resp, ...) {
      "Brute Force protection active. Challenge for this password must be solved to create a session"
    },
    resp_check_status = function(resp, ...) stop("unexpected status check"),
    resp_body_json = function(resp, ...) {
      path <- request_path(resp)
      if (identical(path, "api/session/challenge")) {
        return(challenge)
      }
      if (identical(path, "api/session")) {
        return(list(
          token = "challenge-token",
          displayName = "Tester",
          claims = list(workspaceAdmin = list(list(id = 7, label = "TC Workspace")))
        ))
      }
      if (identical(resp$endpoint, c("version"))) {
        return(list(version = "18.3.0"))
      }
      stop("unexpected response")
    },
    .package = "httr2"
  )

  testcenter <- login_testcenter(dialog = FALSE)
  challenge_request <- performed[[which(vapply(performed, function(req) {
    identical(request_path(req), "api/session/challenge")
  }, logical(1)))]]
  solution_request <- performed[[which(vapply(performed, function(req) {
    identical(request_path(req), "api/session")
  }, logical(1)))]]

  expect_s4_class(testcenter, "LoginTestcenter")
  expect_equal(testcenter@app_version, "18.3.0")
  expect_equal(challenge_request$body$loginType, "admin")
  expect_equal(challenge_request$body$name, "user")
  expect_equal(solution_request$body$number, answer)
  expect_equal(solution_request$body$challenge, challenge$challenge)
})

test_that("login functions validate scalar arguments before prompting", {
  expect_error(login_studio(verbose = "yes"), "logical")
  expect_error(login_testcenter(insecure = "yes"), "logical")
})

test_that("get_credentials can run in test mode without prompting", {
  old <- getOption("eatPrepTBA.test_mode")
  options("eatPrepTBA.test_mode" = TRUE)
  on.exit(options("eatPrepTBA.test_mode" = old), add = TRUE)

  default <- eatPrepTBA:::get_credentials("https://example/", keyring = FALSE, change_key = FALSE, dialog = FALSE)
  custom <- eatPrepTBA:::get_credentials(
    "https://example/",
    keyring = FALSE,
    change_key = FALSE,
    dialog = FALSE,
    name = "alice",
    password = "secret"
  )

  expect_equal(default$name, "eatPrepTBA")
  expect_equal(custom, list(name = "alice", password = "secret"))
})

test_that("get_credentials uses RStudio dialog when available", {
  old <- getOption("eatPrepTBA.test_mode")
  options("eatPrepTBA.test_mode" = FALSE)
  on.exit(options("eatPrepTBA.test_mode" = old), add = TRUE)

  prompts <- list()
  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    showPrompt = function(title, message, default = NULL, timeout = 60) {
      prompts$name <<- message
      "alice"
    },
    askForPassword = function(prompt = "Please enter your password") {
      prompts$password <<- prompt
      "secret"
    },
    .package = "rstudioapi"
  )

  credentials <- eatPrepTBA:::get_credentials(
    "https://example/",
    keyring = FALSE,
    change_key = FALSE,
    dialog = TRUE
  )

  expect_equal(credentials, list(name = "alice", password = "secret"))
  expect_match(prompts$name, "username")
  expect_match(prompts$password, "password")
})

test_that("list_files normalizes file listings and optional dependencies", {
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) req,
    req_body_json = function(req, data, ...) c(req, list(body = data)),
    resp_body_json = function(resp, ...) {
      if (identical(resp$method, "POST")) {
        list(
          Unit = list(
            list(
              type = "Unit",
              name = "U1.xml",
              size = 10,
              dependencies = list(list(relationship_type = "isDefinedBy", object_name = "U1.xml"))
            )
          )
        )
      } else {
        list(
          Unit = list(
            list(type = "Unit", name = "U1.xml", size = 10),
            list(type = "Unit", name = "readme.txt", size = 1)
          ),
          Booklet = list(list(type = "Booklet", name = "B1.xml", size = 5)),
          Resource = list(list(type = "Resource", name = "coding.vocs", size = 3))
        )
      }
    },
    .package = "httr2"
  )

  workspace <- fake_testcenter_workspace()

  files <- list_files(workspace, type = c("Unit", "Booklet"))
  deps <- list_files(workspace, type = "Unit", dependencies = TRUE)

  expect_equal(files$name, c("U1.xml", "readme.txt", "B1.xml"))
  expect_true(tibble::has_name(deps, "dependencies"))
  expect_equal(deps$name, "U1.xml")
})

test_that("file list wrappers filter names by type and extension", {
  testthat::local_mocked_bindings(
    list_files = function(workspace, type, dependencies = FALSE) {
      switch(
        type,
        Unit = tibble::tibble(name = c("U1.xml", "notes.txt")),
        Booklet = tibble::tibble(name = c("B1.xml", "B1.pdf")),
        Testtakers = tibble::tibble(name = c("TT.xml", "TT.csv")),
        Resource = tibble::tibble(name = c("scheme.vocs", "meta.vomd", "image.png"))
      )
    },
    .package = "eatPrepTBA"
  )

  workspace <- fake_testcenter_workspace()

  expect_equal(list_units(workspace), "U1")
  expect_equal(list_booklets(workspace), "B1")
  expect_equal(list_testtakers(workspace), "TT")
  expect_equal(list_resources(workspace), c("scheme.vocs", "meta.vomd"))
})

test_that("Studio list and settings wrappers use safe request results", {
  call_no <- 0L
  testthat::local_mocked_bindings(
    run_safe = function(req, error_message = NULL, default = NULL) {
      call_no <<- call_no + 1L
      switch(
        call_no,
        list("Group B", "Group A"),
        list(
          id = 1,
          settings = list(
            defaultEditor = "editor",
            unitGroups = list("Group A", "Group B")
          )
        ),
        list(
          id = 10,
          settings = list(states = list(list(id = 1, label = "Open", color = "#fff")))
        )
      )
    },
    .package = "eatPrepTBA"
  )

  workspace <- fake_studio_workspace()

  expect_equal(list_groups(workspace), c("Group B", "Group A"))
  settings <- get_settings(workspace)

  expect_equal(settings$default_editor, "editor")
  expect_equal(settings$groups[[1]], c("Group A", "Group B"))
  expect_equal(settings$states[[1]]$state_label, "Open")
})

test_that("get_states and list_system_checks shape API responses", {
  state_mode <- TRUE
  testthat::local_mocked_bindings(
    run_safe = function(req, error_message = NULL, default = NULL) {
      if (state_mode) {
        list(settings = list(states = list(list(id = 1, label = "Open", color = "#fff"))))
      } else {
        list(
          list(id = "G1", count = 2, label = "Group 1"),
          list(id = "G2", count = 1, label = "Group 2")
        )
      }
    },
    .package = "eatPrepTBA"
  )

  states <- get_states(fake_studio_workspace())
  state_mode <- FALSE
  checks <- list_system_checks(fake_testcenter_workspace())

  expect_equal(states$id, c(0, 1))
  expect_equal(checks$id, c("G1", "G2"))
  expect_equal(checks$count, c(2, 1))
})

test_that("get_results, get_reviews, get_logs, and get_responses shape report data", {
  safe_call <- 0L
  testthat::local_mocked_bindings(
    run_safe = function(req, error_message = NULL, default = NULL) {
      safe_call <<- safe_call + 1L
      if (safe_call == 1L) {
        list(list(groupName = "G1", state = "complete"))
      } else {
        list(
          list(
            groupname = "G1",
            loginname = "L1",
            code = "C1",
            bookletname = "B1",
            originalUnitId = "U1",
            unitname = "Alias1",
            reviewtime = "10",
            category_content = TRUE,
            category_design = FALSE,
            category_tech = FALSE
          )
        )
      }
    },
    .package = "eatPrepTBA"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) req,
    resp_body_json = function(resp, ...) {
      if ("log" %in% resp$endpoint) {
        list(list(
          groupname = "G1",
          loginname = "L1",
          code = "C1",
          bookletname = "B1",
          unitname = "Alias1",
          originalUnitId = "",
          timestamp = "1000",
          logentry = "PLAYER = RUNNING"
        ))
      } else {
        list(list(
          groupname = "G1",
          loginname = "L1",
          code = "C1",
          bookletname = "B1",
          unitname = "U1",
          responses = list(list(id = "elementCodes", content = "[{\"id\":\"V1\",\"value\":\"A\"}]", ts = 10)),
          laststate = jsonlite::toJSON(
            list(list(PLAYER = "RUNNING", RESPONSE_PROGRESS = "complete")),
            auto_unbox = TRUE
          )
        ))
      }
    },
    .package = "httr2"
  )

  workspace <- fake_testcenter_workspace()

  expect_equal(get_results(workspace)$groupName, "G1")
  expect_equal(get_reviews(workspace, groups = "G1")$content, TRUE)
  expect_equal(get_logs(workspace, groups = "G1")$unit_key, "Alias1")
  expect_equal(get_responses(workspace, groups = "G1")$responses, "[{\"id\":\"V1\",\"value\":\"A\"}]")
})

test_that("get_system_checks prepares nested system check reports", {
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) req,
    resp_body_json = function(resp, ...) {
      list(
        list(
          environment = list(list(list(label = "Browser", value = "Firefox"))),
          network = list(),
          unit = list(unitname = "U1"),
          fileData = list(),
          questionnaire = list(list(list(id = "q1", value = "yes"))),
          responses = list(
            list(list(content = jsonlite::toJSON(list(list(id = "V1", value = "A")), auto_unbox = TRUE)))
          )
        )
      )
    },
    .package = "httr2"
  )

  out <- get_system_checks(fake_testcenter_workspace(), groups = "G1")

  expect_equal(out$Browser, "Firefox")
  expect_equal(out$q1, "yes")
  expect_equal(out$id, "V1")
  expect_equal(out$value, "A")
})

test_that("get_coding_report extracts unit metadata from report links", {
  testthat::local_mocked_bindings(
    run_safe = function(req, error_message = NULL, default = NULL) {
      tibble::tibble(
        unit = "<a href=#/a/1/10>U1: Unit 1</a>",
        variable = "V1",
        item = "I1",
        validation = "ok",
        validationProblems = list(list(list(type = "INVALID_RULE", breaking = TRUE, code = "1"))),
        codingType = "auto"
      )
    },
    .package = "eatPrepTBA"
  )

  out <- get_coding_report(fake_studio_workspace())

  expect_equal(out$ws_id, "1")
  expect_equal(out$unit_id, 10L)
  expect_equal(out$unit_key, "U1")
  expect_equal(out$coding_type, "auto")
  expect_equal(out$validation_problems[[1]]$type, "INVALID_RULE")
  expect_true(out$validation_problems[[1]]$breaking)
  expect_equal(out$validation_problems[[1]]$code, "1")
})

test_that("get_units prepares Studio unit metadata without live API calls", {
  testthat::local_mocked_bindings(
    get_settings = function(workspace, metadata = TRUE) {
      tibble::tibble(
        ws_id = 1,
        ws_label = "Workspace 1",
        wsg_id = 10,
        wsg_label = "Workspace Group",
        states = list(tibble::tibble(state_id = 1, state_label = "Open", state_color = "#fff"))
      )
    },
    run_safe = function(req, error_message = NULL, default = NULL) {
      list(
        list(
          id = 10,
          key = "U1",
          name = "Unit 1",
          groupName = "Group A",
          state = 1,
          variables = list(
            list(
              id = "v1",
              alias = "V1",
              page = 1,
              type = "text",
              format = "",
              values = list(),
              multiple = "FALSE",
              nullable = "TRUE",
              valuesComplete = "FALSE"
            )
          ),
          metadata = list(),
          scheme = minimal_coding_scheme(),
          lastChangedDefinition = "2026-01-01T00:00:00Z",
          lastChangedScheme = "2026-01-02T00:00:00Z",
          lastChangedMetadata = "2026-01-03T00:00:00Z"
        )
      )
    },
    .package = "eatPrepTBA"
  )

  out <- get_units(fake_studio_workspace(), metadata = FALSE, unit_definition = FALSE)

  expect_s3_class(out, "tba_units")
  expect_equal(out$unit_key, "U1")
  expect_equal(out$unit_variables[[1]]$variable_id, "V1")
  expect_equal(out$state_label, "Open")
  expect_s3_class(attr(out, "ws_settings"), "tbl_df")
})

test_that("get_design combines testtakers, booklets, and optional units", {
  testthat::local_mocked_bindings(
    get_testtakers = function(workspace, files = NULL) {
      tibble::tibble(
        group_id = "G1",
        group_label = "Group 1",
        login_name = "L1",
        login_mode = "run-hot-return",
        login_code = "C1",
        booklet_id = "B1",
        booklet_no = 1L
      )
    },
    get_booklets = function(workspace, files = NULL) {
      tibble::tibble(
        booklet_id = "B1",
        booklet_label = "Booklet 1",
        testlet_id = NA_character_,
        testlet_label = NA_character_,
        testlet_no = 1L,
        unit_key = c("U1", "U2"),
        unit_label = c("Unit 1", "Unit 2"),
        unit_alias = c("U1", "U2"),
        unit_testlet_no = c(1L, 2L),
        unit_booklet_no = c(1L, 2L)
      )
    },
    add_coding_scheme = function(units, overwrite = FALSE, filter_has_codes = TRUE) {
      units
    },
    .package = "eatPrepTBA"
  )

  units <- tibble::tibble(unit_key = "U1", unit_codes = list(tibble::tibble(variable_id = "V1")))

  design <- get_design(fake_testcenter_workspace())
  design_units <- get_design(fake_testcenter_workspace(), units = units)

  expect_equal(design$unit_key, c("U1", "U2"))
  expect_equal(design_units$unit_key, "U1")
  expect_equal(design_units$variable_id, "V1")
})

test_that("download and upload wrappers delegate to safe API calls", {
  performed <- list()

  testthat::local_mocked_bindings(
    list_units = function(workspace) {
      list(list(ws_id = 1, ws_label = "Workspace 1", units = list(list(unit_id = 10, unit_key = "U1"))))
    },
    run_safe = function(req, error_message = NULL, default = NULL) {
      req()
      "ok"
    },
    .package = "eatPrepTBA"
  )
  testthat::local_mocked_bindings(
    req_body_json = function(req, data, ...) c(req, list(body = data)),
    req_body_multipart = function(req, ...) c(req, list(body = list(...))),
    req_perform = function(req, ...) {
      performed[[length(performed) + 1L]] <<- req
      req
    },
    resp_body_json = function(resp, ...) list(),
    .package = "httr2"
  )

  tmp <- tempfile()
  writeLines("x", tmp)

  expect_no_error(download_codebook(fake_studio_workspace(), path = tempdir(), format = "json"))
  expect_no_error(download_units(
    fake_studio_workspace(),
    path = tempdir(),
    add_players = FALSE,
    booklet_label = "Booklet label"
  ))
  expect_no_error(upload_file(fake_testcenter_workspace(), path = tmp))

  download_request <- performed[vapply(performed, function(req) {
    isTRUE(req$query$download)
  }, logical(1))][[1]]
  download_settings <- jsonlite::fromJSON(download_request$query$settings)

  expect_equal(download_settings$bookletLabel, "Booklet label")
  expect_true("bookletSettings" %in% names(download_settings))
  expect_equal(download_settings$unitIdList, 10L)
})

test_that("change and comment wrappers build settings and use run_safe", {
  changed <- list()
  testthat::local_mocked_bindings(
    run_safe = function(req, error_message = NULL, default = NULL) {
      changed[[length(changed) + 1L]] <<- req()
      "ok"
    },
    list_groups = function(workspace) "Group A",
    get_states = function(workspace) tibble::tibble(id = 1, label = "Open"),
    .package = "eatPrepTBA"
  )
  testthat::local_mocked_bindings(
    req_body_json = function(req, data, ...) c(req, list(body = data)),
    req_perform = function(req, ...) req,
    .package = "httr2"
  )

  workspace <- fake_studio_workspace()

  expect_no_error(eatPrepTBA:::change_unit_settings(
    workspace,
    unit_id = 10,
    unit_key = "ABC_1",
    player = "2.10.4",
    group_name = "Group A",
    state = "Open"
  ))
  expect_no_error(eatPrepTBA:::settings(
    workspace,
    unit_id = 10,
    unit_key = "A",
    unit_name = NULL,
    description = NULL,
    player = 2.10,
    editor = 3.2,
    schemer = 2.5,
    group_name = NULL,
    state = NULL
  ))
  expect_no_error(change_units_settings(workspace, unit_ids = c(10, 11), editor = "3.2"))
  expect_no_error(eatPrepTBA:::add_comment(workspace@login, ws_id = 1, unit_id = 10, comment = "<p>Hello</p>"))
  expect_true(length(changed) >= 4)
})
