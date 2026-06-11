fake_base_req <- function() {
  function(method, endpoint, query = NULL) {
    list(
      method = method,
      endpoint = as.character(endpoint),
      query = query
    )
  }
}

fake_studio_login <- function(base_req = fake_base_req()) {
  new(
    "LoginStudio",
    base_url = "https://studio.example/",
    base_req = base_req,
    ws_list = list(
      list(ws_id = 1, ws_label = "Workspace 1"),
      list(ws_id = 2, ws_label = "Workspace 2")
    ),
    wsg_list = list(
      list(
        wsg_id = 10,
        wsg_label = "Workspace Group",
        ws_list = list(
          list(ws_id = 1, ws_label = "Workspace 1"),
          list(ws_id = 2, ws_label = "Workspace 2")
        )
      )
    ),
    user_id = 99,
    user_key = "tester",
    user_label = "Test User",
    app_version = "16.0.2"
  )
}

fake_testcenter_login <- function(base_req = fake_base_req()) {
  new(
    "LoginTestcenter",
    base_url = "https://testcenter.example/",
    base_req = base_req,
    ws_list = list(
      list(ws_id = 7, ws_label = "TC Workspace")
    ),
    app_version = "16.0.2"
  )
}

fake_studio_workspace <- function(login = fake_studio_login()) {
  new(
    "WorkspaceStudio",
    login = login,
    ws_id = 1,
    ws_label = "Workspace 1",
    wsg_id = 10,
    wsg_label = "Workspace Group"
  )
}

fake_testcenter_workspace <- function(login = fake_testcenter_login()) {
  new(
    "WorkspaceTestcenter",
    login = login,
    ws_id = 7,
    ws_label = "TC Workspace"
  )
}

minimal_coding_scheme <- function() {
  jsonlite::toJSON(
    list(
      variableCodings = list(
        list(
          id = "v1",
          alias = "V1",
          label = "Variable 1",
          sourceType = "BASE",
          sourceParameters = list(
            processing = list(),
            solverExpression = NULL
          ),
          deriveSources = list(),
          processing = list(),
          fragmenting = NULL,
          manualInstruction = "Code carefully",
          codeModel = "basic",
          page = 1,
          codes = list(
            list(
              id = 1,
              type = "FULL_CREDIT",
              label = "Right",
              score = 1,
              manualInstruction = "Accept A",
              ruleSetOperatorAnd = TRUE,
              ruleSets = list()
            ),
            list(
              id = 0,
              type = "RESIDUAL",
              label = "Wrong",
              score = 0,
              manualInstruction = "Other",
              ruleSetOperatorAnd = TRUE,
              ruleSets = list()
            )
          )
        )
      )
    ),
    auto_unbox = TRUE,
    null = "null"
  ) |>
    as.character()
}

minimal_unit_codes <- function() {
  tibble::tibble(
    variable_id = "V1",
    variable_ref = "v1",
    variable_label = "Variable 1",
    variable_source_type = "BASE",
    variable_level = 0,
    variable_multiple = FALSE,
    variable_page = 1,
    variable_section = 1,
    variable_page_always_visible = FALSE,
    variable_sources = list(
      tibble::tibble(
        variable_source_id = NA_character_,
        variable_source_ref = NA_character_,
        variable_source_level = NA_integer_,
        variable_source_direct = FALSE
      )
    ),
    variable_source_processing = list(list()),
    variable_codes = list(
      tibble::tibble(
        code_id = c(1L, 0L),
        code_type = c("FULL_CREDIT", "RESIDUAL"),
        code_score = c(1, 0)
      )
    )
  )
}

minimal_units <- function() {
  tibble::tibble(
    ws_id = 1,
    ws_label = "Workspace 1",
    unit_id = 10,
    unit_key = "U1",
    unit_label = "Unit 1",
    coding_scheme = minimal_coding_scheme(),
    unit_variables = list(
      tibble::tibble(
        variable_id = "V1",
        variable_ref = "v1",
        variable_type = "choice",
        variable_format = "",
        variable_multiple = FALSE,
        variable_values = list(
          tibble::tibble(
            value = c("A", "B"),
            value_label = c("Alpha", "Beta")
          )
        )
      )
    ),
    unit_codes = list(minimal_unit_codes()),
    items_list = list(
      tibble::tibble(
        item_no = 1L,
        item_id = "I1",
        variable_id = "V1",
        variable_ref = "v1"
      )
    )
  )
}
