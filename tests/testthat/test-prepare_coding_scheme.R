coding_scheme_json <- function(variable_codings) {
  jsonlite::toJSON(
    list(variableCodings = variable_codings),
    auto_unbox = TRUE,
    null = "null"
  )
}

test_that("prepare_coding_scheme returns empty tibbles for missing schemes", {
  expect_equal(dim(prepare_coding_scheme(NA_character_)), c(0L, 0L))
  expect_equal(dim(prepare_coding_scheme("")), c(0L, 0L))
  expect_equal(dim(prepare_coding_scheme("null")), c(0L, 0L))
})

test_that("prepare_coding_scheme completes missing schemer columns", {
  coding_scheme <- coding_scheme_json(list(
    list(
      id = "v1",
      alias = list("v1"),
      label = "Variable 1",
      codes = list(
        list(
          id = "c1",
          type = "CORRECT",
          label = "Correct",
          score = 1
        )
      )
    )
  ))

  prepared_scheme <- prepare_coding_scheme(coding_scheme)

  expect_s3_class(prepared_scheme, "tbl_df")
  expect_equal(nrow(prepared_scheme), 1L)
  expect_true(all(c(
    "variable_source_type",
    "variable_source_processing",
    "variable_code_model",
    "rule_set_array_position",
    "rule_fragment_position"
  ) %in% names(prepared_scheme)))
  expect_equal(prepared_scheme$code_id, "c1")
  expect_true(is.na(prepared_scheme$variable_source_type))
  expect_true(is.na(prepared_scheme$variable_code_model))
})

test_that("prepare_coding_scheme normalizes code models and keeps rule details", {
  coding_scheme <- coding_scheme_json(list(
    list(
      id = "v1",
      alias = list("v1"),
      label = "Variable 1",
      sourceType = "BASE",
      sourceParameters = list(
        processing = NULL,
        solverExpression = NULL
      ),
      codeModel = list(
        type = "MANUAL",
        version = 1
      ),
      codes = list(
        list(
          id = "c1",
          type = "CORRECT",
          label = "Correct",
          score = 1,
          ruleSetOperatorAnd = TRUE,
          ruleSets = list(
            list(
              ruleOperatorAnd = TRUE,
              valueArrayPos = list(1),
              rules = list(
                list(
                  method = "equals",
                  fragment = list(1),
                  parameters = list("x")
                )
              )
            )
          )
        ),
        list(
          id = "c2",
          type = "WRONG",
          label = "Wrong",
          score = 0
        )
      )
    )
  ))

  prepared_scheme <- prepare_coding_scheme(coding_scheme)

  expect_equal(nrow(prepared_scheme), 2L)
  expect_equal(prepared_scheme$variable_code_model[[1]], "{\"type\":\"MANUAL\",\"version\":1}")
  expect_equal(prepared_scheme$rule_set_operator[[1]], "AND")
  expect_equal(prepared_scheme$rule_set_array_position[[1]], "1")
  expect_equal(prepared_scheme$rule_operator[[1]], "AND")
  expect_equal(prepared_scheme$rule_fragment_position[[1]], "1")
  expect_equal(prepared_scheme$rule_method[[1]], "equals")
  expect_equal(prepared_scheme$rule_parameter[[1]], "x")
})

test_that("add_coding_scheme tolerates missing coding schemes", {
  coding_scheme <- coding_scheme_json(list(
    list(
      id = "v1",
      alias = list("v1"),
      label = "Variable 1",
      sourceType = "BASE",
      sourceParameters = list(
        processing = NULL,
        solverExpression = NULL
      ),
      codes = list(
        list(
          id = "c1",
          type = "CORRECT",
          label = "Correct",
          score = 1
        )
      )
    )
  ))

  units <- tibble::tibble(
    ws_id = c("ws", "ws"),
    unit_id = c("u1", "u2"),
    unit_key = c("U1", "U2"),
    coding_scheme = c(coding_scheme, NA_character_),
    unit_variables = list(
      tibble::tibble(
        variable_ref = "v1",
        variable_id = "v1",
        variable_type = "string",
        variable_format = ""
      ),
      tibble::tibble(
        variable_ref = character(),
        variable_id = character(),
        variable_type = character(),
        variable_format = character()
      )
    )
  )

  prepared_units <- add_coding_scheme(units)

  expect_equal(nrow(prepared_units), 2L)
  expect_s3_class(prepared_units$unit_codes[[1]], "tbl_df")
  expect_null(prepared_units$unit_codes[[2]])
})
