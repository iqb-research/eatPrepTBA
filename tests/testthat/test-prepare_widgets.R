test_that("prepare_widgets returns NULL if no widgets are selected", {
  expect_null(prepare_widgets(NULL))
  expect_null(prepare_widgets(character()))
})

test_that("prepare_widgets creates calculator configuration", {
  result <- prepare_widgets("calculator")

  expect_equal(
    result$Widgets$Widget,
    list(
      id = "calc",
      model = "CALCULATOR",
      Parameter = list(
        "0",
        key = "JOURNAL_LINES"
      )
    )
  )
})

test_that("prepare_widgets uses the specified journal lines", {
  result <- prepare_widgets("calculator", journal_lines = 3)

  expect_equal(
    result$Widgets$Widget$Parameter[[1]],
    "3"
  )
})

test_that("prepare_widgets rejects invalid journal lines", {
  expect_error(
    prepare_widgets("calculator", journal_lines = -1),
    class = "assertError"
  )

  expect_error(
    prepare_widgets("calculator", journal_lines = 1.5),
    class = "assertError"
  )
})

test_that("prepare_widgets rejects invalid or duplicate widgets", {
  expect_error(
    prepare_widgets("unknown"),
    "contains invalid widget"
  )

  expect_error(
    prepare_widgets(c("calculator", "calculator")),
    "duplicate widgets"
  )
})

test_that("prepare_widgets reports unimplemented widgets", {
  expect_error(
    prepare_widgets("molecule"),
    "not implemented yet"
  )

  expect_error(
    prepare_widgets("periodic"),
    "not implemented yet"
  )
})

test_that("prepare_widgets accepts numeric integer values", {
  result <- prepare_widgets("calculator", journal_lines = 3.0)

  expect_equal(
    result$Widgets$Widget$Parameter[[1]],
    "3"
  )
})

test_that("prepare_widgets ignores journal_lines if no widgets are selected", {
  expect_null(
    prepare_widgets(NULL, journal_lines = -1)
  )
})
