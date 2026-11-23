test_that("prepare_calculator omits disabled calculators", {
  expect_null(prepare_calculator())
  expect_null(prepare_calculator(FALSE))
})

test_that("prepare_calculator creates calculator configuration", {
  result <- prepare_calculator(TRUE)

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

test_that("prepare_calculator uses the specified journal lines", {
  result <- prepare_calculator(TRUE, journal_lines = 3)

  expect_equal(result$Widgets$Widget$Parameter[[1]], "3")
  expect_equal(
    prepare_calculator(TRUE, journal_lines = 3.0)$Widgets$Widget$Parameter[[1]],
    "3"
  )
})

test_that("prepare_calculator validates its inputs", {
  expect_error(prepare_calculator("mas"))
  expect_error(prepare_calculator(TRUE, journal_lines = -1), class = "assertError")
  expect_error(prepare_calculator(TRUE, journal_lines = 1.5), class = "assertError")
})

test_that("generate_booklet adds calculator only when requested", {
  units <- tibble::tibble(id = "U1", label = "Unit 1")

  with_calculator <- generate_booklet(
    booklet_id = "B1",
    booklet_label = "Booklet 1",
    units = units,
    calculator = TRUE,
    journal_lines = 2
  )
  without_calculator <- generate_booklet(
    booklet_id = "B2",
    booklet_label = "Booklet 2",
    units = units
  )

  expect_length(xml2::xml_find_all(with_calculator, ".//Widgets/Widget[@id='calc']"), 1)
  expect_equal(
    xml2::xml_attr(xml2::xml_find_first(with_calculator, ".//Units"), "widgets"),
    "calc"
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(
      with_calculator,
      ".//Widgets/Widget[@id='calc']/Parameter[@key='JOURNAL_LINES']"
    )),
    "2"
  )
  expect_length(xml2::xml_find_all(without_calculator, ".//Widgets/Widget"), 0)
  expect_true(is.na(xml2::xml_attr(
    xml2::xml_find_first(without_calculator, ".//Units"),
    "widgets"
  )))
})
