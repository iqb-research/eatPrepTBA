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
  expect_error(prepare_calculator(TRUE, journal_lines = -1))
  expect_error(prepare_calculator(TRUE, journal_lines = 1.5))
  expect_error(prepare_calculator(TRUE, journal_lines = NA_real_), "journal_lines")
  expect_error(prepare_calculator(TRUE, journal_lines = NaN), "journal_lines")
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

test_that("generate_booklet applies the calculator to testlets", {
  testlets <- tibble::tibble(
    id = "T1",
    label = "Testlet 1",
    units = list(tibble::tibble(
      id = c("U1", "U2"),
      label = c("Unit 1", "Unit 2")
    )),
    restrictions = list(list(minutes = 5))
  )

  for (enabled in c(FALSE, TRUE)) {
    xml <- generate_booklet(
      booklet_id = "B1",
      booklet_label = "Booklet 1",
      testlets = testlets,
      calculator = enabled
    )

    expect_equal(
      xml2::xml_attr(xml2::xml_find_first(xml, "/Booklet/Units"), "widgets"),
      if (enabled) "calc" else NA_character_
    )
    expect_length(
      xml2::xml_find_all(xml, "/Booklet/Widgets/Widget[@id='calc']"),
      as.integer(enabled)
    )
    expect_equal(
      xml2::xml_attr(xml2::xml_find_all(xml, "/Booklet/Units/Testlet/Unit"), "id"),
      c("U1", "U2")
    )
    expect_equal(
      xml2::xml_attr(xml2::xml_find_first(
        xml, "/Booklet/Units/Testlet/Restrictions/TimeMax"
      ), "minutes"),
      "5"
    )
  }
})

test_that("generate_booklet writes journal lines as decimal integers", {
  units <- tibble::tibble(id = "U1", label = "Unit 1")
  values <- list(0L, 3.0, 1e-9, 2.00000001, 2.99999999, 1e7, 2 + 0i)

  actual <- vapply(values, function(value) {
    xml <- generate_booklet(
      booklet_id = "B1",
      booklet_label = "Booklet 1",
      units = units,
      calculator = TRUE,
      journal_lines = value
    )
    parameters <- xml2::xml_find_all(
      xml, "/Booklet/Widgets/Widget[@id='calc']/Parameter[@key='JOURNAL_LINES']"
    )
    expect_length(parameters, 1)
    xml2::xml_text(parameters)
  }, character(1))

  expect_equal(actual, c("0", "3", "0", "2", "3", "10000000", "2"))
})

test_that("generate_booklet rejects calculators for older booklet schemas", {
  units <- tibble::tibble(id = "U1", label = "Unit 1")

  for (version in c("18.0", "legacy-16")) {
    expect_error(
      generate_booklet(
        booklet_id = "B1",
        booklet_label = "Booklet 1",
        units = units,
        calculator = TRUE,
        booklet_config_version = version
      ),
      "calculator = TRUE requires booklet_config_version = '18.1'.",
      fixed = TRUE
    )
  }
})
