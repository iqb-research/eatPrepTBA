test_that("validation helpers report missing columns and attributes", {
  data <- tibble::tibble(id = 1)
  attr(data, "source") <- "test"

  expect_identical(eatPrepTBA:::assert_cols(data, "id", "data"), data)
  expect_error(
    eatPrepTBA:::assert_cols(data, c("id", "missing"), "data"),
    "missing"
  )

  expect_identical(eatPrepTBA:::assert_attrs(data, "source", "data"), data)
  expect_error(
    eatPrepTBA:::assert_attrs(data, "missing_attr", "data"),
    "missing_attr"
  )
})

test_that("file and URL validators accept valid inputs and reject invalid inputs", {
  tmp <- tempfile()
  writeLines("ok", tmp)

  expect_identical(eatPrepTBA:::assert_existing_files(tmp), tmp)
  expect_error(eatPrepTBA:::assert_existing_files(paste0(tmp, "-missing")), "do not exist")

  expect_identical(eatPrepTBA:::assert_url("https://example.org"), "https://example.org")
  expect_error(eatPrepTBA:::assert_url("example.org"), "valid URL")
})
