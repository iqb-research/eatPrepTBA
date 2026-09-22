test_that("item lookup uses both keys and preserves response rows", {
  units <- tibble::tibble(
    unit_key = c("U1", "U2"),
    items_list = list(
      tibble::tibble(variable_id = c("V1", "V1", "V2"), item_id = c("I1", "I1", "I2")),
      tibble::tibble(variable_id = "V1", item_id = "I3")
    )
  )
  data <- tibble::tibble(
    unit_key = c("U2", "U1", "U1", "U3", "U1"),
    variable_id = c("V1", "V2", "V1", "V1", "V1"),
    score = c(0, 1, 2, 3, 4)
  )
  out <- add_item_id(data, units)
  expect_identical(out[names(data)], data)
  expect_identical(out$item_id, c("I3", "I2", "I1", NA_character_, "I1"))
  expect_s3_class(add_item_id(as.data.frame(data), units), "data.frame")
})

test_that("missing and empty links never match", {
  units <- tibble::tibble(
    unit_key = c("U1", NA_character_, ""),
    items_list = list(
      tibble::tibble(variable_id = c(NA, "", " ", "V1", "V2", "V3"),
                     item_id = c("orphan", "empty", "blank", NA, "", "I3")),
      tibble::tibble(variable_id = "V3", item_id = "no-unit"),
      tibble::tibble(variable_id = "V3", item_id = "empty-unit")
    )
  )
  data <- tibble::tibble(unit_key = c(rep("U1", 6), NA, ""),
                         variable_id = c(NA, "", " ", "V1", "V2", "V3", "V3", "V3"))
  expect_identical(add_item_id(data, units)$item_id,
                   c(rep(NA_character_, 5), "I3", NA_character_, NA_character_))
})

test_that("conflicting links across units or workspaces are rejected", {
  units <- tibble::tibble(
    ws_id = c(1, 2), unit_key = "U1",
    items_list = list(
      tibble::tibble(variable_id = "V1", item_id = "I1"),
      tibble::tibble(variable_id = "V1", item_id = "I2")
    )
  )
  data <- tibble::tibble(unit_key = "U1", variable_id = "V1")
  expect_error(add_item_id(data, units), "Multiple.*item_id.*U1 / V1")
  expect_equal(add_item_id(data, units[1, ])$item_id, "I1")
})

test_that("prepared metadata and empty inputs are supported", {
  units <- minimal_units()
  data <- tibble::tibble(unit_key = "U1", variable_id = "V1")
  prepared <- dplyr::rename(units, item_metadata = items_list)
  expect_equal(add_item_id(data, prepared)$item_id, "I1")
  expect_identical(add_item_id(data[0, ], units)$item_id, character())
  expect_identical(add_item_id(data, units[0, ])$item_id, NA_character_)
  units$items_list <- list(NULL)
  expect_identical(add_item_id(data, units)$item_id, NA_character_)
  units$items_list <- list(tibble::tibble(variable_id = character(), item_id = character()))
  expect_identical(add_item_id(data, units)$item_id, NA_character_)
})

test_that("overwrite is explicit and validation explains missing metadata", {
  units <- minimal_units()
  data <- tibble::tibble(unit_key = c("U1", "U2"), variable_id = "V1",
                         item_id = "old")
  expect_error(add_item_id(data, units), "already contains.*item_id")
  expect_identical(add_item_id(data, units, overwrite = TRUE)$item_id,
                   c("I1", NA_character_))
  expect_error(add_item_id(data, units, overwrite = NA), "overwrite")
  expect_error(add_item_id(data["unit_key"], units), "variable_id")
  expect_error(add_item_id(data[1:2], units["unit_key"]), "items_list.*item_metadata")
  units$items_list <- list(tibble::tibble(item_id = "I1"))
  expect_error(add_item_id(data[1:2], units), "variable_id")
})
