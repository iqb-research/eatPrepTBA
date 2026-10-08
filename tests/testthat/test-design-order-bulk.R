bulk_order_fixture <- function() {
  units <- tibble::tibble(
    unit_key = c("U1", "U2", "U3"),
    unit_codes = list(
      tibble::tibble(variable_id = c("A", "B", "C"),
        variable_source_type = "BASE", variable_level = 0L,
        variable_page = 1:3, variable_element = 1L),
      tibble::tibble(variable_id = c("X", "Y", "D"),
        variable_source_type = c("BASE", "BASE", "SUM_CODE"),
        variable_level = c(0L, 0L, 1L), variable_page = c(1, 2, NA),
        variable_element = 1L, derive_sources = list(character(), character(), c("X", "Y"))),
      tibble::tibble(variable_id = "Z", variable_source_type = "BASE",
        variable_level = 0L, variable_page = NA_real_, variable_element = NA_integer_)),
    items_list = list(
      tibble::tibble(variable_id = c("A", "B"), item_id = c("I1", "I2"), item_no = 1:2),
      tibble::tibble(variable_id = "D", item_id = "I1", item_no = 1L),
      tibble::tibble(variable_id = character(), item_id = character(), item_no = integer())))
  design <- tibble::tibble(booklet_id = rep(c("B1", "B2", "B3"), each = 3L),
    testlet_no = rep(c(1L, 1L, 2L), 3L), unit_booklet_no = rep(1:3, 3L),
    unit_key = c("U1", "U2", "U1", "U3", "U1", "U2", "U2", "U1", "U3"),
    unit_alias = paste0("occurrence", 1:9))
  list(units = units, design = design)
}

test_that("batched booklets preserve local ranks and occurrence-specific overrides", {
  fixture <- bulk_order_fixture()
  override <- tibble::tibble(unit_key = "U1", variable_id = c("C", "B", "A"),
    local_order = 1:3, booklet_id = "B1", unit_booklet_no = 3L)
  for (method in c("hybrid", "structure", "vomd")) {
    full <- eatPrepTBA:::get_design_order(fixture$design, fixture$units,
      order_method = method, order_overrides = override)
    for (booklet in unique(fixture$design$booklet_id)) {
      partial <- eatPrepTBA:::get_design_order(
        fixture$design[fixture$design$booklet_id == booklet, ], fixture$units,
        order_method = method,
        order_overrides = if (booklet == "B1") override else NULL)
      expect_identical(lapply(full[full$booklet_id == booklet, ], identity),
        lapply(partial, identity))
      expected_relations <- attr(full, "design_precedence")
      expected_relations <- Filter(function(x) x$keys$booklet_id == booklet, expected_relations)
      expect_identical(expected_relations, attr(partial, "design_precedence"))
      expect_identical(attr(full, "order_conflicts"), attr(partial, "order_conflicts"))
    }
    expect_identical(full$variable_id[full$booklet_id == "B1" & full$unit_booklet_no == 3L],
      c("C", "B", "A"))
    expect_identical(full$variable_id[full$booklet_id == "B2" & full$unit_key == "U1"],
      c("A", "B", "C"))
  }
})

test_that("batched item indices retain unselected slots and reset within each booklet", {
  fixture <- bulk_order_fixture()
  selection <- tibble::tibble(unit_key = c("U1", "U2", "U3"),
    variable_id = c("B", "D", "Z"), item_id = c("I2", "I1", NA_character_))
  full <- eatPrepTBA:::get_design_order(fixture$design, fixture$units,
    order_method = "hybrid", item_selection = selection)
  for (booklet in unique(fixture$design$booklet_id)) {
    design <- fixture$design[fixture$design$booklet_id == booklet, ]
    partial <- eatPrepTBA:::get_design_order(design, fixture$units,
      order_method = "hybrid", item_selection = selection[selection$unit_key %in% design$unit_key, ])
    index <- full$booklet_id == booklet
    expect_identical(full$item_order[index], partial$item_order)
    expect_identical(full$item_order_source[index], partial$item_order_source)
  }
  expect_true(all(is.na(full$item_order[full$variable_id == "A"])))
  expect_identical(full$item_order[full$booklet_id == "B2" & full$variable_id == "Z"], 1L)
  expect_identical(full$item_order[full$booklet_id == "B2" & full$variable_id == "B"], 3L)
})

test_that("item duplication is checked within an occurrence across batched booklets", {
  fixture <- bulk_order_fixture()
  fixture$units$items_list[[1L]]$item_id <- c("same", "same")
  expect_error(eatPrepTBA:::get_design_order(fixture$design, fixture$units),
    "Several variables map to the same item")
  selection <- tibble::tibble(unit_key = "U1", variable_id = "A")
  expect_no_error(eatPrepTBA:::get_design_order(fixture$design, fixture$units,
    item_selection = selection))
})
