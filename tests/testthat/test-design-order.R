order_test_units <- function(ids = c("V1", "V2"), refs = ids,
                              type = rep("BASE", length(ids)),
                              page = rep(1, length(ids)),
                              section = rep(0, length(ids)),
                              element = seq_along(ids), sources = rep(list(character()), length(ids))) {
  tibble::tibble(
    unit_key = "U1",
    unit_codes = list(tibble::tibble(
      variable_id = ids, variable_ref = refs,
      variable_source_type = type,
      variable_level = ifelse(type == "BASE", 0L, 1L),
      variable_page = page, variable_section = section, variable_element = element,
      variable_page_always_visible = FALSE,
      variable_sources = lapply(sources, function(source) {
        tibble::tibble(variable_source_ref = source, variable_source_direct = TRUE)
      })
    ))
  )
}

order_test_design <- function(booklet = "b1", key = "U1", number = 1L, alias = key,
                               testlet = 1L) {
  tibble::tibble(booklet_id = booklet, testlet_no = testlet,
                 unit_booklet_no = number, unit_key = key, unit_alias = alias)
}

test_that("static booklet ranks ignore persons and expand full unit occurrences", {
  units <- order_test_units(c("V1", "V10", "V2"), page = c(2, 1, 1), element = c(0, 1, 0))
  second <- order_test_units("Q")
  second$unit_key <- "U2"
  units <- dplyr::bind_rows(units, second)
  design <- dplyr::bind_rows(
    order_test_design("b1", "U1", 1L, "first"),
    order_test_design("B1", "U2", 2L, "middle"),
    order_test_design("B1", "U1", 3L, "repeat", 2L),
    order_test_design("B2", "U2", 1L),
    order_test_design("B2", "U1", 2L)
  )
  design$variable_id <- ifelse(design$unit_key == "U1", "V1", "Q")
  design <- dplyr::bind_rows(dplyr::mutate(design, login_name = "A", booklet_no = 1L),
                             dplyr::mutate(design, login_name = "B", booklet_no = 4L))
  out <- get_design_order(design, units)
  b1 <- out[out$booklet_id == "B1", ]
  expect_identical(b1$variable_order, 1:7)
  expect_identical(b1$variable_id, c("V2", "V10", "V1", "Q", "V2", "V10", "V1"))
  expect_identical(out$variable_order[out$booklet_id == "B2"], 1:4)
  expect_false(any(c("login_name", "booklet_no") %in% names(out)))
  expect_identical(get_design_order(design[nrow(design):1L, ], units[2:1, ]), out)
})

test_that("natural fallback is deterministic without interpreting element IDs as paths", {
  units <- order_test_units(c("10", "2", "01", "1"),
                             section = rep("section1", 4), element = c("1", "2", "3", "4"))
  out <- get_design_order(order_test_design(), units)
  expect_setequal(out$variable_id[1:2], c("01", "1"))
  expect_identical(out$variable_id[3:4], c("2", "10"))
  expect_true(all(out$order_source == "page_naming"))
  expect_true(all(out$order_group == 1L))
  units$unit_codes[[1]] <- units$unit_codes[[1]][4:1, ]
  expect_identical(get_design_order(order_test_design(), units), out)
})

test_that("numeric structural paths precede naming and partial paths fall back together", {
  units <- order_test_units(c("V1", "V2", "V3"))
  units$unit_codes[[1]]$variable_section <- list(c(1, 10), c(1, 2), c(0, 9))
  out <- get_design_order(order_test_design(), units)
  expect_identical(out$variable_id, c("V3", "V2", "V1"))
  expect_true(all(out$order_source == "page_section_element"))
  units$unit_codes[[1]]$variable_section <- list(0, NA_integer_, 0)
  out <- get_design_order(order_test_design(), units)
  expect_identical(out$variable_id, c("V1", "V2", "V3"))
  expect_true(all(out$order_source == "page_naming"))
})

test_that("derived blocks resolve original references, siblings, and chains", {
  units <- order_test_units(
    ids = c("01a", "01b", "01", "same", "02", "03"),
    refs = c("base-a", "base-b", "derived-one", "sibling", "combined", "chain"),
    type = c("BASE", "BASE", rep("SOLVER", 4)),
    element = c(0, 1, NA, NA, NA, NA),
    sources = list(character(), character(), "base-a", "base-a",
                   c("derived-one", "base-b"), "combined")
  )
  out <- get_design_order(order_test_design(), units)
  expect_identical(out$variable_id, c("01a", "01", "same", "01b", "02", "03"))
  expect_identical(out$variable_order, 1:6)
  metadata <- eatPrepTBA:::design_order_metadata(units)
  expect_identical(metadata$source_ids[[match("02", metadata$variable_id)]], c("01", "01b"))
  expect_setequal(metadata$basis_sources[[match("03", metadata$variable_id)]], c("01a", "01b"))
  expect_true(all(metadata$sources_known))
  expect_identical(eatPrepTBA:::design_order_metadata(metadata), metadata)
  units$unit_codes[[1]] <- units$unit_codes[[1]][6:1, ]
  expect_identical(get_design_order(order_test_design(), units), out)
})

test_that("unknown, empty, and cyclic derivations never acquire a trusted basis", {
  units <- order_test_units(
    c("base", "empty", "missing", "cycle1", "cycle2", "dependent"),
    type = c("BASE", rep("SOLVER", 5)),
    sources = list(character(), character(), "unknown", "cycle2", "cycle1", c("base", "missing"))
  )
  out <- get_design_order(order_test_design(), units)
  expect_identical(out$variable_id[[1]], "base")
  expect_true(out$sources_known[[1]])
  expect_false(any(out$sources_known[-1]))
  expect_true(all(out$order_source[-1] == "derived_unresolved"))
  expect_length(out$basis_sources[[match("empty", out$variable_id)]], 0)
  expect_true(anyNA(out$source_ids[[match("missing", out$variable_id)]]))
})

test_that("deactivated basis variables do not invent available dependencies", {
  units <- order_test_units(c("base", "inactive", "derived"),
                             type = c("BASE", "BASE_NO_VALUE", "SOLVER"),
                             sources = list(character(), character(), "inactive"))
  out <- get_design_order(order_test_design(), units)
  expect_false("inactive" %in% out$variable_id)
  expect_false(out$sources_known[out$variable_id == "derived"])
})

test_that("overrides are complete basis orders with optional occurrence scope", {
  units <- order_test_units(c("A", "B", "derived"), type = c("BASE", "BASE", "SOLVER"),
                             sources = list(character(), character(), "B"))
  design <- dplyr::bind_rows(order_test_design("B1"), order_test_design("B2"))
  override <- tibble::tibble(unit_key = "U1", variable_id = c("A", "B"),
                              local_order = c(20, 10), booklet_id = "b2")
  out <- get_design_order(design, units, order_overrides = override)
  expect_identical(out$variable_id[out$booklet_id == "B1"], c("A", "B", "derived"))
  expect_identical(out$variable_id[out$booklet_id == "B2"], c("B", "derived", "A"))
  expect_true(all(out$order_source[out$booklet_id == "B2" & out$variable_source_type == "BASE"] == "override"))
  expect_error(get_design_order(design, units, order_overrides = override[1, ]), "full basis")
  bad <- override
  bad$local_order <- 1
  expect_error(get_design_order(design, units, order_overrides = bad), "Duplicate")
  bad <- override
  bad$booklet_id <- "unknown"
  expect_error(get_design_order(design, units, order_overrides = bad), "do not match")
  bad <- override
  bad$variable_id[1] <- "derived"
  expect_error(get_design_order(design, units, order_overrides = bad), "derived or unknown")
})

test_that("Studio items and explicit selections have separate consecutive indices", {
  units <- order_test_units(c("V1", "V2", "V3"))
  units$items_list <- list(tibble::tibble(variable_id = c("V1", "V2", "V3"), item_id = c("I1", "I2", "I3")))
  design <- dplyr::bind_rows(order_test_design("B1", number = 1L, alias = "first"),
                             order_test_design("B1", number = 2L, alias = "second"))
  full <- get_design_order(design, units)
  expect_identical(full$variable_order, 1:6)
  expect_identical(full$item_order, 1:6)
  selection <- tibble::tibble(unit_key = "U1", variable_id = c("V3", "V1"))
  selected <- get_design_order(design, units, item_selection = selection)
  expect_identical(selected$variable_order, full$variable_order)
  expect_identical(selected$item_order, c(1L, NA_integer_, 2L, 3L, NA_integer_, 4L))
  no_metadata <- dplyr::select(units, -items_list)
  selected <- get_design_order(design, no_metadata, item_selection = selection)
  expect_true(all(is.na(selected$item_id)))
  expect_identical(selected$item_order, c(1L, NA_integer_, 2L, 3L, NA_integer_, 4L))
  selection$item_id <- c("chosen3", "chosen1")
  selected <- get_design_order(design, no_metadata, item_selection = selection)
  expect_identical(selected$item_id[1:3], c("chosen1", NA_character_, "chosen3"))
  default <- get_design_order(design, no_metadata)
  expect_true(all(is.na(default$item_order)))
  expect_true(all(default$item_order_source == "unavailable"))
})

test_that("ambiguous Studio item mappings require an explicit scoring variable", {
  units <- order_test_units(c("V1", "V2"))
  units$items_list <- list(tibble::tibble(variable_id = c("V1", "V2"), item_id = "I1"))
  expect_error(get_design_order(order_test_design(), units), "Several variables map")
  selected <- get_design_order(order_test_design(), units,
                                item_selection = tibble::tibble(unit_key = "U1", variable_id = "V2"))
  expect_identical(selected$item_order, c(NA_integer_, 1L))
})

test_that("invalid keys, conflicting occurrences and selections fail clearly", {
  units <- order_test_units()
  design <- order_test_design()
  bad <- design
  bad$unit_key <- "unknown"
  expect_error(get_design_order(bad, units), "Unknown or empty units")
  bad <- design
  bad$variable_id <- "unknown"
  expect_error(get_design_order(bad, units), "Unknown variables")
  bad <- dplyr::bind_rows(design, dplyr::mutate(design, unit_alias = "different"))
  expect_error(get_design_order(bad, units), "Conflicting unit occurrences")
  selection <- tibble::tibble(unit_key = "U1", variable_id = "unknown")
  expect_error(get_design_order(design, units, item_selection = selection), "Unknown variables")
  selection$variable_id <- "V1"
  expect_error(get_design_order(design, units, item_selection = dplyr::bind_rows(selection, selection)), "Duplicate")
  bad <- units
  bad$unit_codes[[1]]$variable_ref <- "same"
  expect_error(get_design_order(design, bad), "Multiple variable aliases")
})

test_that("empty designs retain the public schema", {
  out <- get_design_order(order_test_design()[0, ], order_test_units())
  expect_equal(nrow(out), 0)
  expect_type(out$variable_order, "integer")
  expect_type(out$item_order, "integer")
  expect_named(out, c("booklet_id", "testlet_no", "unit_booklet_no", "unit_key", "unit_alias",
                       "variable_id", "variable_ref", "variable_source_type", "variable_level",
                       "variable_page", "variable_section", "variable_element", "variable_page_always_visible",
                       "source_ids", "basis_sources", "sources_known", "variable_order", "order_group",
                       "order_source", "item_id", "item_order_source", "item_order"))
})
