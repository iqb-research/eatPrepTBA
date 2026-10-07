legacy_get_design_order <- function(...) eatPrepTBA:::get_design_order(..., order_method = 'structure')

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
  out <- legacy_get_design_order(design, units)
  b1 <- out[out$booklet_id == "B1", ]
  expect_identical(b1$variable_order, 1:7)
  expect_identical(b1$variable_id, c("V2", "V10", "V1", "Q", "V2", "V10", "V1"))
  expect_identical(out$variable_order[out$booklet_id == "B2"], 1:4)
  expect_false(any(c("login_name", "booklet_no") %in% names(out)))
  expect_identical(legacy_get_design_order(design[nrow(design):1L, ], units[2:1, ]), out)
})

test_that("natural fallback is deterministic without interpreting element IDs as paths", {
  units <- order_test_units(c("10", "2", "01", "1"),
                             section = rep("section1", 4), element = c("1", "2", "3", "4"))
  out <- legacy_get_design_order(order_test_design(), units)
  expect_setequal(out$variable_id[1:2], c("01", "1"))
  expect_identical(out$variable_id[3:4], c("2", "10"))
  expect_true(all(out$order_source == "page_naming"))
  expect_true(all(out$order_group == 1L))
  units$unit_codes[[1]] <- units$unit_codes[[1]][4:1, ]
  expect_identical(legacy_get_design_order(order_test_design(), units), out)
})

test_that("numeric structural paths precede naming and partial paths fall back together", {
  units <- order_test_units(c("V1", "V2", "V3"))
  units$unit_codes[[1]]$variable_section <- list(c(1, 10), c(1, 2), c(0, 9))
  out <- legacy_get_design_order(order_test_design(), units)
  expect_identical(out$variable_id, c("V3", "V2", "V1"))
  expect_true(all(out$order_source == "page_section_element"))
  units$unit_codes[[1]]$variable_section <- list(0, NA_integer_, 0)
  out <- legacy_get_design_order(order_test_design(), units)
  expect_identical(out$variable_id, c("V1", "V2", "V3"))
  expect_true(all(out$order_source == "page_naming"))
})

test_that("unknown pages use natural display order without overriding known positions", {
  units <- order_test_units(c("V1", "V2", "V10"), page = c(1, NA, 2))
  out <- legacy_get_design_order(order_test_design(), units)
  expect_identical(out$variable_id, c("V1", "V2", "V10"))
  expect_identical(out$order_source, c("page_section_element", "naming", "page_section_element"))

  units <- order_test_units(c("V1", "V2", "V3"), page = c(2, NA, 1))
  out <- legacy_get_design_order(order_test_design(), units)
  expect_identical(out$variable_id, c("V2", "V3", "V1"))
  expect_identical(out$variable_order, 1:3)
  units$unit_codes[[1]] <- units$unit_codes[[1]][3:1, ]
  expect_identical(legacy_get_design_order(order_test_design(), units), out)
})

test_that("known siblings retain their physical order despite a missing sibling path", {
  units <- order_test_units(c("V1", "V2", "V3"), section = c(2, NA, 1))
  out <- legacy_get_design_order(order_test_design(), units)
  expect_identical(out$variable_id, c("V2", "V3", "V1"))
  units$unit_codes[[1]]$variable_section <- 0L
  units$unit_codes[[1]]$variable_element <- c(2, NA, 1)
  expect_identical(legacy_get_design_order(order_test_design(), units)$variable_id,
                   c("V2", "V3", "V1"))
})

test_that("analytic precedence uses known locations and leaves unknown pages incomparable", {
  metadata <- eatPrepTBA:::design_order_metadata(
    order_test_units(c("V1", "V2", "V3"), page = c(1, NA, 2))
  )
  before <- eatPrepTBA:::design_order_precedence(metadata, c(3L, 2L, 1L))
  expected <- matrix(FALSE, 3, 3)
  expected[1, 3] <- TRUE
  expect_identical(before, expected)

  confirmed <- eatPrepTBA:::design_order_precedence(
    metadata, c(3L, 2L, 1L), use_variable_names_for_recoding = TRUE
  )
  expect_identical(confirmed, upper.tri(expected))

  metadata$variable_page <- c(1, 1, 2)
  metadata$variable_section <- rep(list(NA_integer_), 3)
  before <- eatPrepTBA:::design_order_precedence(
    metadata, 1:3, order_source = rep("page_naming", 3)
  )
  expected[2, 3] <- TRUE
  expect_identical(before, expected)
})

test_that("unconfirmed display ranks and element identifiers do not prove precedence", {
  metadata <- tibble::tibble(unit_key = "U1", variable_id = c("V1", "V2"))
  expect_identical(eatPrepTBA:::design_order_precedence(metadata, c(2L, 1L)),
                   matrix(FALSE, 2, 2))
  confirmed <- eatPrepTBA:::design_order_precedence(
    metadata, c(2L, 1L), use_variable_names_for_recoding = TRUE
  )
  expect_true(confirmed[1, 2])
  expect_false(confirmed[2, 1])

  metadata$variable_page <- 1
  metadata$variable_section <- list("section1", "section2")
  metadata$variable_element <- list(1, 2)
  expect_identical(eatPrepTBA:::design_order_precedence(metadata, 1:2),
                   matrix(FALSE, 2, 2))
  expect_identical(eatPrepTBA:::design_order_precedence(metadata[0, ], integer()),
                   matrix(FALSE, 0, 0))
})

test_that("confirming names rejects conflicts with known physical precedence", {
  metadata <- eatPrepTBA:::design_order_metadata(
    order_test_units(c("V1", "V2"), page = c(2, 1))
  )
  expect_error(eatPrepTBA:::design_order_precedence(
    metadata, c(2L, 1L), use_variable_names_for_recoding = TRUE
  ), "unit.*U1.*V2.*V1")
  expect_error(eatPrepTBA:::design_order_precedence(
    metadata, c(2L, 1L), use_variable_names_for_recoding = TRUE
  ), "order_overrides")

  metadata$variable_page <- 1
  metadata$variable_section <- list(2, 1)
  expect_error(eatPrepTBA:::design_order_precedence(
    metadata, c(2L, 1L), use_variable_names_for_recoding = TRUE
  ), "conflicts with physical metadata")
  metadata$variable_section <- list(0, 0)
  metadata$variable_element <- list(2, 1)
  expect_error(eatPrepTBA:::design_order_precedence(
    metadata, c(2L, 1L), use_variable_names_for_recoding = TRUE
  ), "conflicts with physical metadata")
})

test_that("complete occurrence overrides establish precedence independently of names", {
  metadata <- eatPrepTBA:::design_order_metadata(order_test_units())
  expected <- matrix(FALSE, 2, 2)
  expected[2, 1] <- TRUE
  for (use_names in c(FALSE, TRUE)) {
    expect_identical(eatPrepTBA:::design_order_precedence(
      metadata, c(2L, 1L), order_source = c("override", "override"),
      use_variable_names_for_recoding = use_names
    ), expected)
  }
  expect_error(eatPrepTBA:::design_order_precedence(
    metadata, c(2L, 1L), order_source = c("override", "page_naming")
  ), "every basis variable")
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
  out <- legacy_get_design_order(order_test_design(), units)
  expect_identical(out$variable_id, c("01a", "01", "same", "01b", "02", "03"))
  expect_identical(out$variable_order, 1:6)
  metadata <- eatPrepTBA:::design_order_metadata(units)
  expect_identical(metadata$source_ids[[match("02", metadata$variable_id)]], c("01", "01b"))
  expect_setequal(metadata$basis_sources[[match("03", metadata$variable_id)]], c("01a", "01b"))
  expect_true(all(metadata$sources_known))
  expect_identical(eatPrepTBA:::design_order_metadata(metadata), metadata)
  units$unit_codes[[1]] <- units$unit_codes[[1]][6:1, ]
  expect_identical(legacy_get_design_order(order_test_design(), units), out)
})

test_that("unknown, empty, and cyclic derivations never acquire a trusted basis", {
  units <- order_test_units(
    c("base", "empty", "missing", "cycle1", "cycle2", "dependent"),
    type = c("BASE", rep("SOLVER", 5)),
    sources = list(character(), character(), "unknown", "cycle2", "cycle1", c("base", "missing"))
  )
  out <- legacy_get_design_order(order_test_design(), units)
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
  out <- legacy_get_design_order(order_test_design(), units)
  expect_false("inactive" %in% out$variable_id)
  expect_false(out$sources_known[out$variable_id == "derived"])
})

test_that("overrides are complete basis orders with optional occurrence scope", {
  units <- order_test_units(c("A", "B", "derived"), type = c("BASE", "BASE", "SOLVER"),
                             sources = list(character(), character(), "B"))
  design <- dplyr::bind_rows(order_test_design("B1"), order_test_design("B2"))
  override <- tibble::tibble(unit_key = "U1", variable_id = c("A", "B"),
                              local_order = c(20, 10), booklet_id = "b2")
  out <- legacy_get_design_order(design, units, order_overrides = override)
  expect_identical(out$variable_id[out$booklet_id == "B1"], c("A", "B", "derived"))
  expect_identical(out$variable_id[out$booklet_id == "B2"], c("B", "derived", "A"))
  expect_true(all(out$order_source[out$booklet_id == "B2" & out$variable_source_type == "BASE"] == "override"))
  expect_error(legacy_get_design_order(design, units, order_overrides = override[1, ]), "full basis")
  bad <- override
  bad$local_order <- 1
  expect_error(legacy_get_design_order(design, units, order_overrides = bad), "Duplicate")
  bad <- override
  bad$booklet_id <- "unknown"
  expect_error(legacy_get_design_order(design, units, order_overrides = bad), "do not match")
  bad <- override
  bad$variable_id[1] <- "derived"
  expect_error(legacy_get_design_order(design, units, order_overrides = bad), "derived or unknown")
})

test_that("Studio items and explicit selections have separate consecutive indices", {
  units <- order_test_units(c("V1", "V2", "V3"))
  units$items_list <- list(tibble::tibble(variable_id = c("V1", "V2", "V3"), item_id = c("I1", "I2", "I3")))
  design <- dplyr::bind_rows(order_test_design("B1", number = 1L, alias = "first"),
                             order_test_design("B1", number = 2L, alias = "second"))
  full <- legacy_get_design_order(design, units)
  expect_identical(full$variable_order, 1:6)
  expect_identical(full$item_order, 1:6)
  selection <- tibble::tibble(unit_key = "U1", variable_id = c("V3", "V1"))
  selected <- legacy_get_design_order(design, units, item_selection = selection)
  expect_identical(selected$variable_order, full$variable_order)
  expect_identical(selected$item_order, c(1L, NA_integer_, 3L, 4L, NA_integer_, 6L))
  no_metadata <- dplyr::select(units, -items_list)
  selected <- legacy_get_design_order(design, no_metadata, item_selection = selection)
  expect_true(all(is.na(selected$item_id)))
  expect_identical(selected$item_order, c(1L, NA_integer_, 2L, 3L, NA_integer_, 4L))
  selection$item_id <- c("chosen3", "chosen1")
  selected <- legacy_get_design_order(design, no_metadata, item_selection = selection)
  expect_identical(selected$item_id[1:3], c("chosen1", NA_character_, "chosen3"))
  default <- legacy_get_design_order(design, no_metadata)
  expect_true(all(is.na(default$item_order)))
  expect_true(all(default$item_order_source == "unavailable"))
})

test_that("ambiguous Studio item mappings require an explicit scoring variable", {
  units <- order_test_units(c("V1", "V2"))
  units$items_list <- list(tibble::tibble(variable_id = c("V1", "V2"), item_id = "I1"))
  expect_error(legacy_get_design_order(order_test_design(), units), "Several variables map")
  selected <- legacy_get_design_order(order_test_design(), units,
                                item_selection = tibble::tibble(unit_key = "U1", variable_id = "V2"))
  expect_identical(selected$item_order, c(NA_integer_, 2L))
})

test_that("invalid keys, conflicting occurrences and selections fail clearly", {
  units <- order_test_units()
  design <- order_test_design()
  bad <- design
  bad$unit_key <- "unknown"
  expect_error(legacy_get_design_order(bad, units), "Unknown or empty units")
  bad <- design
  bad$variable_id <- "unknown"
  expect_error(legacy_get_design_order(bad, units), "Unknown variables")
  bad <- dplyr::bind_rows(design, dplyr::mutate(design, unit_alias = "different"))
  expect_error(legacy_get_design_order(bad, units), "Conflicting unit occurrences")
  selection <- tibble::tibble(unit_key = "U1", variable_id = "unknown")
  expect_error(legacy_get_design_order(design, units, item_selection = selection), "Unknown variables")
  selection$variable_id <- "V1"
  expect_error(legacy_get_design_order(design, units, item_selection = dplyr::bind_rows(selection, selection)), "Duplicate")
  bad <- units
  bad$unit_codes[[1]]$variable_ref <- "same"
  expect_error(legacy_get_design_order(design, bad), "Multiple variable aliases")
})

test_that("empty designs retain the public schema", {
  out <- legacy_get_design_order(order_test_design()[0, ], order_test_units())
  expect_equal(nrow(out), 0)
  expect_type(out$variable_order, "integer")
  expect_type(out$item_order, "integer")
  expect_named(out, c("booklet_id", "testlet_no", "unit_booklet_no", "unit_key", "unit_alias",
                       "variable_id", "variable_ref", "variable_source_type", "variable_level",
                       "variable_page", "variable_section", "variable_element", "variable_page_always_visible",
                       "source_ids", "basis_sources", "sources_known", "item_id", "item_position",
                       "box_position", "box_included", "box_is_item", "analysis_included",
                       "variable_order", "order_group", "order_source", "position_group",
                       "position_source", "item_order_source", "item_order"))
})

test_that("VOMD item-list positions resolve references and retain sources in one group", {
  units <- order_test_units(
    c("01a", "01b", "01", "02", "orphan"),
    refs = c("source-a", "source-b", "derived-ref", "second-ref", "orphan-ref"),
    type = c("BASE", "BASE", "SUM_CODE", "BASE", "BASE"),
    page = rep(NA_real_, 5),
    sources = list(character(), character(), c("source-a", "source-b"),
                   character(), character())
  )
  units$items_list <- list(tibble::tibble(
    item_no = c(1L, 2L), item_id = c("I1", "I2"),
    variable_id = c("obsolete-alias", "02"),
    variable_ref = c("derived-ref", "second-ref")
  ))
  out <- eatPrepTBA:::get_design_order(order_test_design(), units)
  expect_identical(out$variable_id, c("01a", "01b", "01", "02", "orphan"))
  expect_identical(out$variable_order, 1:5)
  expect_identical(out$position_group, c(1, 1, 1, 2, NA_real_))
  expect_identical(out$item_order, c(NA_integer_, NA_integer_, 1L, 2L, NA_integer_))
  expect_identical(out$box_is_item, c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(out$box_included, c(TRUE, TRUE, TRUE, TRUE, FALSE))
  relation <- attr(out, "design_precedence")[[1L]]$before
  expect_false(relation[1, 2])
  expect_false(relation[2, 1])
  expect_true(all(relation[1:3, 4]))
  expect_false(any(relation[5, ] | relation[, 5]))
})

test_that("VOMD positions ignore unrelated physical order and item selection", {
  units <- order_test_units(c("A", "B", "C"), page = c(3, 2, 1))
  units$items_list <- list(tibble::tibble(
    variable_id = c("B", "A", "C"), item_id = c("IB", "IA", "IC"),
    item_no = c(1L, 2L, 3L), item_order = c(99, 1, 50)
  ))
  full <- eatPrepTBA:::get_design_order(order_test_design(), units)
  selected <- eatPrepTBA:::get_design_order(order_test_design(), units,
    item_selection = tibble::tibble(unit_key = "U1", variable_id = "C"))
  expect_identical(full$variable_id, c("B", "A", "C"))
  expect_identical(selected$variable_order, full$variable_order)
  expect_identical(selected$position_group, full$position_group)
  expect_identical(selected$box_included, full$box_included)
  expect_identical(selected$item_order, c(NA_integer_, NA_integer_, 3L))
  expect_equal(attr(selected, "design_precedence"), attr(full, "design_precedence"))
  structure <- eatPrepTBA:::get_design_order(order_test_design(), units, order_method = "structure")
  expect_identical(structure$variable_id, c("C", "B", "A"))
  expect_identical(structure$item_order, c(3L, 1L, 2L))
  expect_error(eatPrepTBA:::get_design_order(order_test_design(), units,
    order_method = "hybrid"), "VOMD order conflicts")
})

test_that("shared hidden sources stay unlocated while direct mapped sources keep their anchor", {
  units <- order_test_units(c("shared", "A", "B", "chain"),
    type = c("BASE", "SUM_CODE", "SUM_CODE", "SUM_CODE"),
    sources = list(character(), "shared", "shared", "A"))
  units$items_list <- list(tibble::tibble(
    variable_id = c("chain", "B"), item_id = c("IC", "IB"), item_no = c(1L, 2L)
  ))
  out <- eatPrepTBA:::get_design_order(order_test_design(), units)
  expect_false(out$box_included[out$variable_id == "shared"])
  expect_true(is.na(out$box_position[out$variable_id == "shared"]))
  expect_equal(out$box_position[out$variable_id == "A"], 1)
  expect_false(out$box_is_item[out$variable_id == "A"])
  expect_true(out$box_included[out$variable_id == "A"])
  units$items_list[[1]] <- dplyr::bind_rows(
    units$items_list[[1]], tibble::tibble(variable_id = "shared", item_id = "IS", item_no = 3L))
  mapped <- eatPrepTBA:::get_design_order(order_test_design(), units)
  expect_equal(mapped$box_position[mapped$variable_id == "shared"], 3)
  expect_true(mapped$box_included[mapped$variable_id == "shared"])
  expect_lt(mapped$variable_order[mapped$variable_id == "shared"],
            mapped$variable_order[mapped$variable_id == "A"])
})

test_that("hybrid metadata refines a VOMD group without trusting display indices", {
  units <- order_test_units(c("a", "b", "derived", "orphan"),
    type = c("BASE", "BASE", "SUM_CODE", "BASE"),
    page = c(1, 2, NA, NA),
    sources = list(character(), character(), c("a", "b"), character()))
  units$items_list <- list(tibble::tibble(variable_id = "derived", item_id = "I", item_no = 1L))
  vomd <- eatPrepTBA:::get_design_order(order_test_design(), units)
  hybrid <- eatPrepTBA:::get_design_order(order_test_design(), units, order_method = "hybrid")
  relation <- attr(hybrid, "design_precedence")[[1L]]
  a <- match("a", relation$variable_ids)
  b <- match("b", relation$variable_ids)
  orphan <- match("orphan", relation$variable_ids)
  expect_true(relation$before[a, b])
  expect_false(any(relation$before[orphan, ] | relation$before[, orphan]))
  expect_false(attr(vomd, "design_precedence")[[1L]]$before[a, b])
  expect_identical(hybrid$box_position, vomd$box_position)
  named <- eatPrepTBA:::get_design_order(order_test_design(), units,
    use_variable_names_for_recoding = TRUE)
  expect_true(attr(named, "design_precedence")[[1L]]$before[a, b])
})

test_that("orphan variables have only unit-level positions by default", {
  units <- order_test_units(c("B", "A"), page = c(1, 2))
  out <- eatPrepTBA:::get_design_order(order_test_design(), units)
  expect_identical(out$variable_id, c("A", "B"))
  expect_true(all(is.na(out$position_group)))
  expect_false(any(out$box_included))
  expect_false(any(attr(out, "design_precedence")[[1L]]$before))
  named <- eatPrepTBA:::get_design_order(order_test_design(), units,
    use_variable_names_for_recoding = TRUE)
  expect_true(attr(named, "design_precedence")[[1L]]$before[1, 2])
  override <- tibble::tibble(unit_key = "U1", variable_id = c("A", "B"), local_order = 2:1)
  explicit <- eatPrepTBA:::get_design_order(order_test_design(), units, order_overrides = override)
  expect_identical(explicit$variable_id, c("B", "A"))
  expect_identical(explicit$position_source, c("override", "override"))
  expect_true(attr(explicit, "design_precedence")[[1L]]$before[1, 2])
})

test_that("directly mapped intermediate items re-anchor their hidden source closure", {
  units <- order_test_units(c("base", "A", "B"),
    type = c("BASE", "SUM_CODE", "SUM_CODE"),
    sources = list(character(), "base", "A"))
  units$items_list <- list(tibble::tibble(
    variable_id = c("A", "B"), item_id = c("IA", "IB"), item_no = 1:2))
  out <- eatPrepTBA:::get_design_order(order_test_design(), units)
  expect_identical(out$box_position, c(1, 1, 2))
  expect_true(all(out$box_included))
  expect_identical(out$box_is_item, c(FALSE, TRUE, TRUE))
})

test_that("unresolved VOMD targets remain available as static diagnostics", {
  units <- order_test_units("active")
  units$items_list <- list(tibble::tibble(
    variable_id = c("active", "absent"), item_id = c("IA", "IX"), item_no = 1:2))
  out <- eatPrepTBA:::get_design_order(order_test_design(), units)
  unresolved <- attr(out, "vomd_unresolved")
  expect_identical(unresolved$variable_id, "absent")
  expect_identical(unresolved$item_id, "IX")
  expect_equal(unresolved$item_position, 2)
  expect_identical(out$variable_id, "active")
})

test_that("ready derived blocks follow their last source regardless of names or item position", {
  units <- order_test_units(c("A", "B", "ZZ", "ZZ2"),
    type = c("BASE", "BASE", "SUM_CODE", "SUM_CODE"),
    sources = list(character(), character(), "A", "A"))
  units$items_list <- list(tibble::tibble(variable_id = c("A", "B", "ZZ", "ZZ2"),
    item_id = c("IA", "IB", "IZ", "IZ2"), item_no = 1:4))
  out <- eatPrepTBA:::get_design_order(order_test_design(), units)
  expect_identical(out$variable_id, c("A", "ZZ", "ZZ2", "B"))
  expect_identical(out$variable_order, 1:4)
  expect_identical(out$item_order, c(1L, 3L, 4L, 2L))
  expect_identical(out$position_group, c(1, 3, 4, 2))
  relation <- attr(out, "design_precedence")[[1L]]$before
  expect_true(relation[4, 2])
  expect_false(relation[2, 4])
})
