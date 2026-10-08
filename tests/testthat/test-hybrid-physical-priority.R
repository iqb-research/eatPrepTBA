hybrid_priority_fixture <- function(ids, page, item_ids = ids,
                                    sources = rep(list(character()), length(ids))) {
  derived <- lengths(sources) > 0L
  units <- tibble::tibble(unit_key = "U",
    unit_codes = list(tibble::tibble(variable_id = ids, variable_ref = ids,
      variable_source_type = ifelse(derived, "SUM_CODE", "BASE"),
      variable_level = ifelse(derived, 1L, 0L), variable_page = page,
      variable_section = 0L, variable_element = 0L,
      variable_page_always_visible = FALSE, derive_sources = sources)),
    items_list = list(tibble::tibble(variable_id = item_ids,
      item_id = paste0("I", seq_along(item_ids)), item_no = seq_along(item_ids))))
  design <- tibble::tibble(login_code = "P", booklet_id = "B", booklet_no = 1L,
    testlet_no = 1L, unit_booklet_no = 1L, unit_key = "U", unit_alias = "U")
  list(units = units, design = design)
}

hybrid_priority_order <- function(f, ...) {
  eatPrepTBA:::get_design_order(f$design, f$units, order_method = "hybrid", ...)
}

hybrid_priority_relation <- function(out) {
  entry <- attr(out, "design_precedence")[[1L]]
  relation <- entry$before
  dimnames(relation) <- list(entry$variable_ids, entry$variable_ids)
  relation
}

test_that("hybrid follows pages across an obsolete item permutation and warns once per unit", {
  ids <- sprintf("%02d", 1:9)
  items <- ids[c(2, 4, 5, 6, 1, 3, 7, 8, 9)]
  f <- hybrid_priority_fixture(ids, 1:9, items)
  f$design <- dplyr::bind_rows(f$design, dplyr::mutate(f$design, booklet_id = "B2"))
  warnings <- list()
  out <- withCallingHandlers(hybrid_priority_order(f), warning = function(cnd) {
    warnings[[length(warnings) + 1L]] <<- cnd
    invokeRestart("muffleWarning")
  })
  expect_length(warnings, 1L)
  expect_s3_class(warnings[[1L]], "eatPrepTBA_order_conflict")
  expect_identical(out$variable_id, rep(ids, 2))
  expect_true(hybrid_priority_relation(out)["01", "02"])
  diagnostics <- attr(out, "order_conflicts")
  expect_true(all(diagnostics$discarded_source == "vomd"))
  expect_true(all(diagnostics$retained_source == "structure"))
  expect_true(any(diagnostics$before_variable_id == "02" & diagnostics$after_variable_id == "01"))
  expect_false(anyDuplicated(diagnostics) > 0L)

  vomd <- eatPrepTBA:::get_design_order(f$design, f$units, order_method = "vomd")
  expect_identical(vomd$variable_id, rep(items, 2))
  expect_false(hybrid_priority_relation(vomd)["01", "02"])
  expect_equal(nrow(attr(vomd, "order_conflicts")), 0L)
})

test_that("source presentation and later derived item anchors do not invent conflicts", {
  f <- hybrid_priority_fixture(c("A", "B", "D"), c(1, 2, NA),
    sources = list(character(), character(), "A"))
  expect_no_warning(out <- hybrid_priority_order(f))
  expect_identical(out$variable_id, c("A", "D", "B"))
  expect_equal(out$item_order, c(1L, 3L, 2L))
  expect_equal(out$box_position, c(1, 3, 2))
  expect_equal(out$position_group, c(1, 1, 2))
  relation <- hybrid_priority_relation(out)
  expect_true(relation["A", "B"])
  expect_true(relation["D", "B"])
  expect_false(relation["B", "D"])
  expect_false(relation["A", "D"])
  expect_equal(nrow(attr(out, "order_conflicts")), 0L)

  coded <- f$design[rep(1L, 3), ]
  coded$variable_id <- c("A", "B", "D")
  coded$code_status <- c("NOT_REACHED", "CODING_COMPLETE", "CODING_COMPLETE")
  coded$code_id <- coded$code_score <- c(NA_real_, 1, 0)
  coded$code_type <- c(NA_character_, "FULL_CREDIT", "NO_CREDIT")
  coded$value <- c(NA_character_, "answer", NA_character_)
  coded$response_present <- TRUE
  expect_no_warning(completed <- complete_design(coded, f$units, f$design,
    missing_policy = "eatPrepTBA", diagnostics = "none", progress = FALSE))
  expect_equal(completed$code_id[match(c("A", "B", "D"), completed$variable_id)], c(-99, 1, 0))
  expect_equal(completed$code_score[match(c("A", "B", "D"), completed$variable_id)], c(0, 1, 0))
})

test_that("indirect VOMD cycles discard every ambiguous lower-tier edge deterministically", {
  f <- hybrid_priority_fixture(c("A", "B", "C", "D"), c(1, NA, 2, 3),
    item_ids = c("C", "B", "A", "D"))
  expect_warning(out <- hybrid_priority_order(f), "Order conflict")
  relation <- hybrid_priority_relation(out)
  expect_true(relation["A", "C"])
  expect_true(all(relation[c("A", "B", "C"), "D"]))
  expect_false(any(relation["B", c("A", "C")]))
  expect_false(any(relation[c("A", "C"), "B"]))
  conflicts <- attr(out, "order_conflicts")
  expect_setequal(paste(conflicts$before_variable_id, conflicts$after_variable_id),
    c("B A", "C A", "C B"))
  expect_equal(sum(conflicts$reason == "indirect_cycle"), 2L)

  f$units$unit_codes[[1L]] <- f$units$unit_codes[[1L]][4:1, ]
  f$units$items_list[[1L]] <- f$units$items_list[[1L]][4:1, ]
  expect_warning(reversed <- hybrid_priority_order(f), "Order conflict")
  expect_identical(reversed, out)
})

test_that("trusted names remain below known physical and item relationships", {
  f <- hybrid_priority_fixture(c("A", "B", "C"), c(2, NA, 1), item_ids = character())
  expect_warning(out <- hybrid_priority_order(f, use_variable_names_for_recoding = TRUE),
    "Order conflict")
  relation <- hybrid_priority_relation(out)
  expect_true(relation["C", "A"])
  expect_false(any(relation["B", ] | relation[, "B"]))
  expect_true(all(attr(out, "order_conflicts")$discarded_source == "naming"))

  f <- hybrid_priority_fixture(c("A", "B", "C"), rep(NA_real_, 3), c("C", "A"))
  expect_warning(out <- hybrid_priority_order(f, use_variable_names_for_recoding = TRUE),
    "Order conflict")
  relation <- hybrid_priority_relation(out)
  expect_true(relation["C", "A"])
  expect_false(any(relation["B", ] | relation[, "B"]))
})

test_that("unlocated bases interleave for display and shared sources retain uncertainty", {
  f <- hybrid_priority_fixture(c("V1", "V2", "V10"), c(1, NA, 2), item_ids = character())
  out <- hybrid_priority_order(f)
  expect_identical(out$variable_id, c("V1", "V2", "V10"))
  relation <- hybrid_priority_relation(out)
  expect_false(any(relation["V2", ] | relation[, "V2"]))
  trusted <- hybrid_priority_order(f, use_variable_names_for_recoding = TRUE)
  relation <- hybrid_priority_relation(trusted)
  expect_true(relation["V1", "V2"])
  expect_true(relation["V2", "V10"])

  shared <- hybrid_priority_fixture(c("shared", "A", "B", "later"), rep(NA_real_, 4),
    item_ids = c("A", "B", "later"),
    sources = list(character(), "shared", "shared", character()))
  out <- hybrid_priority_order(shared)
  relation <- hybrid_priority_relation(out)
  expect_false(any(relation["shared", ] | relation[, "shared"]))
  expect_false(any(relation[c("A", "B"), "later"]))
})

test_that("complete overrides remain authoritative over physical and VOMD positions", {
  f <- hybrid_priority_fixture(c("A", "B", "D"), c(1, 2, NA),
    sources = list(character(), character(), "A"))
  override <- tibble::tibble(unit_key = "U", variable_id = c("A", "B"), local_order = 2:1)
  expect_no_warning(out <- hybrid_priority_order(f, order_overrides = override,
    use_variable_names_for_recoding = TRUE))
  expect_identical(out$variable_id, c("B", "A", "D"))
  expect_true(hybrid_priority_relation(out)["B", "A"])
  expect_equal(nrow(attr(out, "order_conflicts")), 0L)
  expect_error(hybrid_priority_order(f, order_overrides = override[1, ]), "full basis")
})

test_that("physical priority fixes NR codes and scores while coding_box retains VOMD behavior", {
  ids <- sprintf("%02d", 1:9)
  f <- hybrid_priority_fixture(ids, 1:9, ids[c(2, 4, 5, 6, 1, 3, 7, 8, 9)])
  coded <- f$design[rep(1L, length(ids)), ]
  coded$variable_id <- ids
  coded$code_status <- ifelse(ids == "02", "CODING_COMPLETE", "NOT_REACHED")
  coded$code_id <- coded$code_score <- ifelse(ids == "02", 1, NA_real_)
  coded$code_type <- ifelse(ids == "02", "FULL_CREDIT", NA_character_)
  coded$value <- ifelse(ids == "02", "answer", NA_character_)
  coded$response_present <- TRUE
  expect_warning(out <- complete_design(coded, f$units, f$design,
    missing_policy = "eatPrepTBA", diagnostics = "none", progress = FALSE), "Order conflict")
  at <- match(ids, out$variable_id)
  expect_equal(out$code_id[at], c(-99, 1, rep(-96, 7)))
  expect_equal(out$code_score[at], c(0, 1, rep(NA_real_, 7)))
  expect_identical(out$code_status[at], coded$code_status)
  box <- complete_design(coded, f$units, f$design,
    missing_policy = "coding_box", diagnostics = "none", progress = FALSE)
  expect_equal(box$code_id[box$variable_id == "01"], -96)
  expect_true(is.na(box$code_score[box$variable_id == "01"]))
})
