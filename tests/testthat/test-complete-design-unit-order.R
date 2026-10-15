unit_order_coded <- function(design, types) {
  code_ids <- c(A = 1, O = -99, NR = -96, I = -98, E = -97)
  code_types <- c(A = "FULL_CREDIT", O = "MISSING_BY_OMISSION",
                  NR = "MISSING_NOT_REACHED", I = "MISSING_INVALID_RESPONSE",
                  E = "MISSING_CODING_IMPOSSIBLE")
  statuses <- c(A = "CODING_COMPLETE", O = "DISPLAYED", NR = "NOT_REACHED",
                I = "INVALID", E = "CODING_ERROR")
  scores <- c(A = 1, O = 0, NR = NA_real_, I = 0, E = NA_real_)
  design %>%
    dplyr::select(login_code, booklet_id, unit_key, unit_alias, variable_id) %>%
    dplyr::mutate(
      code_id = unname(code_ids[types]), code_type = unname(code_types[types]),
      code_status = unname(statuses[types]), code_score = unname(scores[types]),
      value = ifelse(types == "A", "answer", NA_character_)
    )
}

unit_order_fixture <- function(types = c("O", "A", "O")) {
  # Natural name order puts V2 before V10, unlike lexical sorting.
  ids <- c("V2", "V10", "V11")
  codes <- tibble::tibble(
    variable_id = ids, variable_source_type = "BASE", variable_level = 0L,
    variable_page = 1L, variable_section = 1L, variable_element = 1:3,
    variable_page_always_visible = FALSE
  )
  design <- tibble::tibble(
    login_code = "P1", booklet_id = "B1", booklet_no = 1L,
    testlet_no = 1L, unit_booklet_no = 1L, unit_key = "U1",
    unit_alias = "first", variable_id = ids
  )
  list(units = tibble::tibble(
    unit_key = "U1", unit_codes = list(codes),
    items_list = list(tibble::tibble(variable_id = ids, item_id = paste0("I", seq_along(ids))))
  ),
       design = design, coded = unit_order_coded(design, types))
}

unit_order_complete <- function(f, recode = FALSE, expect_fallback = FALSE, ...) {
  boundary_warnings <- character()
  out <- withCallingHandlers(suppressMessages(complete_design(
    f$coded, f$units, f$design, identifiers = "login_code",
    recode_omissions_to_not_reached = recode, ...
  )), eatPrepTBA_item_boundary_fallback = function(w) {
    boundary_warnings <<- c(boundary_warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_length(boundary_warnings, as.integer(expect_fallback))
  out
}

unit_order_values <- function(out, f, column = "code_id") {
  out[[column]][match(f$design$variable_id, out$variable_id)]
}

unit_order_fields <- function(out) {
  out %>%
    dplyr::arrange(login_code, booklet_id, testlet_no, unit_booklet_no, variable_id) %>%
    dplyr::select(login_code, booklet_id, testlet_no, unit_booklet_no, unit_key,
                  unit_alias, variable_id, variable_position, value, code_status, code_type,
                  code_id, code_score, id_used)
}

test_that("within-unit order distinguishes inner missings from the trailing region", {
  for (type in c("O", "NR")) {
    f <- unit_order_fixture(c(type, "A", type))
    for (recode in c(FALSE, TRUE)) {
      out <- unit_order_complete(f, recode)
      expected <- if (type == "O" && !recode) c(-99, 1, -99) else c(-99, 1, -96)
      expect_equal(unit_order_values(out, f), expected)
      expect_equal(unit_order_values(out, f, "code_score"),
                   if (type == "O" && !recode) c(0, 1, 0) else c(0, 1, NA_real_))
      expect_equal(unit_order_values(out, f, "code_status"), f$coded$code_status)
      expect_equal(nrow(out), nrow(f$design))
    }
  }
})

test_that("page order takes precedence over natural variable names", {
  f <- unit_order_fixture(c("NR", "A", "NR"))
  f$units$unit_codes[[1]]$variable_page <- c(2L, 3L, 1L)
  out <- unit_order_complete(f)
  expect_equal(unit_order_values(out, f), c(-99, 1, -99))
  expect_equal(out$variable_id, c("V11", "V2", "V10"))
  expect_identical(out$variable_position, 1:3)
  f$units$unit_codes[[1]]$variable_page <- c("2", "10", "1")
  out <- unit_order_complete(f)
  expect_equal(unit_order_values(out, f), c(-99, 1, -99))
  expect_equal(out$variable_id, c("V11", "V2", "V10"))
})

test_that("missing pages follow known pages and use natural name order", {
  f <- unit_order_fixture(c("NR", "A", "NR"))
  f$units$unit_codes[[1]]$variable_section <- c(3L, 2L, 1L)
  f$units$unit_codes[[1]]$variable_page <- c(NA_integer_, 1L, NA_integer_)
  expect_equal(unit_order_values(unit_order_complete(f), f), c(-96, 1, -96))
  f$units$unit_codes[[1]]$variable_page <- c(1L, NA_integer_, NA_integer_)
  expect_equal(unit_order_values(unit_order_complete(f), f), c(-99, 1, -96))
  f$units$unit_codes[[1]]$variable_page <- rep(NA_integer_, 3)
  expect_equal(unit_order_values(unit_order_complete(f), f), c(-99, 1, -96))
  f$units$unit_codes[[1]]$variable_page <- NULL
  expect_equal(unit_order_values(unit_order_complete(f), f), c(-99, 1, -96))
})

test_that("sections precede names but elements and derivation level do not", {
  f <- unit_order_fixture(c("NR", "A", "NR"))
  f$units$unit_codes[[1]]$variable_section <- c("10", "2", NA_character_)
  f$units$unit_codes[[1]]$variable_element <- list(c(2L, 1L), c(1L, 2L), c(1L, 10L))
  f$units$unit_codes[[1]]$variable_level <- c(0L, 2L, 1L)
  f$units$unit_codes[[1]]$variable_page_always_visible <- TRUE
  out <- unit_order_complete(f)
  expect_equal(unit_order_values(out, f), c(-96, 1, -96))
  expect_equal(out$variable_id, c("V10", "V2", "V11"))

  f$units$unit_codes[[1]]$variable_section <- rep(NA_integer_, 3)
  out <- unit_order_complete(f)
  expect_equal(unit_order_values(out, f), c(-99, 1, -96))
  expect_equal(out$variable_id, f$design$variable_id)
})

test_that("classification is invariant to response, design, and metadata row order", {
  f <- unit_order_fixture(c("NR", "A", "O"))
  shuffled <- f
  shuffled$design <- f$design[c(3, 1, 2), ]
  shuffled$coded <- f$coded[c(2, 3, 1), ]
  shuffled$units$unit_codes[[1]] <- f$units$unit_codes[[1]][c(3, 2, 1), ]
  for (recode in c(FALSE, TRUE)) {
    expect_equal(unit_order_fields(unit_order_complete(shuffled, recode)),
                 unit_order_fields(unit_order_complete(f, recode)))
  }
})

test_that("boundaries respect people, testlets, and repeated occurrences of a unit", {
  f <- unit_order_fixture()
  occurrences <- tibble::tibble(
    unit_alias = c("first", "second", "third"),
    testlet_no = c(1L, 1L, 2L), unit_booklet_no = c(1L, 2L, 1L)
  )
  f$design <- tidyr::crossing(login_code = c("P1", "P2"), occurrences,
                             variable_id = f$design$variable_id) %>%
    dplyr::mutate(booklet_id = "B1", booklet_no = 1L, unit_key = "U1")
  types <- rep("O", nrow(f$design))
  answered <- f$design$login_code == "P1" &
    ((f$design$unit_alias == "second" & f$design$variable_id == "V2") |
       (f$design$unit_alias == "third" & f$design$variable_id == "V10"))
  types[answered] <- "A"
  f$coded <- unit_order_coded(f$design, types)
  out <- unit_order_fields(unit_order_complete(f, TRUE))
  expect_equal(nrow(out), nrow(f$design))
  expect_identical(out$variable_position, match(out$variable_id, c("V2", "V10", "V11")))
  expect_true(all(out$code_id[out$login_code == "P2"] == -96))
  expect_true(all(out$code_id[out$login_code == "P1" & out$unit_alias == "first"] == -99))
  p1 <- out[out$login_code == "P1", ]
  expect_equal(p1$code_id[p1$unit_alias == "second" & p1$variable_id == "V2"], 1)
  expect_true(all(p1$code_id[p1$unit_alias == "second" & p1$variable_id != "V2"] == -96))
  third <- p1[p1$unit_alias == "third", ]
  expect_equal(third$code_id[match(c("V2", "V10", "V11"), third$variable_id)], c(-99, 1, -96))
})

test_that("derived valid and invalid results retain the historical treatment", {
  f <- unit_order_fixture(c("NR", "A", "I"))
  f$units$unit_codes[[1]]$variable_source_type <- c("BASE", "SUM", "SUM")
  f$units$unit_codes[[1]]$variable_level <- c(0L, 1L, 1L)
  f$units$unit_codes[[1]]$variable_page <- 1:3
  f$units$unit_codes[[1]]$variable_sources <- list(
    tibble::tibble(), tibble::tibble(variable_source_id = "V2"),
    tibble::tibble(variable_source_id = "V2")
  )
  for (recode in c(FALSE, TRUE)) {
    out <- unit_order_complete(f, recode)
    expect_equal(unit_order_values(out, f), c(-99, 1, -98))
    expect_equal(unit_order_values(out, f, "code_type")[-1], f$coded$code_type[-1])
    expect_equal(unit_order_values(out, f, "code_score")[-1], f$coded$code_score[-1])
    expect_equal(unit_order_values(out, f, "code_status"), f$coded$code_status)
  }
})

test_that("missing schemas change analytical fields without touching technical status", {
  f <- unit_order_fixture()
  f$coded$code_status[[1]] <- "PARTLY_DISPLAYED"
  missings <- tibble::tribble(
    ~code_id, ~code_status, ~code_score, ~code_type,
    -196, "CUSTOM_NR", 0.5, "MISSING_NOT_REACHED",
    -197, "CUSTOM_ERROR", NA_real_, "MISSING_CODING_IMPOSSIBLE",
    -198, "CUSTOM_INVALID", 0, "MISSING_INVALID_RESPONSE",
    -199, "CUSTOM_OMISSION", 0.25, "MISSING_BY_OMISSION"
  )
  out <- unit_order_complete(f, TRUE, missings = missings)
  expect_equal(unit_order_values(out, f), c(-199, 1, -196))
  expect_equal(unit_order_values(out, f, "code_score"), c(0.25, 1, 0.5))
  expect_equal(unit_order_values(out, f, "code_status"), f$coded$code_status)

  f$coded <- f$coded[2L, ]
  out <- unit_order_complete(f, missings = missings)
  expect_equal(unit_order_values(out, f), c(-199, 1, -196))
  expect_equal(unit_order_values(out, f, "code_status"),
               c(NA_character_, "CODING_COMPLETE", NA_character_))
  expect_true(all(out$id_used))

  f$coded$code_status <- NA_character_
  out <- unit_order_complete(f, missings = missings)
  expect_true(all(is.na(out$code_status)))
  expect_false(any(out$id_used))
  expect_equal(unit_order_values(out, f), c(-199, 1, -196))

  f$coded <- f$coded[FALSE, ]
  out <- unit_order_complete(f)
  expect_equal(unit_order_values(out, f), rep(-96, 3))
  expect_true(all(is.na(out$code_status)))
  expect_true(all(is.na(out$code_score)))
  expect_false(any(out$id_used))
})

test_that("existing unit-tail cases keep their version 0.9.8.9036 results", {
  # Expected values verified by executing the historical fc7debb function.
  cases <- list(
    list(types = c("A", "O", "O"), no = c(1, -99, -99), yes = c(1, -96, -96)),
    list(types = c("A", "NR", "NR"), no = c(1, -96, -96), yes = c(1, -96, -96)),
    list(types = c("O", "O", "O"), no = c(-99, -99, -99), yes = c(-96, -96, -96)),
    list(types = c("A", "I", "E"), no = c(1, -98, -97), yes = c(1, -98, -97))
  )
  for (case in cases) {
    f <- unit_order_fixture(case$types)
    expect_equal(unit_order_values(unit_order_complete(f), f), case$no)
    expect_equal(unit_order_values(unit_order_complete(f, TRUE), f), case$yes)
  }
})

test_that("the historical variable design is completed without expanding its universe", {
  f <- unit_order_fixture()
  f$design <- f$design[2L, ]
  f$coded <- f$coded[FALSE, ]
  out <- unit_order_complete(f)
  expect_equal(out$variable_id, "V10")
  expect_identical(out$variable_position, 2L)
  expect_equal(out$code_id, -96)
  expect_true(is.na(out$code_status))
  expect_false(out$id_used)

  f$coded <- unit_order_coded(f$design, "A")
  f$coded$booklet_id <- "b1"
  out <- unit_order_complete(f)
  expect_equal(out$code_id, 1)
  expect_true(out$id_used)
})

test_that("variable positions are recalculated instead of using input columns", {
  original <- unit_order_fixture(c("NR", "A", "NR"))
  expected <- unit_order_complete(original)
  for (input in list("coded", "design", c("coded", "design"))) {
    f <- original
    for (name in input) f[[name]]$variable_position <- c(99L, 98L, 97L)
    f$units$unit_codes[[1]]$variable_position <- c(30L, 20L, 10L)
    expect_identical(unit_order_complete(f), expected)
  }
  expect_false(".completion_position" %in% names(expected))
})

test_that("variables absent from unit metadata have an unknown position", {
  f <- unit_order_fixture(c("A", "NR", "NR"))
  f$units$unit_codes[[1]] <- f$units$unit_codes[[1]][1:2, ]
  # V11 is still mapped to an item, so missing metadata requires the unit fallback.
  out <- unit_order_complete(f, expect_fallback = TRUE)
  expect_identical(out$variable_id, c("V2", "V10", "V11"))
  expect_identical(out$variable_position, c(1L, 2L, NA_integer_))
  expect_equal(out$code_id, c(1, -96, -96))
  expect_identical(out$code_status, f$coded$code_status)
})
