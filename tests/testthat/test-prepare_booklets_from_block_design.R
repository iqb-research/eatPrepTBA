test_that("prepare_booklets_from_block_design builds generate_booklets input", {
  booklets <- tibble::tibble(
    booklet_id = "BK1",
    booklet_label = "Booklet 1",
    block_1 = "A",
    block_2 = "B"
  )

  blocks <- tibble::tibble(
    subject = "M",
    domain = "D",
    block = c("A", "B"),
    minutes = c(5, NA)
  )

  units <- tibble::tibble(
    subject = "M",
    domain = "D",
    block = c("A", "A", "B"),
    unit_key = c("U1", "U2", "U3"),
    unit_label = c("Unit 1", "Unit 2", "Unit 3"),
    sequence = c(2, 1, 1)
  )

  restrictions <- tibble::tibble(
    subject = "M",
    domain = "D",
    block = "A",
    minutes = 4,
    leave = "allowed"
  )

  out <- prepare_booklets_from_block_design(
    booklets = booklets,
    blocks = blocks,
    units = units,
    restrictions = restrictions,
    add_start_end = FALSE
  )

  expect_named(out, c("booklet_id", "booklet_label", "booklet_units"))
  expect_equal(nrow(out$booklet_units[[1]]), 2)
  expect_equal(out$booklet_units[[1]]$testlet_id, c("A", "B"))
  expect_equal(out$booklet_units[[1]]$units[[1]]$unit_key, c("U2", "U1"))
  expect_equal(out$booklet_units[[1]]$testlet_restrictions[[1]]$minutes, 5)
  expect_equal(out$booklet_units[[1]]$testlet_restrictions[[1]]$leave, "allowed")

  xml <- generate_booklets(out)$booklet_xml[[1]]

  expect_length(xml2::xml_find_all(xml, ".//Testlet[@id='A']/Restrictions/TimeMax[@minutes='5']"), 1)
  expect_length(xml2::xml_find_all(xml, ".//Testlet[@id='A']/Unit[@id='U2']"), 1)
})

test_that("prepare_booklets_from_block_design can add start and end units", {
  out <- prepare_booklets_from_block_design(
    booklets = tibble::tibble(booklet = "BK1", block_1 = "A"),
    blocks = tibble::tibble(block = "A"),
    units = tibble::tibble(block = "A", unit_key = "U1"),
    add_start_end = TRUE,
    wrap_blocks = FALSE
  )

  xml <- generate_booklets(out)$booklet_xml[[1]]

  expect_length(xml2::xml_find_all(xml, ".//Units/Unit[@id='Start_page']"), 1)
  expect_length(xml2::xml_find_all(xml, ".//Units/Unit[@id='End_page']"), 1)
})

test_that("prepare_booklets_from_block_design handles compact times sheets", {
  booklets <- tibble::tibble(
    booklet_id = "TH_M1_01",
    booklet_label = "Booklet 1",
    block_1 = "A",
    block_2 = "B",
    block_3 = "C"
  )

  blocks <- tibble::tibble(
    subject = "M",
    domain = "D",
    block = c("A", "B", "C")
  )

  units <- tibble::tibble(
    subject = "M",
    domain = "D",
    block = c("A", "B", "C"),
    unit_key = c("U1", "U2", "U3"),
    sequence = 1
  )

  times <- tibble::tibble(
    design = c("M1", "M1"),
    block_group = c("block_1", "block_1"),
    block = c("block_1", "block_2"),
    seconds = c(90, 90)
  )

  out <- prepare_booklets_from_block_design(
    booklets = booklets,
    blocks = blocks,
    units = units,
    restrictions = times,
    add_start_end = FALSE
  )

  expect_equal(nrow(out$booklet_units[[1]]), 2)
  expect_equal(out$booklet_units[[1]]$testlet_id[[1]], "M1_block_1")
  expect_equal(out$booklet_units[[1]]$testlet_label[[1]], "block_1 block_2")
  expect_equal(out$booklet_units[[1]]$units[[1]]$unit_key, c("U1", "U2"))
  expect_equal(out$booklet_units[[1]]$testlet_restrictions[[1]]$minutes, 1.5)
  expect_equal(out$booklet_units[[1]]$testlet_restrictions[[1]]$leave, "allowed")

  xml <- generate_booklets(out)$booklet_xml[[1]]

  expect_length(
    xml2::xml_find_all(
      xml,
      ".//Testlet[@id='M1_block_1']/Restrictions/TimeMax[@minutes='1.5'][@leave='allowed']"
    ),
    1
  )
  expect_length(xml2::xml_find_all(xml, ".//Testlet[@id='M1_block_1']/Unit"), 2)
})

test_that("prepare_booklets_from_block_design keeps compact times leave values", {
  out <- prepare_booklets_from_block_design(
    booklets = tibble::tibble(booklet_id = "TH_S1_01", block_1 = "A"),
    blocks = tibble::tibble(subject = "S", domain = "D", block = "A"),
    units = tibble::tibble(subject = "S", domain = "D", block = "A", unit_key = "U1"),
    restrictions = tibble::tibble(
      design = "S1",
      block_group = "block_1",
      block = "block_1",
      seconds = 120,
      leave = "confirm"
    ),
    add_start_end = FALSE
  )

  expect_equal(out$booklet_units[[1]]$testlet_restrictions[[1]]$minutes, 2)
  expect_equal(out$booklet_units[[1]]$testlet_restrictions[[1]]$leave, "confirm")
})

test_that("compact times match exact design components and default blank leave", {
  out <- prepare_booklets_from_block_design(
    booklets = tibble::tibble(
      booklet_id = c("TH_M1_01", "TH_M10_01"),
      block_1 = c("A", "B")
    ),
    blocks = tibble::tibble(block = c("A", "B")),
    units = tibble::tibble(block = c("A", "B"), unit_key = c("U1", "U2")),
    restrictions = tibble::tibble(
      design = "M1",
      block_group = "block_1",
      block = "block_1",
      seconds = 60,
      leave = ""
    ),
    add_start_end = FALSE
  )

  m1 <- out$booklet_units[[which(out$booklet_id == "TH_M1_01")]]
  m10 <- out$booklet_units[[which(out$booklet_id == "TH_M10_01")]]

  expect_equal(m1$testlet_restrictions[[1]]$minutes, 1)
  expect_equal(m1$testlet_restrictions[[1]]$leave, "allowed")
  expect_null(m10$testlet_restrictions[[1]]$minutes)
})

test_that("compact times reject incomplete grouped timing columns", {
  expect_error(
    prepare_booklets_from_block_design(
      booklets = tibble::tibble(booklet_id = "TH_M1_01", block_1 = "A"),
      blocks = tibble::tibble(block = "A"),
      units = tibble::tibble(block = "A", unit_key = "U1"),
      restrictions = tibble::tibble(
        design = "M1",
        blocks = "block_1",
        seconds = 60
      ),
      add_start_end = FALSE
    ),
    "must contain 'design', 'block_group', 'block', and 'seconds'"
  )
})
