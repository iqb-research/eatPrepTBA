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
