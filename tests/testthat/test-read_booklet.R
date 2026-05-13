test_that("read_booklet handles mixed text nodes in metadata", {
  booklet_xml <- xml2::read_xml(
    "<Booklet>
      <Metadata>
        <Id>BOOKLET_1</Id>metadata text<Label>Booklet 1</Label>
        <Description>Plain text<p>nested text</p></Description>
      </Metadata>
      <Units>
        <Unit id='UNIT_1' label='Unit 1' />
      </Units>
    </Booklet>"
  )

  booklet <- read_booklet(booklet_xml)

  expect_equal(booklet$booklet_id, "BOOKLET_1")
  expect_equal(booklet$booklet_label, "Booklet 1")
  expect_equal(booklet$unit_key, "UNIT_1")
  expect_equal(booklet$unit_label, "Unit 1")
})
