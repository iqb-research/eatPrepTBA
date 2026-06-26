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

test_that("read_booklet keeps testlet attributes for nested units", {
  booklet_xml <- xml2::read_xml(
    "<Booklet>
      <Metadata>
        <Id>BOOKLET_1</Id><Label>Booklet 1</Label>
      </Metadata>
      <Units>
        <Testlet id='TESTLET_1' label='Testlet 1'>
          <Unit id='UNIT_1' label='Unit 1' />
        </Testlet>
      </Units>
    </Booklet>"
  )

  booklet <- read_booklet(booklet_xml)

  expect_equal(booklet$testlet_id, "TESTLET_1")
  expect_equal(booklet$testlet_label, "Testlet 1")
  expect_equal(booklet$unit_key, "UNIT_1")
  expect_equal(booklet$unit_label, "Unit 1")
})

test_that("read_booklet handles pre-existing testlet attributes on units", {
  booklet_xml <- xml2::read_xml(
    "<Booklet>
      <Metadata>
        <Id>BOOKLET_1</Id><Label>Booklet 1</Label>
      </Metadata>
      <Units>
        <Unit id='UNIT_1' label='Unit 1' testlet_id='' testlet_label='' />
        <Testlet id='TESTLET_1' label='Testlet 1'>
          <Unit id='UNIT_2' label='Unit 2' testlet_id='' testlet_label='' />
        </Testlet>
      </Units>
    </Booklet>"
  )

  booklet <- read_booklet(booklet_xml)

  expect_equal(booklet$testlet_id, c(NA_character_, "TESTLET_1"))
  expect_equal(booklet$testlet_label, c(NA_character_, "Testlet 1"))
  expect_equal(booklet$unit_key, c("UNIT_1", "UNIT_2"))
  expect_equal(booklet$unit_label, c("Unit 1", "Unit 2"))
})
