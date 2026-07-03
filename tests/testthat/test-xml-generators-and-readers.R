test_that("generate_booklet and read_booklet round-trip units and testlets", {
  units <- tibble::tibble(
    id = c("U1", "U2"),
    label = c("Unit 1", "Unit 2"),
    alias = c("UA1", "UA2")
  )

  xml <- generate_booklet(
    booklet_id = "B1",
    booklet_label = "Booklet 1",
    booklet_configuration = list(loading_mode = "eager"),
    units = units
  )

  expect_s3_class(xml, "xml_document")
  parsed <- read_booklet(xml)

  expect_equal(unique(parsed$booklet_id), "B1")
  expect_equal(parsed$unit_key, c("U1", "U2"))
  expect_equal(parsed$unit_alias, c("UA1", "UA2"))
  expect_error(read_booklet("<Booklet></Booklet>"), "xml_document")
})

test_that("generate_booklet accepts ordinary data frames", {
  units <- data.frame(
    id = c("U1", "U2"),
    label = c("Unit 1", "Unit 2"),
    alias = c("UA1", "UA2")
  )

  xml <- generate_booklet(
    booklet_id = "B1",
    booklet_label = "Booklet 1",
    units = units
  )

  expect_s3_class(xml, "xml_document")
})

test_that("generate_booklets creates XML for nested booklet specifications", {
  booklets <- tibble::tibble(
    booklet_id = "B1",
    booklet_label = "Booklet 1",
    booklet_units = list(
      tibble::tibble(
        testlet_id = c(NA_character_, "T1"),
        testlet_label = c(NA_character_, "Testlet 1"),
        units = list(
          tibble::tibble(unit_key = "U1", unit_label = "Unit 1"),
          tibble::tibble(unit_key = "U2", unit_label = "Unit 2", unit_alias = "Alias 2")
        )
      )
    )
  )

  out <- generate_booklets(booklets)

  expect_true(tibble::has_name(out, "booklet_xml"))
  expect_s3_class(out$booklet_xml[[1]], "xml_document")
  expect_match(as.character(out$booklet_xml[[1]]), "<Testlet")
  expect_match(as.character(out$booklet_xml[[1]]), "Alias 2")
})

test_that("generate_booklets creates XML for recursively nested testlets", {
  inner_testlets <- tibble::tibble(
    testlet_id = "Inner",
    testlet_label = "Inner testlet",
    units = list(tibble::tibble(unit_key = "U1", unit_label = "Unit 1"))
  )

  booklets <- tibble::tibble(
    booklet_id = "B1",
    booklet_label = "Booklet 1",
    booklet_units = list(
      tibble::tibble(
        testlet_id = "Outer",
        testlet_label = "Outer testlet",
        testlet_restrictions = list(list(minutes = 7, leave = "allowed")),
        testlets = list(inner_testlets)
      )
    )
  )

  xml <- generate_booklets(booklets)$booklet_xml[[1]]

  expect_length(xml2::xml_find_all(xml, ".//Units/Testlet[@id='Outer']"), 1)
  expect_length(xml2::xml_find_all(xml, ".//Testlet[@id='Outer']/Testlet[@id='Inner']"), 1)
  expect_length(xml2::xml_find_all(xml, ".//Testlet[@id='Inner']/Unit[@id='U1']"), 1)
  expect_length(xml2::xml_find_all(xml, ".//Testlet[@id='Outer']/Restrictions/TimeMax[@minutes='7']"), 1)
  expect_length(xml2::xml_find_all(xml, ".//Testlet[@id='Inner']/Restrictions/TimeMax"), 0)
})

test_that("generate_booklets rejects nested time restrictions", {
  inner_testlets <- tibble::tibble(
    testlet_id = "Inner",
    testlet_label = "Inner testlet",
    testlet_restrictions = list(list(minutes = 3, leave = "allowed")),
    units = list(tibble::tibble(unit_key = "U1", unit_label = "Unit 1"))
  )

  booklets <- tibble::tibble(
    booklet_id = "B1",
    booklet_label = "Booklet 1",
    booklet_units = list(
      tibble::tibble(
        testlet_id = "Outer",
        testlet_label = "Outer testlet",
        testlets = list(inner_testlets)
      )
    )
  )

  expect_error(
    generate_booklets(booklets),
    "TimeMax restrictions are only supported for top-level testlets"
  )
})

test_that("generate_testtakers writes groups, logins, booklets, custom texts, and profiles", {
  testtakers <- tibble::tibble(
    group_id = "G1",
    group_label = "Group 1",
    login_name = c("login1", "monitor"),
    login_pw = c("pw1", NA_character_),
    login_mode = c("run-hot-return", "monitor-group"),
    booklet_id = c("B1", NA_character_),
    booklet_codes = c("C1 C2", NA_character_),
    profile_id = c(NA_character_, "P1")
  )

  profiles <- tibble::tibble(
    profile_id = "P1",
    profile_label = "Profile 1",
    block_column = "show",
    unit_column = "hide",
    view = "full",
    group_column = "hide",
    booklet_column = "hide",
    filter_pending = "no",
    filter_locked = "no",
    autoselect_next_block = "no",
    filter_label = "Booklet filter",
    filter_field = "bookletLabel",
    filter_type = "equal",
    filter_value = "Booklet 1",
    filter_not = "false"
  )

  xml <- generate_testtakers(
    testtakers,
    custom_texts = list(AppTitle = "Pilot"),
    profiles = profiles
  )

  xml_text <- as.character(xml)
  expect_s3_class(xml, "xml_document")
  expect_match(xml_text, "<Group")
  expect_match(xml_text, "<Login")
  expect_match(xml_text, "<Booklet")
  expect_match(xml_text, "<CustomText")
  expect_match(xml_text, "<Profile")
})

test_that("read_testtakers expands booklet codes and preserves order", {
  testtakers_xml <- xml2::read_xml(
    "<Testtakers>
      <Group id='G1' label='Group 1'>
        <Login name='L1' pw='pw' mode='run-hot-return'>
          <Booklet codes='C1 C2'>B1</Booklet>
          <Booklet codes='C3'>B2</Booklet>
        </Login>
      </Group>
    </Testtakers>"
  )

  out <- read_testtakers(testtakers_xml)

  expect_equal(out$group_id, rep("G1", 3))
  expect_equal(out$login_code, c("C1", "C2", "C3"))
  expect_equal(out$booklet_no, c(1L, 1L, 1L))
  expect_error(read_testtakers("<Testtakers></Testtakers>"), "xml_document")
})

test_that("configuration and restriction helpers build compact XML-ready lists", {
  config <- eatPrepTBA:::configure_booklet(loading_mode = "eager", log_policy = "lean")
  restriction <- eatPrepTBA:::restrict_testlet(code = "READY", minutes = 15)

  expect_named(config[[1]], c("", "key"))
  expect_equal(config[[1]]$key, "loading_mode")
  expect_equal(config[[1]][[1]][[1]], "EAGER")
  expect_equal(restriction$Restrictions$CodeToEnter$code, "READY")
  expect_equal(restriction$Restrictions$TimeMax$minutes, 15)
})

test_that("generate_booklets emits current booklet config by default", {
  booklets <- tibble::tibble(
    booklet_id = "B1",
    booklet_label = "Booklet 1",
    booklet_units = list(
      tibble::tibble(unit_key = "U1", unit_label = "Unit 1")
    )
  )

  xml <- generate_booklets(booklets)$booklet_xml[[1]]
  config <- xml2::xml_find_all(xml, ".//BookletConfig/Config")
  keys <- xml2::xml_attr(config, "key")

  expect_match(
    as.character(xml),
    "https://w3id.org/iqb/spec/testcenter-booklet-xml/18.0",
    fixed = TRUE
  )
  expect_true("header_content" %in% keys)
  expect_true("navbar_unit_label" %in% keys)
  expect_true("toolbar_show_unit_title" %in% keys)
  expect_true("force_response_complete" %in% keys)
  expect_false("force_responses_complete" %in% keys)
  expect_false("controller_design" %in% keys)
})

test_that("generate_booklets can emit legacy-16 booklet config", {
  booklets <- tibble::tibble(
    booklet_id = "B1",
    booklet_label = "Booklet 1",
    booklet_units = list(
      tibble::tibble(unit_key = "U1", unit_label = "Unit 1")
    )
  )

  xml <- generate_booklets(booklets, booklet_config_version = "legacy-16")$booklet_xml[[1]]
  config <- xml2::xml_find_all(xml, ".//BookletConfig/Config")
  keys <- xml2::xml_attr(config, "key")
  values <- stats::setNames(xml2::xml_text(config), keys)

  expect_match(
    as.character(xml),
    "https://raw.githubusercontent.com/iqb-berlin/testcenter/16.0.2/definitions/vo_Booklet.xsd",
    fixed = TRUE
  )
  expect_equal(values[["controller_design"]], "2018")
  expect_equal(values[["page_navibuttons"]], "OFF")
  expect_equal(values[["unit_navibuttons"]], "FULL")
  expect_equal(values[["ask_for_fullscreen"]], "ON")
  expect_false("header_content" %in% keys)
  expect_false("force_response_complete" %in% keys)
})

test_that("configure_booklet maps deprecated arguments in current mode", {
  config <- NULL
  expect_warning(
    config <- eatPrepTBA:::configure_booklet(unit_navibuttons = "full"),
    "deprecated"
  )
  keys <- purrr::map_chr(config, "key")
  values <- stats::setNames(
    purrr::map_chr(config, function(x) x[[1]][[1]]),
    keys
  )

  expect_equal(values[["navbar_unit_label"]], "INDEX")
  expect_equal(values[["navbar_unit_controls_hidden"]], "FALSE")
  expect_false("unit_navibuttons" %in% keys)
})

test_that("list_to_xml converts unnamed children to XML nodes and named values to attributes", {
  node <- eatPrepTBA:::list_to_xml(
    list(Root = list(list(Child = list(list("value"), id = "C1")), version = "1"))
  )

  xml <- xml2::as_xml_document(node)

  expect_match(as.character(xml), "<Root version=\"1\">")
  expect_match(as.character(xml), "<Child id=\"C1\">value</Child>")
})
