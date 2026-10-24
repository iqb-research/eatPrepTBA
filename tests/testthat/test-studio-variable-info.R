studio_variable_info_unit <- function(version = "v2", id = 10L) {
  aliases <- c("V10", "V2", "V1")
  refs <- c("v10", "v2", "v1")
  variables <- lapply(seq_along(refs), function(i) {
    variable <- list(id = refs[i], alias = aliases[i], multiple = FALSE)
    if (version == "v1") {
      variable$type <- "string"
      variable$format <- "text-selection"
      variable$page <- 4L - i
      variable$values <- list()
    } else {
      variable$type <- "STRING"
      # Optional format and values are deliberately absent on other variables.
      if (i == 3L) variable$format <- "TEXT_SELECTION"
    }
    variable
  })
  codings <- lapply(seq_along(refs), function(i) {
    list(id = refs[i], alias = aliases[i], sourceType = "BASE",
         sourceParameters = list(processing = list(), solverExpression = NULL),
         deriveSources = list(), processing = list(), codeModel = "basic",
         codes = list(list(id = 1L, type = "FULL_CREDIT", score = 1,
                           ruleSetOperatorAnd = TRUE, ruleSets = list())))
  })
  derived <- codings[[2]]
  derived$id <- "derived"
  derived$alias <- "D2"
  derived$sourceType <- "COPY_VALUE"
  derived$deriveSources <- list("v2")
  codings[[4]] <- derived
  items <- lapply(c(1L, 3L, 4L), function(i) {
    item <- list(id = paste0("I", i))
    if (version == "v1") {
      item$variableId <- codings[[i]]$alias
      item$variableReadOnlyId <- codings[[i]]$id
    } else {
      item$sourceVariableId <- codings[[i]]$alias
      item$sourceVariableUuid <- codings[[i]]$id
    }
    item
  })
  list(id = id, key = paste0("U", id), name = paste("Unit", id),
       groupName = "Group A", state = 1L, variables = variables,
       metadata = list(items = items),
       scheme = as.character(jsonlite::toJSON(list(variableCodings = codings),
                                              auto_unbox = TRUE, null = "null")),
       lastChangedDefinition = "2026-10-17T00:00:00Z",
       lastChangedScheme = "2026-10-17T00:00:00Z",
       lastChangedMetadata = "2026-10-17T00:00:00Z")
}

studio_variable_info_get_units <- function(versions, unit_definition, legacy_pages = NULL,
                                         layout = c("cloze", "direct", "mixed")) {
  layout <- match.arg(layout)
  payload <- Map(studio_variable_info_unit, versions, seq_along(versions) + 9L)
  if (!is.null(legacy_pages)) {
    for (i in which(versions == "v1")) {
      for (j in seq_along(payload[[i]]$variables)) {
        payload[[i]]$variables[[j]]$page <- legacy_pages[j]
      }
    }
  }
  # Real definition extraction orders V10 before V2 before V1, whereas the
  # legacy page fields and the natural-name fallback give the reverse order.
  pages <- lapply(c("v10", "v2", "v1"), function(ref) {
    element <- list(id = ref, type = "text-field")
    if (layout == "cloze" || (layout == "mixed" && ref == "v2")) {
      element <- list(type = "cloze", content = list(element))
    }
    list(alwaysVisible = FALSE, sections = list(list(elements = list(element))))
  })
  definition <- as.character(jsonlite::toJSON(list(pages = pages), auto_unbox = TRUE))
  testthat::local_mocked_bindings(
    get_settings = function(workspace, metadata = TRUE) {
      tibble::tibble(
        ws_id = 1L, ws_label = "Workspace 1", wsg_id = 10L,
        wsg_label = "Workspace Group",
        states = list(tibble::tibble(state_id = 1L, state_label = "Open", state_color = "#fff"))
      )
    },
    .package = "eatPrepTBA"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) req,
    resp_body_json = function(resp, ...) {
      endpoint <- tail(resp$endpoint, 1L)
      if (endpoint == "properties") return(payload)
      if (endpoint == "definition") return(list(definition = definition))
      stop("Unexpected Studio endpoint")
    },
    .package = "httr2"
  )
  suppressMessages(get_units(fake_studio_workspace(), unit_definition = unit_definition))
}

studio_variable_info_complete <- function(units, nr_origin = TRUE) {
  keys <- units$unit_key
  design <- tidyr::expand_grid(unit_key = keys, variable_id = c("V10", "V2", "V1", "D2"))
  design <- dplyr::mutate(design, login_code = "P", booklet_id = "B",
                         booklet_no = 1L, testlet_no = 1L,
                         unit_booklet_no = match(unit_key, keys), unit_alias = unit_key)
  coded <- dplyr::mutate(
    dplyr::select(design, login_code, booklet_id, unit_key, unit_alias, variable_id),
    code_type = rep(c("MISSING_BY_OMISSION", "FULL_CREDIT", "MISSING_NOT_REACHED", "NO_CODING"), length(keys)),
    code_status = rep(c("DISPLAYED", "CODING_COMPLETE", "NOT_REACHED", "NO_CODING"), length(keys)),
    code_id = rep(c(-99, 1, -96, -93), length(keys)),
    code_score = rep(c(0, 1, NA_real_, NA_real_), length(keys)),
    value = ifelse(variable_id == "V2", "answer", NA_character_),
    coding_from_not_reached = variable_id == "V2" & nr_origin
  )
  suppressMessages(complete_design(
    coded, units, design, identifiers = "login_code",
    recode_omissions_to_not_reached = TRUE, preserve_not_reached = TRUE
  ))
}

test_that("Studio variable enums normalize known v2 spellings and retain extensions", {
  types <- c("STRING", "INTEGER", "NUMBER", "BOOLEAN", "ATTACHMENT", "JSON", "NO_VALUE", "CODED",
             "string", "no-value", "CUSTOM_TYPE", "String")
  expected_types <- c("string", "integer", "number", "boolean", "attachment", "json", "no-value", "coded",
                      "string", "no-value", "CUSTOM_TYPE", "String")
  formats <- c("TEXT_SELECTION", "IMAGE", "CAPTURE_IMAGE", "AUDIO", "GGB_FILE", "NON_NEGATIVE",
               "LATEX", "MATH_ML", "MATH_TABLE", "MATH_TEXT_MIX", "GGB_VARIABLE",
               "text-selection", "CHEM_MOLECULE", "Text_Selection", "")
  expected_formats <- c("text-selection", "image", "capture-image", "audio", "ggb-file", "non-negative",
                        "latex", "math-ml", "math-table", "math-text-mix", "ggb-variable",
                        "text-selection", "CHEM_MOLECULE", "Text_Selection", "")
  unit <- studio_variable_info_unit()
  unit$variables <- lapply(seq_along(formats), function(i) {
    list(id = paste0("v", i), type = rep(types, length.out = length(formats))[i], format = formats[i])
  })
  out <- eatPrepTBA:::read_unit(unit)$variables[[1]]
  expect_identical(out$variable_type, rep(expected_types, length.out = length(formats)))
  expect_identical(out$variable_format, expected_formats)
  expect_identical(out$variable_id, out$variable_ref)
  expect_true(all(is.na(out$variable_page)))
  expect_true(all(vapply(out$variable_values, function(x) {
    identical(names(x), c("value", "value_label")) && all(is.na(x))
  }, logical(1))))
})

test_that("optional v2 fields coexist with legacy values within a unit", {
  unit <- studio_variable_info_unit()
  unit$variables[[1]]$page <- 3L
  unit$variables[[1]]$values <- list(list(value = "A", label = "Alpha"))
  unit$variables[[2]]$type <- "no-value"
  unit$variables[[2]]$alias <- NULL
  unit$variables[[3]]$type <- "NO_VALUE"
  unit$variables[[3]]$format <- NULL
  out <- eatPrepTBA:::read_unit(unit)$variables[[1]]
  expect_identical(out$variable_type, c("string", "no-value", "no-value"))
  expect_identical(out$variable_format, rep("", 3L))
  expect_identical(out$variable_id, c("V10", "v2", "V1"))
  expect_equal(out$variable_page, c(3, NA, NA))
  expect_equal(out$variable_values[[1]], tibble::tibble(value = "A", value_label = "Alpha"))
})

for (versions in list("v1", "v2", c("v1", "v2"))) {
  for (definition in c(FALSE, TRUE)) {
    test_that(paste("Studio preparation and completion support", paste(versions, collapse = "+"),
                    "with unit_definition", definition), {
      units <- studio_variable_info_get_units(versions, definition)
      expect_s3_class(units, "tba_units")
      expect_equal(length(units$unit_variables), length(versions))
      expect_no_warning(prepared <- suppressMessages(add_coding_scheme(units)))
      expect_no_warning(out <- studio_variable_info_complete(prepared))
      expected_order <- if (definition) c("V10", "V2", "D2", "V1") else c("V1", "V2", "D2", "V10")
      expect_equal(out$variable_id, rep(expected_order, length(versions)))
      expect_identical(out$variable_position, rep(1:4, length(versions)))
      expect_identical(out$coding_from_not_reached, out$variable_id == "V2")
      expect_equal(out$code_id[out$variable_id == "V2"], rep(1, length(versions)))
      expect_equal(out$code_status[out$variable_id == "V2"], rep("CODING_COMPLETE", length(versions)))
      for (i in seq_along(versions)) {
        codes <- prepared$unit_codes[[i]]
        base_rows <- match(c("V10", "V2", "V1"), codes$variable_id)
        expect_equal(codes$variable_type[base_rows], rep("string", 3L))
        expect_equal(units$items_list[[i]]$variable_id, c("V10", "V1", "D2"))
        if (definition) {
          expect_equal(codes$variable_page[base_rows], 0:2)
          expect_equal(codes$variable_page[codes$variable_id == "D2"], 1)
        } else if (versions[i] == "v2") {
          expect_true(all(is.na(codes$variable_page)))
        } else {
          expect_equal(codes$variable_page[base_rows], 3:1)
        }
      }
    })
  }
}

test_that("v2 preparation keeps NR provenance from establishing a later-work boundary", {
  units <- suppressMessages(add_coding_scheme(studio_variable_info_get_units("v2", TRUE)))
  from_nr <- studio_variable_info_complete(units, nr_origin = TRUE)
  genuine <- studio_variable_info_complete(units, nr_origin = FALSE)
  expect_equal(from_nr$code_id[from_nr$variable_id == "V10"], -96)
  expect_equal(genuine$code_id[genuine$variable_id == "V10"], -99)
})

test_that("legacy character pages combine with missing v2 pages", {
  for (legacy_pages in list(rep("", 3L), as.character(3:1))) {
    for (definition in c(FALSE, TRUE)) {
      units <- studio_variable_info_get_units(c("v1", "v2"), definition, legacy_pages)
      expect_identical(units$unit_variables[[1]]$variable_page, legacy_pages)
      expect_no_warning(prepared <- suppressMessages(add_coding_scheme(units)))
      expect_no_warning(out <- studio_variable_info_complete(prepared))
      expected_order <- if (definition) c("V10", "V2", "D2", "V1") else c("V1", "V2", "D2", "V10")
      expect_identical(out$variable_id, rep(expected_order, 2L))
      if (definition) {
        expect_equal(prepared$unit_codes[[1]]$variable_page[
          match(c("V10", "V2", "V1"), prepared$unit_codes[[1]]$variable_id)
        ], 0:2)
      }
    }
  }
})

test_that("previously saved v2 units can omit the legacy page column", {
  units <- studio_variable_info_get_units("v2", TRUE)
  units$unit_variables[[1]]$variable_page <- NULL
  expect_no_warning(prepared <- suppressMessages(add_coding_scheme(units)))
  expect_equal(prepared$unit_codes[[1]]$variable_page[
    match(c("V10", "V2", "V1"), prepared$unit_codes[[1]]$variable_id)
  ], 0:2)
})

for (versions in list("v1", "v2", c("v1", "v2"))) {
  for (layout in c("direct", "mixed")) {
    test_that(paste("Studio", paste(versions, collapse = "+"),
                    "preserves pages and completion for", layout, "elements"), {
      units <- studio_variable_info_get_units(versions, TRUE, layout = layout)
      for (pages in units$variable_pages) {
        # Direct elements have no content path; mixed layouts have it only
        # for the nested field. Exercise the real extractor in both cases.
        expect_identical("variable_content" %in% names(pages), layout == "mixed")
        if (layout == "mixed") {
          expect_identical(is.na(pages$variable_content), pages$variable_ref != "v2")
        }
      }
      expect_no_warning(prepared <- suppressMessages(add_coding_scheme(units)))
      for (codes in prepared$unit_codes) {
        expect_equal(codes$variable_page[match(c("V10", "V2", "V1", "D2"), codes$variable_id)],
                     c(0, 1, 2, 1))
        source <- codes$variable_sources[[match("D2", codes$variable_id)]]
        expect_identical(source$variable_source_ref, "v2")
        expect_equal(source$variable_source_page, 1)
        expect_equal(source$variable_source_section, 0)
        expect_equal(source$variable_source_element, 0)
        if (layout == "mixed") expect_equal(source$variable_source_content, 0)
      }
      for (nr_origin in c(FALSE, TRUE)) {
        expect_no_warning(out <- studio_variable_info_complete(prepared, nr_origin = nr_origin))
        expect_identical(out$variable_id, rep(c("V10", "V2", "D2", "V1"), length(versions)))
        expect_identical(out$variable_position, rep(1:4, length(versions)))
        expect_equal(out$code_id[out$variable_id == "V10"],
                     rep(if (nr_origin) -96 else -99, length(versions)))
        expect_equal(out$code_id[out$variable_id == "V2"], rep(1, length(versions)))
      }
    })
  }
}
