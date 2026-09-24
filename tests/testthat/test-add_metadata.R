mock_mixed_metadata_units <- function() {
  testthat::local_mocked_bindings(
    get_metadata_profile = function(url) {
      tibble::tibble(
        profile_name = c("Klassenstufe", "Thema", "Titel"),
        profile_type = "text",
        multiple = c(url == "multi", url == "multi", FALSE),
        data = rep(list(tibble::tibble(
          value_id = NA_character_, value_label = NA_character_
        )), 3)
      )
    },
    .package = "eatPrepTBA", .env = parent.frame()
  )

  grades <- list("5", c("5", "6", "7", "8"), c("7", "8"))
  topics <- list("A", c("A", "B"), c("A", "B", "C", "D"))
  profile_ids <- c("single", "multi", "multi")
  unit_profiles <- lapply(seq_along(grades), function(i) {
    tibble::tibble(
      profile_id = profile_ids[[i]],
      profile_name = c(rep("Klassenstufe", length(grades[[i]])),
                       rep("Thema", length(topics[[i]])), "Titel"),
      value_id = NA_character_,
      value_text = c(grades[[i]], topics[[i]], paste("Unit", i))
    )
  })
  units <- tibble::tibble(
    ws_id = c(1L, 2L, 2L),
    unit_id = c(10L, 10L, 11L),
    unit_profiles = unit_profiles,
    items_profiles = lapply(unit_profiles, function(x) dplyr::mutate(x, item_no = 1L)),
    items_list = rep(list(tibble::tibble(item_no = 1L, item_id = "item")), 3)
  )
  attr(units, "ws_settings") <- tibble::tibble(
    ws_id = c(1L, 2L),
    unit_md_profile = c("single", "multi"),
    item_md_profile = c("single", "multi")
  )
  units
}

test_that("add_metadata preserves multiple values when workspace definitions differ", {
  units <- mock_mixed_metadata_units()

  out <- add_metadata(units)

  expect_equal(out$ws_id, units$ws_id)
  expect_equal(out$unit_id, units$unit_id)
  expect_equal(attr(out, "ws_settings"), attr(units, "ws_settings"))
  for (column in c("unit_metadata", "item_metadata")) {
    expect_equal(vapply(out[[column]], nrow, integer(1)), rep(1L, 3))
    expect_equal(lapply(out[[column]], function(x) x$Klassenstufe[[1]]),
                 list("5", c("5", "6", "7", "8"), c("7", "8")))
    expect_equal(lapply(out[[column]], function(x) x$Thema[[1]]),
                 list("A", c("A", "B"), c("A", "B", "C", "D")))
    expect_true(all(vapply(out[[column]], function(x) is.list(x$Klassenstufe), logical(1))))
    expect_equal(vapply(out[[column]], function(x) x$Titel, character(1)),
                 paste("Unit", 1:3))
  }
  for (profile in c("unit_md_profile", "item_md_profile")) {
    definitions <- attr(out, profile)
    expect_equal(definitions$multiple[definitions$profile_name == "Klassenstufe"],
                 c(FALSE, TRUE))
  }
})

test_that("add_metadata keeps scalar columns when all profiles allow only one value", {
  units <- mock_mixed_metadata_units()
  settings <- attr(units, "ws_settings")
  units <- units[1, ]
  attr(units, "ws_settings") <- settings[1, ]

  out <- add_metadata(units)

  for (column in c("unit_metadata", "item_metadata")) {
    expect_equal(out[[column]][[1]]$Klassenstufe, "5")
    expect_equal(out[[column]][[1]]$Thema, "A")
    expect_equal(out[[column]][[1]]$Titel, "Unit 1")
  }
})

metadata_entry <- function(label = "Variablenbezeichnung", value = "Expected label") {
  list(
    id = "iqb_var_name",
    label = list(list(lang = "de", value = label)),
    value = list(list(lang = "de", value = value)),
    valueAsText = list(list(lang = "de", value = value))
  )
}

metadata_profile <- function(profile_id, is_current, value = "Expected label", label = "Variablenbezeichnung") {
  list(
    entries = list(metadata_entry(label = label, value = value)),
    profileId = profile_id,
    isCurrent = is_current
  )
}

test_that("read_items_profiles keeps non-current profiles for downstream profile-id fallback", {
  unit_metadata <- list(
    items = list(
      list(
        uuid = "3b8bb988-c351-4f27-b5b1-92ef6fbe7098",
        id = "Sskodeu_a",
        variableId = "01a",
        profiles = list(metadata_profile("p60-item", FALSE, "Self concept German"))
      )
    )
  )

  profiles <- eatPrepTBA:::read_items_profiles(unit_metadata)

  expect_equal(nrow(profiles), 1)
  expect_equal(profiles$profile_id, "p60-item")
  expect_false(profiles$profile_is_current)
  expect_equal(profiles$profile_name, "Variablenbezeichnung")
  expect_equal(profiles$value_text, "Self concept German")
})

test_that("read_unit_profiles keeps non-current profiles for downstream profile-id fallback", {
  unit_metadata <- list(
    profiles = list(metadata_profile("p60-unit", FALSE, "Unit transcript", "Transkript"))
  )

  profiles <- eatPrepTBA:::read_unit_profiles(unit_metadata)

  expect_equal(nrow(profiles), 1)
  expect_equal(profiles$profile_id, "p60-unit")
  expect_false(profiles$profile_is_current)
  expect_equal(profiles$profile_name, "Transkript")
  expect_equal(profiles$value_text, "Unit transcript")
})

test_that("read_unit_profiles supports metadata-values 3 simple values", {
  unit_metadata <- list(
    profiles = list(
      list(
        profileId = "p60-unit",
        order = 0L,
        entries = list(
          list(
            id = "unit_time",
            value = list(
              raw = "1.03",
              asText = list(
                list(lang = "de", value = "1,03"),
                list(lang = "en", value = "1.03")
              )
            )
          )
        )
      )
    )
  )

  profiles <- eatPrepTBA:::read_unit_profiles(unit_metadata)

  expect_equal(nrow(profiles), 1)
  expect_equal(profiles$profile_id, "p60-unit")
  expect_equal(profiles$profile_order, 0L)
  expect_true(is.na(profiles$profile_is_current))
  expect_equal(profiles$profile_name, "unit_time")
  expect_equal(profiles$value_id, NA_character_)
  expect_equal(profiles$value_text, "1,03")
})

test_that("read_unit_profiles supports metadata-values 3 language-coded text values", {
  unit_metadata <- list(
    profiles = list(
      list(
        profileId = "p60-unit",
        order = 0L,
        entries = list(
          list(
            id = "transcript",
            label = list(
              list(lang = "de", value = "Transkript"),
              list(lang = "en", value = "Transcript")
            ),
            value = list(
              list(lang = "de", value = "Hallo"),
              list(lang = "en", value = "Hello")
            )
          )
        )
      )
    )
  )

  profiles <- eatPrepTBA:::read_unit_profiles(unit_metadata)

  expect_equal(profiles$profile_name, "Transkript")
  expect_equal(profiles$value_id, NA_character_)
  expect_equal(profiles$value_text, "Hallo")
})

test_that("read_items_profiles supports metadata-values 3 vocabulary entries", {
  unit_metadata <- list(
    items = list(
      list(
        uuid = "3b8bb988-c351-4f27-b5b1-92ef6fbe7098",
        id = "Sskodeu_a",
        variableId = "01a",
        profiles = list(
          list(
            profileId = "p60-item",
            order = 1L,
            entries = list(
              list(
                id = "school_type",
                label = list(
                  list(lang = "de", value = "Schulform"),
                  list(lang = "en", value = "School Type")
                ),
                value = list(
                  list(
                    id = "https://example.test/vocab/primary",
                    label = list(
                      list(lang = "de", value = "Grundschule"),
                      list(lang = "en", value = "Primary School")
                    ),
                    annotation = list(
                      list(lang = "de", value = "Primarstufe")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  profiles <- eatPrepTBA:::read_items_profiles(unit_metadata)

  expect_equal(nrow(profiles), 1)
  expect_equal(profiles$item_no, 1L)
  expect_equal(profiles$profile_id, "p60-item")
  expect_equal(profiles$profile_order, 1L)
  expect_equal(profiles$profile_name, "Schulform")
  expect_equal(profiles$value_id, "https://example.test/vocab/primary")
  expect_equal(profiles$value_text, "Primarstufe")
})

test_that("read item metadata supports current unit-items fields", {
  unit_metadata <- list(
    items = list(
      list(
        uuid = "3b8bb988-c351-4f27-b5b1-92ef6fbe7098",
        id = "Sskodeu_a",
        order = 2L,
        sourceVariableId = "01a",
        sourceVariableUuid = "variable-uuid-01a",
        variableId = "legacy-01a",
        variableReadOnlyId = "legacy-variable-uuid-01a",
        description = "Item description",
        createdAt = "2026-08-10T10:00:00.000Z",
        changedAt = "2026-08-10T11:00:00.000Z",
        metadata = list(
          list(
            profileId = "p60-item",
            order = 0L,
            entries = list(metadata_entry(value = "Current item label"))
          )
        )
      )
    )
  )

  items <- eatPrepTBA:::read_items_list(unit_metadata)
  profiles <- eatPrepTBA:::read_items_profiles(unit_metadata)

  expect_equal(items$item_no, 1L)
  expect_equal(items$item_id, "Sskodeu_a")
  expect_equal(items$variable_id, "01a")
  expect_equal(items$variable_ref, "variable-uuid-01a")
  expect_equal(items$item_order, 2L)
  expect_equal(items$item_description, "Item description")
  expect_false(any(c("item_position", "item_locked", "item_weighting") %in% names(items)))
  expect_equal(profiles$profile_id, "p60-item")
  expect_equal(profiles$value_text, "Current item label")
})

test_that("read item metadata still tolerates legacy item fields", {
  unit_metadata <- list(
    items = list(
      list(
        uuid = "3b8bb988-c351-4f27-b5b1-92ef6fbe7098",
        id = "Sskodeu_a",
        order = 0L,
        locked = FALSE,
        position = NA_character_,
        weighting = NA_real_,
        variableId = "01a",
        variableReadOnlyId = "variable-uuid-01a",
        profiles = list(
          list(
            profileId = "p60-item",
            isCurrent = TRUE,
            entries = list(metadata_entry(value = "Legacy item label"))
          )
        )
      )
    )
  )

  items <- eatPrepTBA:::read_items_list(unit_metadata)
  profiles <- eatPrepTBA:::read_items_profiles(unit_metadata)

  expect_equal(items$variable_id, "01a")
  expect_equal(items$variable_ref, "variable-uuid-01a")
  expect_false(any(c("item_position", "item_locked", "item_weighting") %in% names(items)))
  expect_equal(profiles$value_text, "Legacy item label")
})

test_that("read_items_profiles tolerates items without profiles", {
  unit_metadata <- list(
    items = list(
      list(
        uuid = "3b8bb988-c351-4f27-b5b1-92ef6fbe7098",
        id = "Sskodeu_a",
        variableId = "01a"
      )
    )
  )

  profiles <- eatPrepTBA:::read_items_profiles(unit_metadata)

  expect_equal(nrow(profiles), 1)
  expect_equal(profiles$item_no, NA_integer_)
  expect_equal(profiles$profile_id, NA_character_)
})

test_that("filter_current_metadata_profiles prefers the workspace profile over stale isCurrent flags", {
  profile_rows <- tibble::tibble(
    ws_id = 1116L,
    unit_id = 132797L,
    item_no = 1L,
    profile_id = c("p60-item", "old-item"),
    profile_is_current = c(FALSE, TRUE),
    profile_name = "Variablenbezeichnung",
    value_id = NA_character_,
    value_text = c("Self concept German", "Old label"),
    item_md_profile = "p60-item"
  )

  filtered <- eatPrepTBA:::filter_current_metadata_profiles(
    profile_rows,
    md_profile = "item_md_profile",
    group_cols = c("ws_id", "unit_id", "item_no")
  )

  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$profile_id, "p60-item")
  expect_equal(filtered$value_text, "Self concept German")
})

test_that("filter_current_metadata_profiles falls back to isCurrent if profile ids are unavailable", {
  profile_rows <- tibble::tibble(
    ws_id = 1116L,
    unit_id = 132797L,
    item_no = 1L,
    profile_is_current = c(FALSE, TRUE),
    profile_name = "Variablenbezeichnung",
    value_id = NA_character_,
    value_text = c("Old label", "Current label"),
    item_md_profile = "p60-item"
  )

  filtered <- eatPrepTBA:::filter_current_metadata_profiles(
    profile_rows,
    md_profile = "item_md_profile",
    group_cols = c("ws_id", "unit_id", "item_no")
  )

  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$value_text, "Current label")
})

test_that("filter_current_metadata_profiles hides profiles with order minus one", {
  profile_rows <- tibble::tibble(
    ws_id = 1116L,
    unit_id = 132797L,
    item_no = 1L,
    profile_id = c("hidden-item", "visible-item"),
    profile_order = c(-1L, 0L),
    profile_name = "Variablenbezeichnung",
    value_id = NA_character_,
    value_text = c("Hidden label", "Visible label"),
    item_md_profile = NA_character_
  )

  filtered <- eatPrepTBA:::filter_current_metadata_profiles(
    profile_rows,
    md_profile = "item_md_profile",
    group_cols = c("ws_id", "unit_id", "item_no")
  )

  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$profile_id, "visible-item")
  expect_equal(filtered$value_text, "Visible label")
})

test_that("filter_current_metadata_profiles hides the workspace profile with order minus one", {
  profile_rows <- tibble::tibble(
    ws_id = 1116L,
    unit_id = 132797L,
    item_no = 1L,
    profile_id = "p60-item",
    profile_order = -1L,
    profile_name = "Variablenbezeichnung",
    value_id = NA_character_,
    value_text = "Hidden label",
    item_md_profile = "p60-item"
  )

  filtered <- eatPrepTBA:::filter_current_metadata_profiles(
    profile_rows,
    md_profile = "item_md_profile",
    group_cols = c("ws_id", "unit_id", "item_no")
  )

  expect_equal(nrow(filtered), 0)
})
