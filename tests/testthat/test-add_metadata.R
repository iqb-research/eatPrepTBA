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
