# Generate Testcenter testtakers XML

Generate Testcenter testtakers XML

## Usage

``` r
generate_testtakers(
  testtakers,
  custom_texts = NULL,
  profiles = NULL,
  app_version = "18.0.0",
  login = NULL,
  testtakers_version = c("18.0", "legacy-16")
)
```

## Arguments

- testtakers:

  Data frame in the eatPrepTBA testtakers target format. It must contain
  `group_id` and `login_name`. In current Testcenter 18 output,
  `group_label` is required by the XML specification; missing labels are
  filled from `group_id` with a warning. Optional columns are
  `group_label`, `group_valid_to`, `group_valid_from`,
  `group_valid_for`, `login_pw`, `login_mode`, `login_monitor_code`,
  `booklet_id`, `booklet_codes`, `booklet_state`, and `profile_id`.

- custom_texts:

  Optional named list of Testcenter custom text keys and replacement
  strings. For example, `list(AppTitle = "Pilot")` becomes a
  `CustomText` node with key `AppTitle`.

- profiles:

  Optional data frame defining group-monitor profiles. It must contain
  `profile_id` when supplied. Optional columns are `profile_label`,
  `block_column`, `unit_column`, `view`, `group_column`,
  `booklet_column`, `booklet_states_columns`, `filter_pending`,
  `filter_locked`, `autoselect_next_block`, `filter_label`,
  `filter_field`, `filter_type`, `filter_value`, `filter_sub_value`, and
  `filter_not`.

- app_version:

  Version of the target Testcenter instance. Defaults to `"18.0.0"`.
  This is used for legacy raw-GitHub schema URLs; current Testcenter 18
  output uses `testtakers_version` for the `w3id.org` schema URL.

- login:

  Target Testcenter instance. If it is available, the `app_version` will
  be overwritten.

- testtakers_version:

  Testtakers XML specification version. `"18.0"` emits the current
  Testcenter testtakers schema URL. `"legacy-16"` emits the legacy
  Testcenter 16 schema URL used by older workflows.

## Value

An `xml_document` containing a Testcenter `Testtakers` XML document.

## Details

This function expects the final target table for a Testcenter testtakers
file. Project-specific wrappers can read Excel files, join booklet
designs, expand days or parts, and create monitor logins, but they
should eventually hand over a `testtakers` data frame with one row per
login/booklet or login/profile assignment.

Rows with the same `group_id` form one `Group`. Rows with the same
`login_name` form one `Login`; a `login_name` must not be reused across
groups. If `booklet_id` is present, `Booklet` children are added to the
login. If `profile_id` is present, `Profile` references are added
instead. In Testcenter 18.0 output, one login cannot mix `Booklet` and
`Profile` children.

`generate_testtakers()` returns an XML document only. It does not read
input files or write `testtakers.xml`; wrapper functions should call
[`xml2::write_xml()`](http://xml2.r-lib.org/reference/write_xml.md) if a
file should be created.

`custom_texts` is passed through to the XML as named `CustomText` nodes
and can be an extensive project-specific list. Typical keys override
text in the booklet player (`booklet_*`), group monitor (`gm_*`), and
login screen (`login_*`). Values can contain longer or multi-line
messages. Keep Testcenter placeholders such as `%s` unchanged when
replacing texts. The function checks that the list is named, but it does
not validate the key set against a particular Testcenter version;
unsupported or misspelled keys are handled by Testcenter.

## Examples

``` r
testtakers <- tibble::tibble(
  group_id = "G1",
  group_label = "Group 1",
  login_name = "student01",
  login_pw = "pw01",
  login_mode = "run-hot-return",
  booklet_id = "BOOKLET_1",
  booklet_codes = "A1 B2"
)

custom_texts <- list(
  booklet_codeToEnterPrompt = "Please enter the release code.",
  booklet_msgSoonTimeOver = "You have %s minute(s) left for this section.",
  gm_col_personLabel = "Login/student code",
  gm_control_pause = "Pause",
  login_codeInputTitle = "Enter student code"
)

# Wrapper for the current Testcenter 18 testtakers XML specification.
write_testtakers_xml <- function(testtakers, output = tempfile(fileext = ".xml")) {
  xml <- generate_testtakers(
    testtakers,
    custom_texts = custom_texts
  )
  xml2::write_xml(xml, output)
  output
}

output_18 <- write_testtakers_xml(testtakers)

# Wrapper pinned to the legacy Testcenter 16 schema location.
write_legacy_testtakers_xml <- function(testtakers, output = tempfile(fileext = ".xml")) {
  xml <- generate_testtakers(
    testtakers,
    custom_texts = custom_texts,
    app_version = "16.0.2",
    testtakers_version = "legacy-16"
  )
  xml2::write_xml(xml, output)
  output
}

output_legacy <- write_legacy_testtakers_xml(testtakers)
```
