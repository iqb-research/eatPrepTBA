# Generates booklet XMLs from booklet, testlet, and unit information

Please note that the function currently only works for units that are
nested within

## Usage

``` r
generate_booklets(
  booklets,
  app_version = "16.0.2",
  login = NULL,
  booklet_config_version = c("18.0", "legacy-16")
)
```

## Arguments

- booklets:

  Must be a tibble with the columns `booklet_id`, `booklet_label`, and
  `booklet_units`.Optionally, the columns `booklet_description`
  (character), `booklet_configuration` (list), and
  `booklet_custom_texts` (named list) can be added. The (list) column
  `booklet_units` is a nested tibble with columns `testlet_id`,
  `testlet_label`, and `units`. Optionally, it can contain the columns
  `testlet_restrictions` and `testlets`. Finally, the (list) column
  `units` is again a nested tibble with columns `unit_key`,
  `unit_alias`, `unit_label`, and `unit_labelshort`. The optional
  `testlets` column can contain nested tibbles with the same structure
  as `booklet_units`.

- app_version:

  Version of the target Testcenter instance. Defaults to `"16.0.0"`.

- login:

  Target Testcenter instance. If it is available, the `app_version` will
  be overwritten.

- booklet_config_version:

  Booklet configuration version. `"18.0"` emits the current Testcenter
  booklet configuration keys. `"legacy-16"` emits the legacy key set
  used by older Testcenter 16 workflows.

## Value

A booklet XML.
