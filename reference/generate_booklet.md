# Generates booklets XML from unit information

**\[deprecated\]**

## Usage

``` r
generate_booklet(
  booklet_id,
  booklet_label,
  booklet_description = NULL,
  booklet_configuration = NULL,
  units = NULL,
  testlets = NULL,
  app_version = "16.0.2",
  login = NULL,
  booklet_config_version = c("18.0", "legacy-16")
)
```

## Arguments

- booklet_id:

  Character. `Id` of the booklet to be generated.

- booklet_label:

  Character. `Label` of the booklet to be generated.

- booklet_description:

  Character. `Description` of the booklet to be generated. Defaults to
  `NULL`.

- booklet_configuration:

  A list that can be submitted to
  [`configure_booklet()`](https://iqb-research.github.io/eatPrepTBA/reference/configure_booklet.md).

- units:

  Tbd.

- testlets:

  Tbd.

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
