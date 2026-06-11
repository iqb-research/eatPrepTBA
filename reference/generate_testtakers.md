# Generates testtakers XML from unit information

Generates testtakers XML from unit information

## Usage

``` r
generate_testtakers(
  testtakers,
  custom_texts = NULL,
  profiles = NULL,
  app_version = "16.0.2",
  login = NULL
)
```

## Arguments

- testtakers:

  Must be a data frame with the columns `group_id` and `login_name`.

- custom_texts:

  Optional. List of custom texts to be modified.

- profiles:

  Optional. Data frame of profiles for the group monitor.

- app_version:

  Version of the target Testcenter instance. Defaults to `"16.0.0"`.

- login:

  Target Testcenter instance. If it is available, the `app_version` will
  be overwritten.

## Value

A testtakers XML.
