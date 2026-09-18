# Vignette example data

These files illustrate the documentation without requiring a Studio login.
They are not live API results.

- `example_properties.rds`: an anonymized recorded response for unit `145615`
  (`D2_BT18`), adapted to the current metadata format on 2026-09-18.
  Item links use `sourceVariableId` and `sourceVariableUuid`; retired
  `locked`, `position`, and `weighting` fields were removed. Profiles use
  `order`; vocabulary entries use `label`, simple values use `raw`/`asText`,
  and language-coded text remains a list of language/value pairs.
- `example_units.rds`: an anonymized recorded three-unit snapshot (`D2_BT18`,
  `D3_UF04`, `D3_JP03`) containing all 37 original variables and their metadata.
  Retired item columns were removed; profile identifiers and ordering information
  not retained in this historical snapshot are represented as missing values.
  The separate `example_properties.rds` response has an empty `variables` list
  and cannot be used to reconstruct this complete unit table.
- `example_httr2_result.rds`: an anonymized historical unit-list response used
  to illustrate the outer API response structure.
- `example_scheme_string.json`: the recorded coding scheme for the example
  unit, used to illustrate parsing a JSON string.

The screenshots were recorded with an earlier Studio version. When refreshing
the examples from Studio, anonymize user fields and regenerate the prepared
unit table with the package version used to build the documentation.
