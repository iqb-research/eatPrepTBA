# Add item identifiers to variable-level data

Adds the item identifier stored in Studio metadata by matching
`unit_key` and `variable_id`. No API requests or coding operations are
performed. The player definition alone does not contain this metadata
mapping.

## Usage

``` r
add_item_id(data, units, overwrite = FALSE)
```

## Arguments

- data:

  Data frame containing `unit_key` and `variable_id`.

- units:

  Data frame returned by
  [`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md)
  with `metadata = TRUE`. Uses its `items_list` list-column, or
  `item_metadata` after
  [`add_metadata()`](https://iqb-research.github.io/eatPrepTBA/reference/add_metadata.md).

- overwrite:

  Logical. Replace an existing `item_id` column? Defaults to `FALSE`,
  which raises an error if the column already exists.

## Value

`data` with an `item_id` character column. Row count and order are
preserved. With `overwrite = TRUE`, unmatched rows replace old IDs with
`NA`.

## Details

Missing or empty keys and item identifiers are ignored in the lookup.
Unmatched rows receive `NA_character_`; no item identifiers are
invented. Repeated identical mappings are collapsed. Multiple distinct
item identifiers for the same pair of keys anywhere in `units` cause an
error, including conflicts between workspaces. Subset `units` to the
relevant versions first. When both metadata columns exist, `items_list`
is used.

## Examples

``` r
units <- tibble::tibble(
  unit_key = "U1",
  items_list = list(tibble::tibble(variable_id = "V1", item_id = "I1"))
)
responses <- tibble::tibble(unit_key = c("U1", "U1"),
                            variable_id = c("V1", "V2"))
add_item_id(responses, units)
#> # A tibble: 2 × 3
#>   unit_key variable_id item_id
#>   <chr>    <chr>       <chr>  
#> 1 U1       V1          I1     
#> 2 U1       V2          NA     
```
