# Sets and layouts quantile tables of stay times

**\[experimental\]**

Function for rendering pre-existing quantile tables of unit, page and
item stay times into a shape and layout suitable for a quarto document.
Attention! Dataset needs to be called "data" (Philipp).

## Usage

``` r
layout_staytime_tables(
  data,
  id = "unit-table",
  subject = "dep",
  filterable = TRUE,
  searchable = TRUE,
  sortable = TRUE,
  views = TRUE,
  download = NULL
)
```

## Arguments

- data:

  Pre-computed quantile tables of stay times

- id:

  String. Either "unit-table" (default) or "item-table", changes some
  details about the layout.

- subject:

  String. Default is "dep". Legacy parameter.

- filterable:

  Boolean. Default is TRUE. Necessary for reactable function.

- searchable:

  Boolean. Default is TRUE. Necessary for reactable function.

- sortable:

  Boolean. Default is TRUE. Necessary for reactable function.

- views:

  Boolean. Default is TRUE. Legacy parameter.

- download:

  String. Default is NULL. Download file name (legacy).

## Value

Tables, including quantile dot plots, ready for use in quarto document

## Details

Author: Philipp Franikowski, restructuring by Lea Musiolek
