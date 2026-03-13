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

  description

- id:

  description

- subject:

  description

- filterable:

  description

- searchable:

  description

- sortable:

  description

- views:

  description

- download:

  description

## Value

Tables, including quantile dot plots, ready for using in quarto document

## Details

Author: Philipp Franikowski, restructuring by Lea Musiolek
