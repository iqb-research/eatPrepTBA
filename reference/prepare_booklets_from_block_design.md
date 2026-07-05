# Prepare booklet specifications from a tabular block design

Prepare booklet specifications from a tabular block design

## Usage

``` r
prepare_booklets_from_block_design(
  booklets,
  blocks,
  units,
  restrictions = NULL,
  booklet_subject_fn = NULL,
  add_start_end = TRUE,
  booklet_filter = NULL,
  wrap_blocks = TRUE,
  default_leave = NULL,
  keep_leave_without_minutes = FALSE
)
```

## Arguments

- booklets:

  A tibble with one row per booklet. It must contain either `booklet_id`
  or `booklet` and one or more `block_*` columns containing the block
  ids in booklet order. `booklet_label`, `booklet_description`, and
  `booklet_configuration` are optional.

- blocks:

  A tibble with one row per block. It must contain `block` and may
  contain `subject`, `domain`, `minutes`, `testlet_id`, `testlet_label`,
  and `wrap`. Missing `subject` or `domain` values apply to all matching
  unit subjects or domains.

- units:

  A tibble with one row per unit. It must contain `block` and
  `unit_key`; `subject`, `domain`, `sequence`, `unit_alias`,
  `unit_label`, and `unit_labelshort` are optional.

- restrictions:

  An optional tibble with testlet restriction information. It may
  contain `booklet_id`, `subject`, `domain`, `block`, `testlet_id`,
  `testlet_label`, `minutes`, `leave`, `code`, `code_to_enter`,
  `presentation`, `response`, and `wrap`. Missing `subject` or `domain`
  values apply to all matching unit subjects or domains. It can also use
  the compact timing format `design`, `block`, `seconds`, and optional
  `block_group` and `leave`. `design` is matched as an
  underscore-delimited part of `booklet_id`. `block` contains one
  booklet position column, such as `"block_4"`. If `block_group` is
  present, rows with the same `design` and `block_group` are combined
  into one timed testlet and `block_group` is used as the testlet label.
  If `block_group` is missing or empty, each `block` becomes its own
  timed testlet. If `leave` is missing or empty in this compact format,
  it defaults to `"allowed"`.

- booklet_subject_fn:

  Optional function that receives `booklet_id` and returns a subject
  used to disambiguate blocks that exist for several subjects.

- add_start_end:

  Logical. Should `Start_page` and `End_page` units be added to every
  booklet?

- booklet_filter:

  Optional character vector of booklet ids to keep.

- wrap_blocks:

  Logical. Should blocks be wrapped into testlets by default?

- default_leave:

  Optional default value for the `leave` attribute of `TimeMax`
  restrictions.

- keep_leave_without_minutes:

  Logical. Should `leave` be kept when no `minutes` restriction is
  present?

## Value

A tibble that can be passed to
[`generate_booklets()`](https://iqb-research.github.io/eatPrepTBA/reference/generate_booklets.md).
