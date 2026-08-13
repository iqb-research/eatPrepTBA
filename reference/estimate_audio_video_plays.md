# Estimate audio and video play counts from response JSONs

**\[experimental\]**

Extracts audio and video response elements from the `responses` JSON
column. Media elements are identified by an `id` value that marks the
element as audio or video. Malformed or missing response JSONs are
ignored.

## Usage

``` r
estimate_audio_video_plays(response_df)
```

## Arguments

- response_df:

  Tibble. Response data retrieved with
  [get_responses()](https://iqb-research.github.io/eatPrepTBA/reference/get_responses.md)
  or read with
  [read_responses()](https://iqb-research.github.io/eatPrepTBA/reference/read_responses.md).
  Must contain `responses`, `group_id`, `login_name`, `login_code`,
  `booklet_id`, and `unit_key`. If `page_no` is present, it is retained
  in the output.

## Value

A tibble with one row per participant, unit, page, and media element. It
contains the session and unit columns, `media_id`, `media_type`,
`status`, `n_plays`, and `media_key`. `media_key` is a namespaced
compound key of the form `unit_key__media__media_type__media_id` and is
intended for display or export, not as a replacement for the separate
key columns.
