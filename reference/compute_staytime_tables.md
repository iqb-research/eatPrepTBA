# Computes quantile tables of stay times

**\[experimental\]**

Takes various data sources with unit, item and participant stay times
and metadata, and computes quantiles for use in reports. Uses the
layout_staytime_tables function to layout them for quarto reports.

## Usage

``` r
compute_staytime_tables(
  fach,
  log_times,
  unit_domains,
  final_responses,
  units_cs,
  unit_meta,
  students_select
)
```

## Arguments

- fach:

  String. Letter denoting the school subject in question.

- log_times:

  Data frame. Log data with stay times for one subject. Result of
  pulling log data with eatPrepTBA::get_logs() and then using
  eatPrepTBA::estimate_unit_times(). If necessary, data for the subject
  in question needs to be selected.

- unit_domains:

  Data frame. Three string variables: subject (should equal fach),
  domain (''), unit_key. Should contain each relevant unit_key once.
  Important for assigning subject and domain to each unit key down the
  line. Can be generated from the blocks.xlsx used for generating the
  tests.

- final_responses:

  Data frame. Contains the item-wise and respondent-wise responses,
  ideally corrected for switches etc. Relevant variables: id_used,
  code_type, code_id, variable_source_type, booklet_id, item_id, IDSTUD,
  group_id, login_name, login_code, unit_key, variable_page

- units_cs:

  Data frame. Unit-wise coding schemes, exported directly from IQB
  Studio. Relevant variables: unit_key, unit_codes, variable_label,
  variable_page, variable_id

- unit_meta:

  Data frame. Unit-wise metadata, exported directly from IQB Studio.
  Relevant variables: ws_id, unit_id, unit_key, unit_label,
  unit_metadata, item_metadata

- students_select:

  Vector of strings. If necessary, contains the IDSTUDs of students to
  include in the analysis. Otherwise, NULL

## Value

None; saves tables, including quantile dot plots, ready for using in
quarto document

## Details

Author: Philipp Franikowski, restructured by Lea Musiolek
