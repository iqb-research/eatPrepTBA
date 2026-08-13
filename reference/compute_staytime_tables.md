# Computes quantile tables of stay times

**\[experimental\]**

Takes various data sources with unit, item and participant stay times
and metadata, and computes quantiles for use in reports. Uses the
layout_staytime_tables function to layout them for quarto reports.
IMPORTANT: The function assumes that there is one study design for
regular schools (RS) and one for special needs schools (FS), and that
the booklets with FS design can be identified with certainty by a string
which appears as a substring of final_responses\$booklet_id entries.
This string is passed to the function as the argument FS_marker.

## Usage

``` r
compute_staytime_tables(
  fach,
  log_times,
  unit_domains,
  final_responses,
  units_cs,
  unit_meta,
  students_select,
  FS_marker,
  output_path,
  min_page_n_valid = 2L,
  response_filter = c("coded", "all")
)
```

## Arguments

- fach:

  String. Letter denoting the school subject in question.

- log_times:

  Data frame. Log data with stay times for one school subject. Result of
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
  unit_metadata, item_metadata, Aufgabenzeit

- students_select:

  Vector of strings. If necessary, contains the IDSTUDs of students to
  include in the analysis. Otherwise, NULL

- FS_marker:

  String. If this string appears in final_responses\$booklet_id, then
  the respective booklet is treated as containing a special needs school
  study design.

- output_path:

  String. Directory to store prepared tables in.

- min_page_n_valid:

  Integer. Minimum number of non-missing page stay times required for
  page-level quantiles. The default, 2, keeps sparse but real observed
  page times visible in exploratory stay-time reports. Set to 11 to
  reproduce the legacy rule that pages needed more than ten observed
  stay times.

- response_filter:

  Character. If "coded", keep the legacy behavior and use only coded,
  used, non-example response rows. If "all", retain all response rows
  after unit filtering.

## Value

A tibble. Also saves large tibble with name `tab_{domain}.RData` under
output_path, including quantile dot plots. This can then be incorporated
into a quarto document as described in the example below.

## Details

Author: Philipp Franikowski, restructured by Lea Musiolek

The resulting saved tibble can be incorporated into a Quarto document
using `load("output_path/tab_\{domain\}.RData")`


    ::: panel-tabset

    ### Units

    \verb{```{r}}
    #| column: screen
    tab
    \verb{```}

    ### Items

    \verb{```{r}}
    #| column: screen
    tab_item
    \verb{```}

    :::

## Examples

``` r
if (FALSE) { # \dontrun{
fach <- c("F")
FS_marker <- "S"

data_path <- "..."
prep_path <- "..."
db_path <- "..."
output_path <- "..."

unit_meta <- readRDS(paste(c(db_path, "units_md.rds"), collapse="")) # unit meta data
units_cs <- readRDS(paste(c(db_path, "units_cs.rds"), collapse="")) # unit coding scheme
final_responses <- readRDS(paste(c(data_path, "pilot??_f.rds"), collapse="")) #final results file

log_times <- readRDS(paste(c(prep_path, "log_times_", fach, ".rds"), collapse="")) # output
# of estimate_unit_times()
unit_domains <- readRDS(paste(c(data_path, "unit_domains_", fach, ".rds"), collapse="")) # table
# with one row per unit, columns for unit_key, school subject as letter, and testing domain
# (subject and competence type) as 2-letter combination

eatPrepTBA::compute_staytime_tables(fach,
                                    log_times,
                                    unit_domains,
                                    final_responses,
                                    units_cs,
                                    unit_meta,
                                    students_select,
                                    FS_marker,
                                    output_path)
} # }

# Requires packages eatPrepTBA, tidyverse, reactable, and htmltools there.
```
