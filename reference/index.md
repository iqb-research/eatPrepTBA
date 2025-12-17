# Package index

## Connecting to the API

Functions and classes for communicating with the IQB Testcenter API

### Functions for access

- [`login_studio()`](https://franikowsp.github.io/eatPrepTBA/reference/login_studio.md)
  : Generate a LoginStudio object for the IQB Studio Lite
- [`login_testcenter()`](https://franikowsp.github.io/eatPrepTBA/reference/login_testcenter.md)
  : Generate a LoginTestcenter object for the IQB Testcenter
- [`access_workspace()`](https://franikowsp.github.io/eatPrepTBA/reference/access_workspace.md)
  : Access a workspace

### Classes

- [`Login-class`](https://franikowsp.github.io/eatPrepTBA/reference/Login-class.md)
  : Login credentials
- [`LoginStudio-class`](https://franikowsp.github.io/eatPrepTBA/reference/LoginStudio-class.md)
  : Login credentials for IQB Studio
- [`LoginTestcenter-class`](https://franikowsp.github.io/eatPrepTBA/reference/LoginTestcenter-class.md)
  : Login credentials for IQB Testcenter
- [`Workspace-class`](https://franikowsp.github.io/eatPrepTBA/reference/Workspace-class.md)
  : Workspace access
- [`WorkspaceStudio-class`](https://franikowsp.github.io/eatPrepTBA/reference/WorkspaceStudio-class.md)
  : Workspace access for IQB Studio
- [`WorkspaceTestcenter-class`](https://franikowsp.github.io/eatPrepTBA/reference/WorkspaceTestcenter-class.md)
  : Workspace access for IQB Testcenter

## Preparing unit metadata and coding schemes

Functions to prepare unit data from IQB Studio or Testcenter

### List resources on IQB Studio

- [`list_units()`](https://franikowsp.github.io/eatPrepTBA/reference/list_units.md)
  : List unit files
- [`list_groups()`](https://franikowsp.github.io/eatPrepTBA/reference/list_groups.md)
  : List groups

### List resources on IQB Testcenter

- [`list_files()`](https://franikowsp.github.io/eatPrepTBA/reference/list_files.md)
  : List files
- [`list_resources()`](https://franikowsp.github.io/eatPrepTBA/reference/list_resources.md)
  : List unit resource files
- [`list_units()`](https://franikowsp.github.io/eatPrepTBA/reference/list_units.md)
  : List unit files
- [`list_booklets()`](https://franikowsp.github.io/eatPrepTBA/reference/list_booklets.md)
  : List booklet files
- [`list_testtakers()`](https://franikowsp.github.io/eatPrepTBA/reference/list_testtakers.md)
  : List testtakers files
- [`list_system_checks()`](https://franikowsp.github.io/eatPrepTBA/reference/list_system_checks.md)
  : List system check files

### Handle units and workspaces on IQB Studio

- [`get_units()`](https://franikowsp.github.io/eatPrepTBA/reference/get_units.md)
  : Get multiple units with resources

### Retrieve responses from IQB Testcenter

- [`get_responses()`](https://franikowsp.github.io/eatPrepTBA/reference/get_responses.md)
  : Get responses directly from Testcenter
- [`read_responses()`](https://franikowsp.github.io/eatPrepTBA/reference/read_responses.md)
  : Reads responses files
- [`prepare_responses()`](https://franikowsp.github.io/eatPrepTBA/reference/prepare_responses.md)
  **\[experimental\]** : Prepares responses
- [`get_system_checks()`](https://franikowsp.github.io/eatPrepTBA/reference/get_system_checks.md)
  : Get system check reports
- [`read_system_checks()`](https://franikowsp.github.io/eatPrepTBA/reference/read_system_checks.md)
  : Reads and prepares system check file
- [`get_design()`](https://franikowsp.github.io/eatPrepTBA/reference/get_design.md)
  : Recovers a design from IQB Testcenter
- [`get_booklets()`](https://franikowsp.github.io/eatPrepTBA/reference/get_booklets.md)
  : Get a booklets from IQB Testcenter
- [`read_booklet()`](https://franikowsp.github.io/eatPrepTBA/reference/read_booklet.md)
  : Reads a booklet XML
- [`get_testtakers()`](https://franikowsp.github.io/eatPrepTBA/reference/get_testtakers.md)
  : Get a testtakers from IQB Testcenter
- [`read_testtakers()`](https://franikowsp.github.io/eatPrepTBA/reference/read_testtakers.md)
  : Reads a testtakers XML
- [`get_results()`](https://franikowsp.github.io/eatPrepTBA/reference/get_results.md)
  : Get results
- [`get_logs()`](https://franikowsp.github.io/eatPrepTBA/reference/get_logs.md)
  : Get logs
- [`read_logs()`](https://franikowsp.github.io/eatPrepTBA/reference/read_logs.md)
  : Reads log files
- [`prepare_logs()`](https://franikowsp.github.io/eatPrepTBA/reference/prepare_logs.md)
  **\[experimental\]** : Prepares logs
- [`get_reviews()`](https://franikowsp.github.io/eatPrepTBA/reference/get_reviews.md)
  : Get reviews
- [`compute_sizes()`](https://franikowsp.github.io/eatPrepTBA/reference/compute_sizes.md)
  : Computes resource sizes for Testcenter instances

### Functions for units

- [`add_metadata()`](https://franikowsp.github.io/eatPrepTBA/reference/add_metadata.md)
  : Add metadata to units
- [`add_coding_scheme()`](https://franikowsp.github.io/eatPrepTBA/reference/add_coding_scheme.md)
  : Adds prepared coding scheme to units
- [`extract_metadata()`](https://franikowsp.github.io/eatPrepTBA/reference/extract_metadata.md)
  : Extract metadata profiles from units with metadata

## Managing studies

Functions to check units, prepare material for administration and coding

### Prepare studies on IQB Studio

- [`get_units()`](https://franikowsp.github.io/eatPrepTBA/reference/get_units.md)
  : Get multiple units with resources
- [`get_states()`](https://franikowsp.github.io/eatPrepTBA/reference/get_states.md)
  : Get states
- [`get_settings()`](https://franikowsp.github.io/eatPrepTBA/reference/get_settings.md)
  : Get workspace settings
- [`get_coding_report()`](https://franikowsp.github.io/eatPrepTBA/reference/get_coding_report.md)
  : Get coding report
- [`prepare_coding_scheme()`](https://franikowsp.github.io/eatPrepTBA/reference/prepare_coding_scheme.md)
  : Prepares a readable version of a coding scheme of one unit
- [`download_codebook()`](https://franikowsp.github.io/eatPrepTBA/reference/download_codebook.md)
  : Download codebook
- [`prepare_codebook()`](https://franikowsp.github.io/eatPrepTBA/reference/prepare_codebook.md)
  : Prepares a rectangular codebook
- [`download_units()`](https://franikowsp.github.io/eatPrepTBA/reference/download_units.md)
  : Download units
- [`add_groups()`](https://franikowsp.github.io/eatPrepTBA/reference/add_groups.md)
  : Add workspace groups for a Studio workspace

### Prepare studies on IQB Testcenter

- [`generate_testtakers()`](https://franikowsp.github.io/eatPrepTBA/reference/generate_testtakers.md)
  : Generates testtakers XML from unit information
- [`generate_booklet()`](https://franikowsp.github.io/eatPrepTBA/reference/generate_booklet.md)
  **\[deprecated\]** : Generates booklets XML from unit information
- [`generate_booklets()`](https://franikowsp.github.io/eatPrepTBA/reference/generate_booklets.md)
  : Generates booklet XMLs from booklet, testlet, and unit information
- [`upload_file()`](https://franikowsp.github.io/eatPrepTBA/reference/upload_file.md)
  : Upload file

### Code responses and work with coded responses and logs from IQB Testcenter

- [`code_responses()`](https://franikowsp.github.io/eatPrepTBA/reference/code_responses.md)
  : Code unit responses with coding schemes
- [`complete_design()`](https://franikowsp.github.io/eatPrepTBA/reference/complete_design.md)
  : Complete design with coded responses
- [`estimate_unit_times()`](https://franikowsp.github.io/eatPrepTBA/reference/estimate_unit_times.md)
  **\[experimental\]** : Estimates stay times from log data
- [`evaluate_psychometrics()`](https://franikowsp.github.io/eatPrepTBA/reference/evaluate_psychometrics.md)
  : Evaluates frequencies and discrimination parameters of codes and
  categories
