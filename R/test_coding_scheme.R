#' Test Coding Scheme
#'
#' @param data IQB-Studio data. Needs to be prepared with 'get_units()', 'get_coding_report()' and 'add_coding_scheme()'.
#' @param exceptions A list of unit-exceptions from the tests. Needs to be given as 'exceptions = list("test_nr" = "unit_key")', for example 'exceptions = list("8" = "MZB270")'.
#' @param name_list A logical vector of TRUE or FALSE to identify testnumbers for 'exceptions'. If TRUE, only testname and testnumber are printed. Default is FALSE.
#' @param console A logical vector of TRUE or FALSE. If TRUE, testthat() output is printed to console in addition to the html-table. Default is FALSE.
#'
#'
#' @description
#' This function can be used to determine frequently made mistakes in the coding scheme.
#'
#' @return An html-table and console output if needed.
#' @export
#'
#' @aliases
#' test_coding_scheme,WorkspaceStudio-method
#'
#' @examples
#' \dontrun{
#' test_coding_scheme(
#'   data = data, 
#'   name_list = FALSE, 
#'   console = FALSE, 
#'   exceptions = rlang::list2("8" = "MZB270","12" = "MMB035")
#' )
#' }
#'


test_coding_scheme <- function(data, exceptions = list(), name_list = FALSE, console = FALSE) {
  checkmate::assert_tibble(data)
  checkmate::assert_list(exceptions)
  checkmate::assert_logical(name_list, len = 1)
  checkmate::assert_logical(console, len = 1)

  # Initialisierung ------------------------------------------------------------

  all_tests     <- character()
  error_results <- tibble::tibble()

  # Testdefinitionen -----------------------------------------------------------
  tests <- list(
    # Schemer
    list(
      desc = "Schemer ist aktuell",
      print_cols = c("unit_key", "variable_id", "schemer", "link"),
      code = function() {
        data_filter_base_no_value %>%
          tidyr::unnest(variable_codes) %>%
          dplyr::filter(schemer != "iqb-schemer@2.5")
      }
    ),
    # Ableitungen
    list(
      desc = "Ableitungen haben keine IDs verloren (d_XXXXXXXX)",
      print_cols = c("unit_key", "variable_id", "link"),
      code = function() {
        data_filter_base_no_value %>%
          tidyr::unnest(variable_codes) %>%
          dplyr::filter(stringr::str_detect(variable_id, "d_[0-9]{8}"))
      }
    ),
    # Einzigartigkeit
    list(
      desc = "Einzigartigkeit der IDs in den Units",
      print_cols = c("unit_key", "variable_id", "n"),
      code = function() {
        data_filter_base_no_value %>%
          dplyr::group_by(unit_key) %>%
          dplyr::count(variable_id) %>%
          dplyr::filter(n > 1)
      }
    ),
    # Leere Kodieranweisungen
    list(
      desc = "Keine leeren automatischen Kodieranweisungen",
      print_cols = c("unit_key", "variable_id", "rule_parameter", "link"),
      code = function() {
        data_filter_base_no_value %>%
          tidyr::unnest(variable_codes) %>%
          dplyr::filter(rule_parameter == "")
      }
    ),
    # Fehlende Scores
    list(
      desc = "Keine fehlenden Scores in Variablen ohne '_'",
      print_cols = c("unit_key", "variable_id", "code_score", "link"),
      code = function() {
        data_filter_base_no_value %>%
          tidyr::unnest(variable_codes) %>%
          dplyr::filter(is.na(code_score) &
                          !stringr::str_detect(variable_id, "_"))
      }
    ),
    # FULL_CREDIT
    list(
      desc = "FULL_CREDIT hat code_id beginnend mit 1",
      print_cols = c("unit_key", "variable_id", "code_type", "code_id", "link"),
      code = function() {
        data_filter_base_no_value %>%
          tidyr::unnest(variable_codes) %>%
          dplyr::filter(code_type == "FULL_CREDIT" &
                          !stringr::str_detect(code_id, "^1"))
      }
    ),
    # PARTIAL_CREDIT
    list(
      desc = "PARTIAL_CREDIT hat code_id beginnend mit 2",
      print_cols = c("unit_key", "variable_id", "code_type", "code_id", "link"),
      code = function() {
        data_filter_base_no_value %>%
          tidyr::unnest(variable_codes) %>%
          dplyr::filter(code_type == "PARTIAL_CREDIT" &
                          !stringr::str_detect(code_id, "^2"))
      }
    ),
    # TO_CHECK
    list(
      desc = "TO_CHECK hat code_id beginnend mit 3",
      print_cols = c("unit_key", "variable_id", "code_type", "code_id", "link"),
      code = function() {
        data_filter_base_no_value %>%
          tidyr::unnest(variable_codes) %>%
          dplyr::filter(code_type == "TO_CHECK" &
                          !stringr::str_detect(code_id, "^3"))
      }
    ),
    # RESIDUAL
    list(
      desc = "RESIDUAL-Codes haben code_id = 0",
      print_cols = c("unit_key", "variable_id", "code_type", "code_id", "link"),
      code = function() {
        data_filter_base_no_value %>%
          tidyr::unnest(variable_codes) %>%
          dplyr::filter(code_type %in% c("RESIDUAL","RESIDUAL_AUTO") &
                          code_id != 0)
      }
    ),
    # SUM_CODE
    list(
      desc = "Keine Ableitungen vom Typ SUM_CODE",
      print_cols = c("unit_key", "variable_id", "variable_source_type", "link"),
      code = function() {
        data_filter_base_no_value %>%
          dplyr::filter(variable_source_type == "SUM_CODE")
      }
    ),
    # NUMERIC_MATCH
    list(
      desc = "NUMERIC_MATCH nur numerisch oder newline",
      print_cols = c("unit_key", "variable_id", "rule_parameter", "link"),
      code = function() {
        data_filter_base_no_value %>%
          tidyr::unnest(variable_codes) %>%
          dplyr::filter(
            rule_method == "NUMERIC_MATCH" &
              !stringr::str_detect(rule_parameter, "^[0-9]+(\\.[0-9]+)?$") &
              !stringr::str_detect(rule_parameter, "\n")
          )
      }
    ),
    # '_'-Prefix Variablen
    list(
      desc = "Variablen mit '_'-Prefix haben keine Kodierung",
      print_cols = c("unit_key", "variable_id", "rule_parameter", "link"),
      code = function() {
        data_filter_base_no_value %>%
          tidyr::unnest(variable_codes) %>%
          dplyr::filter(
            stringr::str_detect(variable_id, "^_[0-9]+") &
              rule_parameter != "" &
              !stringr::str_detect(variable_id, "ggb")
          )
      }
    )
  )



  # Ausgabe-Helfer -------------------------------------------------------------
  colour_scheme <- function(text_block, colour, result = NULL) {
    if (colour == "gelb") {
      cat(crayon::yellow(paste0("-> ", text_block)), "\n")
    } else if (colour == "blau") {
      cat(crayon::blue(paste0("== ", text_block, " ==")), "\n")
    } else if (colour == "rot" && !is.null(result)) {
      cat(crayon::red(glue::glue("X {nrow(result)} {text_block}")), "\n")
    } else if (colour == "weiss") {
      cat(text_block, "\n")
    } else if (colour == "gruen") {
      cat(crayon::green(glue::glue("OK {text_block}")), "\n")
    }
  }
  


  # Testthat-reporter auf "silent" setzen, damit er keinen unerwuenschten output generiert
  testthat::set_reporter("silent")


  # Test-Runner ----------------------------------------------------------------
  run_test <- function(desc, print_cols, code_block, test_nr, console) {

    # Test zur Liste aller Tests hinzufuegen
    all_tests <<- c(all_tests, desc)

    # Test ausfuehren
    result <- code_block()

    # Ausnahmen filtern
    exc_units <- exceptions[[as.character(test_nr)]]
    if (!is.null(exc_units) && nrow(result) > 0) {
      result <- result %>% dplyr::filter(!unit_key %in% exc_units)
    }


    # Testthat-Test
      tryCatch({
        testthat::test_that(desc, {
          testthat::expect_equal(nrow(result), 0)
        })

        # Fehler sammeln in error-results
      }, error = function(e) {
        df <- result %>%
          dplyr::distinct(.keep_all = TRUE) %>%
          dplyr::mutate(
            Test = desc,
            print_cols = list(print_cols)
          ) %>%
          dplyr::distinct(!!!rlang::syms(print_cols), .keep_all = TRUE)

        error_results <<- dplyr::bind_rows(error_results, df)
      })


      # Fehlertablle ausgeben, aber nur bei console & !name_list
      if (console && !name_list) {

        colour_scheme(glue::glue("{desc}"), "gelb")

        if(nrow(result) > 0) {
          colour_scheme(glue::glue("Fehler gefunden"), "rot", result)
          print(result %>% dplyr::select(dplyr::any_of(print_cols)) %>% dplyr::distinct(.keep_all = TRUE))
        } else {
          colour_scheme(glue::glue("Keine Fehler gefunden"), "gruen")
          }
          cat(strrep("_", 110), "\n\n")
        }
    }




  # Nur Testnamen zurueckgeben --------------------------------------------------
  if (name_list) {
    return(tibble::tibble(Nr = seq_along(tests), Testname = purrr::map_chr(tests, "desc")))
  }

  assert_cols(data, c("unit_key", "variable_id", "variable_source_type", "variable_codes"), "data")
  data_filter_base_no_value <- data %>%
    dplyr::filter(variable_source_type != "BASE_NO_VALUE")


  # Tests ausfuehren ------------------------------------------------------------
  if (console) {
    cat("\033[33m!! Deaktivierte Variablen werden IMMER ignoriert !!\033[0m\n\n")
  }
  purrr::walk2(tests, seq_along(tests), ~run_test(desc = .x$desc,
                                                  print_cols = .x$print_cols,
                                                  code_block =  .x$code,
                                                  test_nr = .y,
                                                  console = console))

  # Fehlerliste zurueckgeben ---------------------------------------------------

  status_table <- tibble::tibble(Test = all_tests)

  if (nrow(error_results) > 0) {

    error_counts <- error_results %>%
      dplyr::count(Test, name = "n")

    status_table <- status_table %>%
      dplyr::left_join(error_counts, by = "Test") %>%
      dplyr::mutate(
        Status = dplyr::if_else(
          is.na(n),
          "Test bestanden",
          paste0(n, " Fehler gefunden")
        )
      ) %>%
      dplyr::select(-n)

  } else {
    status_table <- status_table %>%
      dplyr::mutate(Status = "Test bestanden")
  }

  # Immer drucken, unabhaengig von console
  print(status_table)
  invisible(status_table)

  # Reactable-Ausgabe optional -------------------------------------------------
  if (!name_list && nrow(error_results) > 0) {
    tabs <- error_results %>%
      dplyr::group_split(Test) %>%
      purrr::map(function(df) {
        cols <- unique(unlist(df$print_cols))
        show_cols <- setdiff(cols, "Test")
        shiny::tabPanel(
          title = unique(df$Test),
          reactable::reactable(
            df %>% dplyr::select(dplyr::any_of(show_cols)),
            searchable = TRUE, filterable = TRUE, pagination = FALSE,
            bordered = TRUE, striped = TRUE,
            columns = list(
              link = reactable::colDef(
                cell = function(value) if (!is.na(value)) htmltools::a("Link", href = value, target = "_blank") else ""
              )
            )
          )
        )
      })

    page <- shiny::fluidPage(
      htmltools::tags$h2("Testfehler - Uebersicht"),
      do.call(shiny::tabsetPanel, tabs)
    )

    htmltools::save_html(page, "test_coding_schemes.html")
    # Browse nur interaktiv
    if(interactive() && !name_list && nrow(error_results) > 0) {
      utils::browseURL("test_coding_schemes.html")
    }
  }
}
