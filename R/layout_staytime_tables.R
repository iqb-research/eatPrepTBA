#' Sets and layouts quantile tables of stay times
#' 
#' Author: Philipp Franikowski, restructuring by Lea Musiolek
#' 
#' @param data description
#' @param id description
#' @param subject description
#' @param filterable description
#' @param searchable description
#' @param sortable description
#' @param views description
#' @param download description
#'
#' @return Tables, including quantile dot plots, ready for using in quarto document
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Function for rendering pre-existing quantile tables of unit, page and item
#' stay times into a shape and layout suitable for a quarto document.
#' Attention! Dataset needs to be called "data" (Philipp).
#'
#' @export

layout_staytime_tables <- function(data,
                               id = "unit-table",
                               subject = "dep",
                               filterable = TRUE,
                               searchable = TRUE,
                               sortable = TRUE,
                               views = TRUE,
                               download = NULL) {
  
  unit_cols <- colUnit(data)

  columns <- c(
    unit_cols,
    if (id == "item-table") colPage,
    colNoShow
  )
  
  # unit_cols <- colUnit(data_table)
  
  columns_filter <-
    columns %>% keep(imap_lgl(., function(x, i) i %in% names(data)))
  
  # if (subject == "dep") {
  #   diff_group <- c("itemP", "itemP_RS", "itemP_FS", "est", "se", "Geschätzte_Schwierigkeit")
  #
  #   download_columns <- c("link_legacy", "item", "Nvalid", "Nvalid_RS", "Nvalid_FS",
  #                         "itemP", "itemP_RS", "itemP_FS", "itemDiscrim", "est", "se",
  #                         "infit", "outfit", "Itemformat", "Geschätzte_Schwierigkeit", "Anforderungsbereich",
  #                         "Bildungsstandard", "SPF", "q3_n",
  #                         "flag")
  # } else {
  #   diff_group <- c("itemP", "itemP_RS", "itemP_FS", "est__g", "est", "se__g", "se", "Geschätzte_Schwierigkeit")
  #
  #   download_columns <- c("link_legacy", "item", "Nvalid", "Nvalid_RS", "Nvalid_FS",
  #                         "itemP", "itemP_RS", "itemP_FS", "itemDiscrim",
  #                         "est", "se", "est__g", "se__g",
  #                         "infit", "outfit", "infit__g", "outfit__g",
  #                         "Itemformat", "Geschätzte_Schwierigkeit", "Anforderungsbereich",
  #                         "Bildungsstandard", "SPF", "q3_n",
  #                         "flag")
  # }
  
  # diff_group <- intersect(
  #   names(data),
  #   diff_group
  # )
  
  group_item <- NULL
  if (id == "item-table") {
    group_item <-
      list(
        reactable::colGroup(name = "Item (Global)", columns = c("page_median", "page_q90", "page_q95")),
        reactable::colGroup(name = "Item (Regel)", columns = c("page_median_RS", "page_q90_RS", "page_q95_RS")),
        reactable::colGroup(name = "Item (SPF)", columns = c("page_median_FS", "page_q90_FS", "page_q95_FS"))
      )
  }
  
  table <-
    reactable(
      data,
      columnGroups = c(
        list(
          reactable::colGroup(name = "Unit (Global)", columns = c("unit_median", "unit_q90", 
                                                                  "unit_q95", "unit_diff", "unit_diff95")),
          reactable::colGroup(name = "Unit (Regel)", columns = c("unit_median_RS", "unit_q90_RS", 
                                                                 "unit_q95_RS", "unit_diff_RS", "unit_diff95_RS")),
          reactable::colGroup(name = "Unit (SPF)", columns = c("unit_median_FS", "unit_q90_FS", 
                                                               "unit_q95_FS", "unit_diff_FS", "unit_diff95_FS"))
        ),
        group_item
      ),
      columns = columns_filter,
      elementId = id,
      searchable = searchable,
      filterable = filterable,
      sortable = sortable,
      showPageSizeOptions = TRUE,
      defaultPageSize = 10,
      pageSizeOptions = c(10, 50, 100, 900),
      style = list("fontFamily" = "Open Sans", width = "100%")#,
      #      language = reactable_language_settings
    )
  
  # if (views) {
  #   filter_parameters <- c("outfit",
  #                          "se",
  #                          "outfit__g",
  #                          "se__g")
  #
  #   show_parameters <- intersect(filter_parameters, names(data))
  #
  #   filter_meta <- c("Geschätzte_Schwierigkeit",
  #                    "Anforderungsbereich",
  #                    "Bildungsstandard",
  #                    "SPF",
  #                    "innovation",
  #                    "unit_label")
  #
  #   if (subject == "dep") {
  #     filter_meta <- c(filter_meta, "innovation_unit")
  #   }
  #
  #   show_meta <- intersect(filter_meta, names(data))
  
  filter_design <- c("unit_median_RS",
                     "unit_q90_RS",
                     "unit_q95_RS",
                     "unit_diff_RS",
                     "unit_median_FS",
                     "unit_q90_FS",
                     "unit_q95_FS",
                     "unit_diff_FS"
  )
  
  if (id == "item-table") {
    filter_design <- c(
      filter_design,
      "page_median_RS",
      "page_q90_RS",
      "page_q95_RS",
      "page_median_FS",
      "page_q90_FS",
      "page_q95_FS"
    )
  }
  
  show_design <- intersect(filter_design, names(data))
  
  # Darstellung mit Checkboxen
  htmltools::browsable(
    div(
      # div(
      #   style = "display: inline-block; margin-right: 10px;",
      #   download_button(download = download,
      #                   id = id,
      #                   columns = download_columns),
      # ),
      # div(
      #   style = "display: inline-block; margin-right: 10px;",
      #   generate_checkbox(label = "Zeige Item-Metadaten",
      #                     checked = FALSE,
      #                     id = id,
      #                     columns = show_meta),
      # ),
      # div(
      #   style = "display: inline-block; margin-right: 10px;",
      #   generate_checkbox(label = "Zeige alle Kennwerte",
      #                     checked = FALSE,
      #                     id = id,
      #                     columns = show_parameters),
      # ),
      div(
        style = "display: inline-block; margin-right: 10px;",
        generate_checkbox(label = "Zeige Teildesign (SPF)",
                          checked = FALSE,
                          id = id,
                          columns = show_design,
                          filter_column = list(list(id = "SPF", value = "ja"))
        ),
      ),
      table
    )
  )
}

# download_button <- function(id, columns, download) {
#   if (is.null(download)) {
#     return(NULL)
#   }
#   columns_json <- jsonlite::toJSON(columns)
#   callback <- glue::glue("Reactable.downloadDataCSV('{id}', '{download}.csv', {{columnIds: {columns_json}, sep: ';', dec: ','}})")
#   htmltools::browsable(tags$button(shiny::icon("download"), "Herunterladen", onclick = callback))
# }

# # Filterfunktionen (allgemein)
# filter_multiple <- htmlwidgets::JS("function(rows, columnId, filterValue) {
#   if (typeof filterValue === 'string') {
#     // Split comma-separated values, trim spaces, and convert to lowercase
#     filterValue = filterValue.split(',').map(value => value.trim().toLowerCase());
#   }
# 
#   // Proceed with filtering rows based on case-insensitive partial matches
#   return rows.filter(row => {
#     const cellValue = String(row.values[columnId]).toLowerCase();
#     return filterValue.some(filterText => cellValue.includes(filterText));
#   });
# }")

# filter_min <- htmlwidgets::JS("function(rows, columnId, filterValue) {
#         return rows.filter(function(row) {
#           return isNaN(filterValue) || row.values[columnId] >= Number(filterValue)
#         })
#       }")

# filter_input_slider <- function(id, min = NULL, max = NULL, step = .001) {
#   function(values, name) {
#     oninput <- stringr::str_glue("Reactable.setFilter('{id}', '{name}', this.value)")
#     
#     min_val <- ifelse(is.null(min), min(values, na.rm = TRUE), min)
#     max_val <- ifelse(is.null(max), max(values, na.rm = TRUE), max)
#     
#     div(
#       style = htmltools::css(
#         display = "flex",
#         alignItems = "center",
#         justifyContent = "center",
#         height = "100%"
#       ),
#       tags$input(
#         style = htmltools::css(
#           width = "90%"
#         ),
#         type = "range",
#         min = min_val,
#         max = max_val,
#         step = step,
#         value = ifelse(length(values) == 0, min, min_val),
#         oninput = oninput,
#         onchange = oninput, # For IE11 support
#         "aria-label" = stringr::str_glue("Filter by minimum {name}")
#       ),
#       
#     )
#   }
# }

# # Links
# display_linkset <- function(value, index) {
#   if (!is.na(value)) {
#     link_icon_old <- shiny::icon("box-archive", lib = "font-awesome")
#     # 
#     # a(
#     #   link_icon_old, target = "_blank", href = value,
#     #   style = "color: #a8a29e;",
#     #   onmouseover = "this.style.color='#d6d3d1'",
#     #   onmouseout = "this.style.color='#a8a29e'"
#     # )
#   }
# }

# display_badge <- function(data, digits = 2, na = "-") {
#   function(value, index, name) {
#     if (is.na(value)) {
#       return(na)
#     }
#     
#     print_value <- printnum(value, digits = digits, gt1 = TRUE)
#     
#     badge <- status_badge(color = data[[index, stringr::str_glue("color_{name}")]])
#     badge_tool <- with_tooltip(badge, data[[index, stringr::str_glue("tooltip_{name}")]])
#     
#     shiny::tagList(badge_tool, print_value)
#   }
# }

# display_q3 <- function(data, id) {
#   function(value, index, name) {
#     if (length(has) > 0) {
#     } else {
#       div(value)
#     }
#   }
# }

to_stamp <- function(x) {
  if (is.na(x)) {
    return("-")
  }
  
  sign_char <- ifelse(x < 0, "-", "")
  abs_x <- abs(x)
  
  # Convert absolute value to period
  p <- abs_x %>%
    round() %>%
    as.integer() %>%
    lubridate::seconds_to_period()
  
  # Format as MM:SS
  time_str <- sprintf("%02d:%02d", lubridate::minute(p), 
                      lubridate::second(p))
  
  # Combine sign and time
  paste0(sign_char, time_str)
}

linkbox <- function(x) {
  shiny::icon("box-archive", lib = "font-awesome")
}

colUnit <- function(data) {
  list(
    link = reactable::colDef(
      name = "Links",
      width = 100,
      filterable = FALSE,
      sortable = FALSE,
      cell = linkbox
    ),
    unit_key = reactable::colDef(
      name = "Kurzname",
      style = sort_function,
      cell = function(value) htmltools::tags$code(value)
    ),
    unit_label = reactable::colDef(
      name = "Aufgabenbezeichnung",
      width = 350,
      style = sort_function
    ),
    unit_estimated = reactable::colDef(
      name = "a-priori",
      cell = to_stamp,
      style = sort_function
    ),
    unit_diff = reactable::colDef(
      name = "Differenz Q90",
      cell = to_stamp,
      style = sort_function
    ),
    unit_diff95 = reactable::colDef(
      name = "Differenz Q95",
      cell = to_stamp,
      style = sort_function
    ),
    
    # Globale Werte
    unit_median = reactable::colDef(
      name = "Median",
      cell = display_dotplot(data),
      style = sort_function,
      width = 400
    ),
    unit_q90 = reactable::colDef(
      name = "Q90",
      cell = to_stamp,
      style = sort_function
    ),
    unit_q95 = reactable::colDef(
      name = "Q95",
      cell = to_stamp,
      style = sort_function
    ),
    
    # Regelschulwerte
    unit_diff_RS = reactable::colDef(
      name = "Differenz Q90",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    ),
    unit_diff95_RS = reactable::colDef(
      name = "Differenz Q95",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    ),
    unit_median_RS = reactable::colDef(
      name = "Median",
      cell = display_dotplot(data, design = "RS"),
      style = sort_function,
      show = FALSE,
      width = 400
    ),
    unit_q90_RS = reactable::colDef(
      name = "Q90",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    ),
    unit_q95_RS = reactable::colDef(
      name = "Q95",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    ),
    
    # Förderschulwerte
    unit_diff_FS = reactable::colDef(
      name = "Differenz Q90",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    ),
    unit_diff95_FS = reactable::colDef(
      name = "Differenz Q95",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    ),
    unit_median_FS = reactable::colDef(
      name = "Median",
      cell = display_dotplot(data, design = "FS"),
      style = sort_function,
      show = FALSE,
      width = 400
    ),
    unit_q90_FS = reactable::colDef(
      name = "Q90",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    ),
    unit_q95_FS = reactable::colDef(
      name = "Q95",
      cell = to_stamp,
      show = FALSE,
      style = sort_function
    )
  )
}

display_dotplot <- function(data, design = NULL) {
  function(value, index) {
    if (is.na(value)) {
      return(value)
    }
    print_value <- to_stamp(value)
    prior <- data[[index, "unit_estimated"]]
    
    if (is.null(design)) {
      q90 <- data[[index, "unit_q90"]]
      q95 <- data[[index, "unit_q95"]]
      # tdiff <- data[[index, "unit_diff"]]
    } else {
      q90 <- data[[index, stringr::str_glue("unit_q90_{design}")]]
      q95 <- data[[index, stringr::str_glue("unit_q95_{design}")]]
      # tdiff <- data[[index, stringr::str_glue("unit_diff_{design}")]]
    }
    
    # color <- case_when(
    #   tdiff > 0 ~ "#fb7185",
    #   tdiff < -60 ~ "#0ea5e9",
    #   .default = "#34d399")
    
    div(
      style = list(display = "flex"),
      
      div(print_value, style = list(flex = "0 0 40px")),
      
      eatWidget::range_chart(
        est = prior,
        est_min = q90,
        est_max = q95,
        global_est = value,
        global_est_min = value,
        global_est_max = q90,
        width = 350,
        height = 20,
        min = 0,
        max = 20 * 60,
        color_line = "#bae6fd",
        global_color_line = "#0ea5e9",
        global_color = "#0ea5e9",
        global_fill = "#0ea5e9",
        fill = "#e2e8f0"
      )
    )
  }
}

generate_checkbox <- function(label, checked = NULL, id = "item-table", columns, filter_column = NULL) {
  filter_code <- ""
  if (!is.null(filter_column)) {
    filter_code <- glue::glue("
    if (!show) {{
      Reactable.setAllFilters(id, {jsonlite::toJSON(filter_column, auto_unbox = TRUE)});
    }}else {{
      Reactable.setAllFilters(id, []);
    }}")
  }
  
  js_code <- glue::glue("((e) => {{
    const show = !e.target.checked;
    const id = '{id}';
    console.log(Reactable.getState(id));
    const cols = {jsonlite::toJSON(columns, auto_unbox = TRUE)};
    cols.map(col => Reactable.toggleHideColumn(id, col, show));
    {filter_code}
    }})(event)")
  
  tags$input(
    label,
    type = "checkbox",
    checked = if (is.null(checked) || !checked) NULL else checked,
    onChange = htmltools::HTML(js_code)
  )
}

sort_function <- htmlwidgets::JS("function(rowInfo, column, state) {
  const {id} = column;
  const firstSorted = state.sorted[0]
  const validIds = ['domain', 'unit_key', 'unit_median', 'unit_label'];
  if (!firstSorted || validIds.includes(firstSorted.id)) {
    const prevRow = state.pageRows[rowInfo.viewIndex - 1]
    if (prevRow && rowInfo.values[id] === prevRow[id]) {
      return { visibility: 'hidden' }
    }
  }
}")

colPage <- list(
  variable_page = reactable::colDef(name = "Seite"),
  page_median = reactable::colDef(name = "Median", cell = to_stamp),
  page_q90 = reactable::colDef(name = "Q90", cell = to_stamp),
  page_q95 = reactable::colDef(name = "Q95", cell = to_stamp),
  page_median_RS = reactable::colDef(name = "Median", cell = to_stamp, show = FALSE),
  page_q90_RS = reactable::colDef(name = "Q90", cell = to_stamp, show = FALSE),
  page_q95_RS = reactable::colDef(name = "Q95", cell = to_stamp, show = FALSE),
  page_median_FS = reactable::colDef(name = "Median", cell = to_stamp, show = FALSE),
  page_q90_FS = reactable::colDef(name = "Q90", cell = to_stamp, show = FALSE),
  page_q95_FS = reactable::colDef(name = "Q95", cell = to_stamp, show = FALSE),
  item_id = reactable::colDef(name = "Item", style = sort_function,
                              cell = function(value) htmltools::tags$code(value),
                              width = 120)
)

no_show_list <- c(
  "SPF"
)

colNoShow <-
  no_show_list %>%
  purrr::map(function(x) reactable::colDef(show = FALSE)) %>%
  purrr::set_names(no_show_list)