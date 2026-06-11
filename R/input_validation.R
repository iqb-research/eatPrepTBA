assert_cols <- function(x, cols, arg) {
  missing_cols <- setdiff(cols, names(x))

  if (length(missing_cols) > 0L) {
    stop(
      paste0(
        "'", arg, "' must contain the columns {",
        paste0(cols, collapse = ", "),
        "}, but is missing the column(s): {",
        paste0(missing_cols, collapse = ", "),
        "}."
      ),
      call. = FALSE
    )
  }

  invisible(x)
}

assert_attrs <- function(x, attrs, arg) {
  missing_attrs <- setdiff(attrs, names(attributes(x)))

  if (length(missing_attrs) > 0L) {
    stop(
      paste0(
        "'", arg, "' must contain the attributes {",
        paste0(attrs, collapse = ", "),
        "}, but is missing the attribute(s): {",
        paste0(missing_attrs, collapse = ", "),
        "}."
      ),
      call. = FALSE
    )
  }

  invisible(x)
}

assert_existing_files <- function(files, arg = "files") {
  checkmate::assert_character(files, min.len = 1)

  missing_files <- files[!file.exists(files)]

  if (length(missing_files) > 0L) {
    stop(
      paste0(
        "'", arg, "' must point to existing files, but the following path(s) do not exist: {",
        paste0(missing_files, collapse = ", "),
        "}."
      ),
      call. = FALSE
    )
  }

  invisible(files)
}
