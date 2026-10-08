# Use the package's cli progress handlers without mixing progress with the
# analytical change report. Explicit IDs keep nested stages independent.
missing_progress_start <- function(name, total = NA_real_, enabled = TRUE,
                                   .envir = parent.frame()) {
  if (!enabled || identical(total, 0L) || identical(total, 0)) return(NULL)
  id <- cli::cli_progress_bar(name = name, total = total, current = FALSE,
    type = if (is.na(total)) "tasks" else "iterator", .envir = .envir)
  cli::cli_progress_update(id = id, inc = 0, force = is.na(total))
  id
}

missing_progress_update <- function(id, inc = 1L, status = NULL) {
  if (!is.null(id)) cli::cli_progress_update(id = id, inc = inc, status = status)
  invisible(NULL)
}

missing_progress_done <- function(id) {
  if (!is.null(id)) cli::cli_progress_done(id = id)
  invisible(NULL)
}
