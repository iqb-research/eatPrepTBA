# One display belongs to a whole operation. Nested helpers only replace its
# current phase; they never create competing bars or restart the overall clock.
.missing_progress_state <- new.env(parent = emptyenv())

missing_progress_clock <- function() proc.time()[[3L]]

missing_progress_backend <- function() {
  if (interactive() && .Platform$OS.type == "windows" &&
      identical(.Platform$GUI, "Rgui")) return("windows")
  if (cli::is_dynamic_tty()) "terminal" else "text"
}

# Wrappers keep Windows-only calls out of other platforms and make the actual
# lifecycle (including a user closing the window) testable without a desktop.
missing_progress_window_open <- function(label) {
  getExportedValue("utils", "winProgressBar")(
    title = "eatPrepTBA", label = label, min = 0, max = 1,
    initial = 0, width = 700L)
}

missing_progress_window_update <- function(handle, value, label) {
  getExportedValue("utils", "setWinProgressBar")(handle, value, label = label)
}

missing_progress_window_close <- function(handle) close(handle)

missing_progress_defer <- function(callback, envir) {
  do.call(base::on.exit, list(as.call(list(callback)), TRUE, TRUE), envir = envir)
}

missing_progress_session_start <- function(enabled = TRUE) {
  if (!enabled) return(NULL)
  active <- .missing_progress_state$active
  if (!is.null(active) && !active$closed) {
    return(list(session = active, owner = FALSE))
  }
  session <- new.env(parent = emptyenv())
  session$backend <- missing_progress_backend()
  session$started <- missing_progress_clock()
  session$stages <- list()
  session$current <- NULL
  session$handle <- NULL
  session$closed <- FALSE
  session$line_width <- 0L
  .missing_progress_state$active <- session
  list(session = session, owner = TRUE)
}

missing_progress_session_done <- function(token) {
  if (is.null(token) || !token$owner || token$session$closed) return(invisible(NULL))
  session <- token$session
  session$closed <- TRUE
  for (stage in session$stages) stage$done <- TRUE
  if (!is.null(session$handle)) {
    tryCatch(missing_progress_window_close(session$handle), error = function(e) NULL)
  }
  if (session$backend == "terminal" && session$line_width > 0L) {
    cat("\n", file = stderr())
  }
  if (identical(.missing_progress_state$active, session)) {
    .missing_progress_state$active <- NULL
  }
  invisible(NULL)
}

missing_progress_start <- function(name, total = NA_real_, enabled = TRUE,
                                   .envir = parent.frame()) {
  if (!enabled || identical(total, 0L) || identical(total, 0)) return(NULL)
  token <- missing_progress_session_start()
  id <- new.env(parent = emptyenv())
  id$session <- token$session
  id$name <- name
  id$total <- total
  id$current <- 0
  id$status <- NULL
  id$started <- missing_progress_clock()
  id$done <- FALSE
  id$active <- TRUE
  id$next_draw <- -Inf
  if (!is.null(id$session$current)) id$session$current$active <- FALSE
  id$session$current <- id
  id$session$stages <- c(id$session$stages, list(id))
  # Finalise the real count even if a helper relies solely on on.exit(). A
  # partial count remains partial when its caller fails or is interrupted.
  missing_progress_defer(function() {
    missing_progress_done(id)
    if (token$owner) missing_progress_session_done(token)
  }, .envir)
  missing_progress_render(id, force = TRUE)
  id
}

missing_progress_update <- function(id, inc = 1L, status = NULL) {
  if (is.null(id) || id$done) return(invisible(NULL))
  id$current <- id$current + inc
  if (!is.null(status)) id$status <- status
  # This hot path runs once per person/unit. Keep counting cheap and check the
  # clock every time, so a single slow iteration is still followed by an update.
  # Rendering, formatting and stack inspection only happen when a draw is due.
  if (id$active) {
    now <- missing_progress_clock()
    if (now >= id$next_draw) missing_progress_render(id, now = now)
  }
  invisible(NULL)
}

missing_progress_label <- function(id, now) {
  count <- if (is.finite(id$total)) {
    sprintf(" (%s/%s; %.0f%%)", id$current, id$total,
            100 * min(1, id$current / id$total))
  } else ""
  status <- if (is.null(id$status) || !nzchar(id$status)) "" else paste0(": ", id$status)
  paste0(id$name, count, status, " | phase ",
         sprintf("%.1f s", max(0, now - id$started)), " | total ",
         sprintf("%.1f s", max(0, now - id$session$started)))
}

missing_progress_render <- function(id, force = FALSE, now = missing_progress_clock()) {
  session <- id$session
  if (id$done || session$closed || !id$active) return(invisible(NULL))
  interval <- if (session$backend == "text") 5 else 0.25
  if (!force && now < id$next_draw) return(invisible(NULL))
  id$next_draw <- now + interval
  label <- missing_progress_label(id, now)
  fraction <- if (is.finite(id$total)) min(1, max(0, id$current / id$total)) else 0
  if (session$backend == "windows") {
    shown <- tryCatch({
      if (is.null(session$handle)) session$handle <- missing_progress_window_open(label)
      missing_progress_window_update(session$handle, fraction, label)
      TRUE
    }, error = function(e) FALSE)
    if (shown) return(invisible(NULL))
    # A closed or unavailable progress window must never interrupt real work.
    if (!is.null(session$handle)) {
      tryCatch(missing_progress_window_close(session$handle), error = function(e) NULL)
    }
    session$handle <- NULL
    session$backend <- "text"
    id$next_draw <- now + 5
  }
  if (session$backend == "terminal") {
    width <- max(20L, getOption("width", 80L) - 1L)
    if (nchar(label) > width) label <- paste0(substr(label, 1L, width - 3L), "...")
    cat("\r", label, strrep(" ", max(0L, session$line_width - nchar(label))),
        sep = "", file = stderr())
    session$line_width <- nchar(label)
    utils::flush.console()
  } else {
    message(label)
  }
  invisible(NULL)
}

missing_progress_drop <- function(id) {
  if (is.null(id) || id$done) return(invisible(NULL))
  id$done <- TRUE
  id$active <- FALSE
  session <- id$session
  session$stages <- Filter(function(stage) !identical(stage, id), session$stages)
  session$current <- if (length(session$stages)) session$stages[[length(session$stages)]] else NULL
  if (!is.null(session$current)) session$current$active <- TRUE
  invisible(NULL)
}

missing_progress_done <- function(id) {
  if (is.null(id) || id$done || id$session$closed) return(invisible(NULL))
  # Force a fresh clock reading, never cli's cached initial elapsed time. Keep
  # the real count even if called by on.exit() during an interrupted loop.
  missing_progress_render(id, force = TRUE)
  missing_progress_drop(id)
  if (!is.null(id$session$current)) {
    missing_progress_render(id$session$current, force = TRUE)
  }
  invisible(NULL)
}
