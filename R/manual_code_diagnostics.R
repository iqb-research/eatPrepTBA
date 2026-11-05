# Check only identifiers, before manual records are filtered or serialized.
# The five additional joins run only when no manual record matches at all.
warn_unmatched_manual_codes <- function(codes_manual, responses, keys) {
  n_manual <- nrow(codes_manual)
  if (n_manual == 0L) return(invisible(NULL))

  manual_keys <- dplyr::select(codes_manual, dplyr::all_of(keys))
  response_keys <- dplyr::select(responses, dplyr::all_of(keys))
  unmatched <- dplyr::anti_join(manual_keys, response_keys, by = keys)
  n_unmatched <- nrow(unmatched)
  if (n_unmatched == 0L) return(invisible(NULL))

  message <- c(
    "{n_unmatched} of {n_manual} manual code records could not be matched to response rows with nonmissing payloads and will not be inserted.",
    "i" = "Matching keys: {.field {keys}}."
  )
  matches_without_key <- numeric()

  if (n_unmatched == n_manual) {
    # Several manual variables can share a response row. Count their records
    # without repeatedly joining identical identifiers or response payloads.
    unmatched_counts <- dplyr::count(
      unmatched, dplyr::across(dplyr::all_of(keys)), name = "n_manual"
    )
    response_keys <- dplyr::distinct(response_keys)
    matches_without_key <- vapply(keys, function(key) {
      candidates <- dplyr::semi_join(
        unmatched_counts, response_keys, by = setdiff(keys, key)
      )
      sum(candidates$n_manual)
    }, numeric(1))

    for (key in keys[matches_without_key > 0]) {
      n_candidates <- matches_without_key[[key]]
      message <- c(message, "i" = paste0(
        "Without {.field ", key, "}, ", n_candidates, " of ", n_manual,
        " manual code records would match the other four keys."
      ))
    }
    if (any(matches_without_key > 0)) {
      message <- c(message, "i" = paste0(
        "Compare identifier values, including missing values (NA) versus ",
        "empty strings (\"\")."
      ))
    } else {
      message <- c(message, "i" = paste0(
        "Omitting any single key still yields no matches. Check for multiple ",
        "identifier differences or unavailable response payloads."
      ))
    }
  }

  cli::cli_warn(
    message,
    class = "eatPrepTBA_unmatched_manual_codes",
    n_manual = n_manual,
    n_unmatched = n_unmatched,
    keys = keys,
    matches_without_key = matches_without_key
  )
  invisible(NULL)
}
