prepare_codebook_score_example <- function() {
  testthat::local_mocked_bindings(
    download_codebook = function(workspace, path, show_score, ...) {
      expect_true(show_score)
      make_code <- function(id, score) {
        code <- list(id = id, label = paste("Code", id), description = "Description")
        if (!missing(score)) code["score"] <- list(score)
        code
      }
      unit <- list(
        key = "U1", name = "Unit 1",
        variables = list(
          list(id = "text", label = "Text scores",
               codes = list(make_code("1", "1"), make_code("2", "0.5"),
                            make_code("3", ""), make_code("4", "not scored"))),
          list(id = "numeric", label = "Numeric scores",
               codes = list(make_code("1", 1), make_code("0", 0), make_code("2", 0.5))),
          list(id = "absent", label = "No score field", codes = list(make_code("1"))),
          list(id = "null", label = "Null score", codes = list(make_code("1", NULL)))
        ),
        missings = list(list(code = -99, label = "Profile missing", description = "From Studio"))
      )
      jsonlite::write_json(list(unit), file.path(path, "codebook.json"),
                          auto_unbox = TRUE, null = "null")
    }, .package = "eatPrepTBA"
  )
  prepare_codebook(
    fake_studio_workspace(), show_score = TRUE, missings_profile = "Profile",
    missings = tibble::tibble(id = "-97", label = "Own missing", description = "From user")
  )
}

test_that("scores remain scalar text with NA for profile and user missing codes", {
  out <- prepare_codebook_score_example()
  expect_type(out$score, "character")
  expect_equal(out$score[out$variable_id == "text"],
               c("1", "0.5", "", "not scored", NA_character_, NA_character_))
  expect_equal(out$score[out$variable_id == "numeric"],
               c("1", "0", "0.5", NA_character_, NA_character_))
  expect_true(all(is.na(out$score[out$variable_id %in% c("absent", "null")])))
  expect_equal(nrow(out), 17L)
  expect_false(any(vapply(out, is.list, logical(1))))
})

test_that("scores are scalar without appended missing codes too", {
  for (score in list("1", 1, NULL)) {
    code <- list(id = "1", label = "Correct", description = "Right", score = score)
    out <- eatPrepTBA:::prepare_codebook_codes(code, missing_codes = NULL)
    expect_identical(out$score, if (is.null(score)) NA_character_ else "1")
  }
})

test_that("score-free exports keep their existing columns", {
  code <- list(id = "1", label = "Correct", description = "Right")
  missing <- list(list(id = "-99", label = "Missing", description = "Missing code"))
  out <- eatPrepTBA:::prepare_codebook_codes(list(code), missing_codes = missing)
  expect_named(out, c("code_id", "code_label", "code_description"))
})

test_that("codebooks with scores and missing codes can be exported to Excel", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  out <- prepare_codebook_score_example()
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)

  expect_no_error(writexl::write_xlsx(out, path))
  restored <- readxl::read_xlsx(path, col_types = "text")
  # Empty strings and NA both appear as blank cells in Excel.
  expected <- out
  expected$score[expected$score == "" & !is.na(expected$score)] <- NA_character_
  expect_equal(restored, expected)
})
