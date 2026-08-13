test_that("estimate_audio_video_plays extracts audio and video entries", {
  response_df <- tibble::tibble(
    group_id = c("G1", "G1"),
    login_name = c("L1", "L1"),
    login_code = c("C1", "C1"),
    booklet_id = c("B1", "B1"),
    unit_key = c("U1", "U2"),
    page_no = c(1L, 2L),
    responses = c(
      jsonlite::toJSON(
        list(
          list(id = "audio_clip_1", status = "ok", value = 2),
          list(id = "video_clip_1", status = "ok", value = 1),
          list(id = "text_input_1", status = "mentions audio", value = 99)
        ),
        auto_unbox = TRUE
      ),
      jsonlite::toJSON(
        list(
          list(id = "video_clip_2", status = "ok", value = 3)
        ),
        auto_unbox = TRUE
      )
    )
  )

  out <- estimate_audio_video_plays(response_df)

  expect_s3_class(out, "tbl_df")
  expect_true(all(c(
    "group_id", "login_name", "login_code", "booklet_id", "unit_key", "page_no",
    "media_id", "media_type", "status", "n_plays", "media_key"
  ) %in% names(out)))

  expect_equal(sort(out$media_id), c("audio_clip_1", "video_clip_1", "video_clip_2"))
  expect_equal(out$n_plays[out$media_id == "audio_clip_1"], 2)
  expect_equal(out$n_plays[out$media_id == "video_clip_1"], 1)
  expect_equal(out$n_plays[out$media_id == "video_clip_2"], 3)
  expect_equal(
    sort(out$media_key),
    sort(c(
      "U1__media__audio__audio_clip_1",
      "U1__media__video__video_clip_1",
      "U2__media__video__video_clip_2"
    ))
  )
})

test_that("estimate_audio_video_plays ignores missing, malformed, and non-media responses", {
  response_df <- tibble::tibble(
    group_id = c("G1", "G1", "G2", "G3"),
    login_name = c("L1", "L1", "L2", "L3"),
    login_code = c("C1", "C1", "C2", "C3"),
    booklet_id = c("B1", "B1", "B2", "B3"),
    unit_key = c("U1", "U1", "U3", "U4"),
    page_no = c(1L, 2L, 1L, 1L),
    responses = c(
      NA_character_,
      "{not-json",
      jsonlite::toJSON(
        list(list(id = "text_input_1", status = "played audio", value = 7)),
        auto_unbox = TRUE
      ),
      jsonlite::toJSON(
        list(list(id = "audio_clip_9", status = "ok", value = 4)),
        auto_unbox = TRUE
      )
    )
  )

  out <- estimate_audio_video_plays(response_df)

  expect_equal(nrow(out), 1)
  expect_equal(out$media_id, "audio_clip_9")
  expect_equal(out$media_type, "audio")
  expect_equal(out$n_plays, 4)
  expect_equal(out$media_key, "U4__media__audio__audio_clip_9")
})

test_that("estimate_audio_video_plays returns a stable empty tibble", {
  response_df <- tibble::tibble(
    group_id = "G1",
    login_name = "L1",
    login_code = "C1",
    booklet_id = "B1",
    unit_key = "U1",
    responses = jsonlite::toJSON(
      list(list(id = "text_input_1", status = "ok", value = 1)),
      auto_unbox = TRUE
    )
  )

  out <- estimate_audio_video_plays(response_df)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
  expect_equal(
    names(out),
    c(
      "group_id", "login_name", "login_code", "booklet_id", "unit_key", "page_no",
      "media_id", "media_type", "status", "n_plays", "media_key"
    )
  )
})

test_that("estimate_audio_video_plays handles nested strings and single objects", {
  nested_response <- jsonlite::toJSON(
    jsonlite::toJSON(
      list(list(id = "audio_clip_nested", status = "ok", value = 5)),
      auto_unbox = TRUE
    ),
    auto_unbox = TRUE
  )
  single_response <- jsonlite::toJSON(
    list(id = "video_clip_single", status = "ok", value = 6),
    auto_unbox = TRUE
  )

  response_df <- tibble::tibble(
    group_id = c("G1", "G1"),
    login_name = c("L1", "L1"),
    login_code = c("C1", "C1"),
    booklet_id = c("B1", "B1"),
    unit_key = c("U1", "U1"),
    responses = c(nested_response, single_response)
  )

  out <- estimate_audio_video_plays(response_df)

  expect_equal(sort(out$media_id), c("audio_clip_nested", "video_clip_single"))
  expect_true(all(is.na(out$page_no)))
  expect_equal(out$n_plays[out$media_id == "audio_clip_nested"], 5)
  expect_equal(out$n_plays[out$media_id == "video_clip_single"], 6)
})
