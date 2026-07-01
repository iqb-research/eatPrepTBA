test_that("estimate_audio_video_plays extracts and combines audio and video entries", {
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
          list(id = "other_id", status = "ok", value = 99)
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
    "id", "status", "n_plays", "media_unit_ids"
  ) %in% names(out)))

  expect_equal(sort(out$id), c("audio_clip_1", "video_clip_1", "video_clip_2"))
  expect_equal(out$n_plays[out$id == "audio_clip_1"], 2)
  expect_equal(out$n_plays[out$id == "video_clip_1"], 1)
  expect_equal(out$n_plays[out$id == "video_clip_2"], 3)
  expect_equal(
    sort(out$media_unit_ids),
    sort(c("U1 audio_clip_1", "U1 video_clip_1", "U2 video_clip_2"))
  )
})

test_that("estimate_audio_video_plays ignores NA and non-media responses", {
  response_df <- tibble::tibble(
    group_id = c("G1", "G1", "G2"),
    login_name = c("L1", "L1", "L2"),
    login_code = c("C1", "C1", "C2"),
    booklet_id = c("B1", "B1", "B2"),
    unit_key = c("U1", "U1", "U3"),
    page_no = c(1L, 2L, 1L),
    responses = c(
      NA_character_,
      jsonlite::toJSON(
        list(list(id = "text_input_1", status = "ok", value = 7)),
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
  expect_equal(out$id, "audio_clip_9")
  expect_equal(out$n_plays, 4)
  expect_equal(out$media_unit_ids, "U3 audio_clip_9")
})
