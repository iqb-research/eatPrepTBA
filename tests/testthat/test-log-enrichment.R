test_that("add_unit_sizes joins compute_sizes output and derives loadtime ratios", {
  files <- tibble::tibble(
    type = c("Unit", "Unit", "Booklet", "Testtakers", "Resource"),
    name = c("U1.xml", "U2.xml", "B1.xml", "TT.xml", "res.vocs"),
    size = c(10, 20, 5, 3, 2),
    dependencies = list(
      list(list(relationship_type = "isDefinedBy", object_name = "U1.xml")),
      list(list(relationship_type = "isDefinedBy", object_name = "U2.xml")),
      list(list(relationship_type = "uses", object_name = "res.vocs")),
      list(),
      list()
    )
  )
  sizes <- compute_sizes(files)
  unit_data <- tibble::tibble(
    unit_key = c("U1", "U2", "U3"),
    unit_loadtime = c(1000, 2000, 3000),
    unit_playbacks = list(
      tibble::tibble(unit_loadtime_i = c(100, 150)),
      tibble::tibble(unit_loadtime_i = c(200, NA_real_)),
      tibble::tibble(unit_loadtime_i = 300)
    )
  )

  out <- add_unit_sizes(unit_data, sizes)
  u1 <- out[out$unit_key == "U1", ]
  u3 <- out[out$unit_key == "U3", ]

  expect_equal(u1$unit_size_name, "U1.xml")
  expect_equal(u1$unit_size_bytes, 10)
  expect_equal(u1$unit_size_mb, 10 / 1024^2)
  expect_true(u1$unit_size_available)
  expect_equal(u1$unit_n_valid_loadtimes, 2)
  expect_equal(u1$unit_first_loadtime, 100)
  expect_equal(u1$unit_median_loadtime, 125)
  expect_equal(u1$unit_loadtime_per_mb, 1000 / (10 / 1024^2))
  expect_equal(u1$unit_first_loadtime_per_mb, 100 / (10 / 1024^2))

  expect_false(u3$unit_size_available)
  expect_true(is.na(u3$unit_size_bytes))
  expect_true(is.na(u3$unit_loadtime_per_mb))
})

test_that("summarise_system_checks detects network metrics in wide get_system_checks-like data", {
  system_checks <- tibble::tibble(
    groupname = c("G1", "G1"),
    loginname = c("L1", "L1"),
    code = c("C1", "C1"),
    Browser = c("Firefox", "Firefox"),
    downlink = c("12.5", "12.5"),
    rtt = c("50", "50"),
    effectiveType = c("4g", "4g"),
    id = c("V1", "V2"),
    value = c("A", "B")
  )

  out <- summarise_system_checks(system_checks)

  expect_equal(nrow(out), 1)
  expect_equal(out$group_id, "G1")
  expect_equal(out$login_name, "L1")
  expect_equal(out$n_system_check_rows, 2)
  expect_equal(out$n_response_variables, 2)
  expect_true(out$has_network_metrics)
  expect_equal(out$system_check_download_value, "12.5")
  expect_equal(out$system_check_rtt_value, "50")
  expect_equal(out$system_check_effective_type, "4g")
  expect_s3_class(out$system_check_network_metrics[[1]], "tbl_df")
})

test_that("summarise_system_checks detects network metrics in long read_system_checks-like data", {
  system_checks <- tibble::tibble(
    Name = c("check1", "check1", "check1"),
    variable_id = c("download_speed", "upload_speed", "other"),
    value = c("20", "5", "x"),
    status = "VALUE_CHANGED"
  )

  out <- summarise_system_checks(system_checks)

  expect_equal(out$Name, "check1")
  expect_equal(out$n_system_check_rows, 3)
  expect_equal(out$n_network_metric_values, 2)
  expect_true(out$has_network_metrics)
  expect_equal(out$system_check_download_value, "20")
  expect_equal(out$system_check_upload_value, "5")
})

test_that("add_system_check_summary joins only by explicit or shared keys", {
  log_qc <- tibble::tibble(
    group_id = c("G1", "G2"),
    login_name = c("L1", "L2"),
    login_code = c("C1", "C2"),
    log_qc_flag = c("ok", "warning")
  )
  system_summary <- summarise_system_checks(
    tibble::tibble(
      groupname = "G1",
      loginname = "L1",
      login_code = "C1",
      variable_id = "download_speed",
      value = "20"
    )
  )

  out <- add_system_check_summary(log_qc, system_summary)

  expect_equal(out$n_system_check_rows[out$login_name == "L1"], 1)
  expect_true(out$has_network_metrics[out$login_name == "L1"])
  expect_true(is.na(out$n_system_check_rows[out$login_name == "L2"]))

  expect_error(
    add_system_check_summary(
      tibble::tibble(x = 1),
      tibble::tibble(y = 1)
    ),
    "Must have length"
  )
})
