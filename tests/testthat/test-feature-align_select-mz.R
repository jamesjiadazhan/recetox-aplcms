test_that("select_mz function works", {
  sample <- read_parquet("../testdata/input/feature-align_select-mz.parquet")
  sample_names <- c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened")
  min_occurrence <- 2
  mz_tol_relative <- 6.85676325338646e-06
  rt_tol_relative <- 2.17918873407775

  actual <- select_mz(sample,
                      mz_tol_relative,
                      rt_tol_relative,
                      min_occurrence,
                      sample_names)
  expected <- readRDS("../testdata/aligned/output_select-mz.rds")
  expect_equal(actual, expected)
})