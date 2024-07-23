patrick::with_parameters_test_that(
  "feature.align test",
  {
    testdata <- file.path("..", "testdata")

    corrected_features <- read_parquet_files(files, "adjusted", ".parquet")
    
    res <- compute_clusters(
        corrected_features,
        mz_tol_relative,
        mz_tol_absolute,
        10 * mz_tol,
        rt_tol_relative,
        do.plot,
        files
    )
    
    aligned_actual <- create_aligned_feature_table(
        dplyr::bind_rows(res$feature_tables),
        min_occurrence,
        files,
        res$rt_tol_relative,
        res$mz_tol_relative,
        get_num_workers()
    )
    
    aligned_actual$mz_tol_relative <- res$mz_tol_relative
    aligned_actual$rt_tol_relative <- res$rt_tol_relative

    aligned_expected <- load_aligned_features(
      file.path(testdata, "aligned", "metadata_table.parquet"),
      file.path(testdata, "aligned", "intensity_table.parquet"),
      file.path(testdata, "aligned", "rt_table.parquet"),
      file.path(testdata, "aligned", "tolerances.parquet")
    )

    expect_equal(aligned_actual, aligned_expected)
  },
  patrick::cases(
    RCX_shortened = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened"),
      min_occurrence = 2,
      mz_tol_relative = NA,
      rt_tol_relative = NA,
      mz_tol = 1e-05,
      mz_tol_absolute = 0.01,
      do.plot = FALSE
    )
  )
)


patrick::with_parameters_test_that(
  "compute_aligned_feature_table test",
  {
    testdata <- file.path("..", "testdata")

    corrected_features <- read_parquet_files(files, "clusters", "_adjusted_clusters.parquet")

    aligned_actual <- create_aligned_feature_table(
        dplyr::bind_rows(corrected_features),
        min_occurrence,
        files,
        rt_tol_relative,
        mz_tol_relative,
        get_num_workers()
    )

    aligned_expected <- load_aligned_features(
      file.path(testdata, "aligned", "metadata_table.parquet"),
      file.path(testdata, "aligned", "intensity_table.parquet"),
      file.path(testdata, "aligned", "rt_table.parquet"),
      file.path(testdata, "aligned", "tolerances.parquet")
    )

    aligned_expected["mz_tol_relative"] <- NULL
    aligned_expected["rt_tol_relative"] <- NULL

    expect_equal(aligned_actual, aligned_expected)
  },
  patrick::cases(
    RCX_shortened = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened"),
      min_occurrence = 2,
      mz_tol_relative = 6.85676325338646e-06,
      rt_tol_relative = 2.17918873407775
    )
  )
)
