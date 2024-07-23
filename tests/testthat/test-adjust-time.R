patrick::with_parameters_test_that(
  "compute template",
  {
    testdata <- file.path("..", "testdata")

    extracted <- read_parquet_files(files, "clusters", "_extracted_clusters.parquet")
    template_features <- compute_template(extracted)

    expected <- file.path(testdata, "template", "RCX_shortened.parquet")
    expected <- arrow::read_parquet(expected)

    expect_equal(template_features, expected)
  },
  patrick::cases(
    RCX_shortened = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened")
    )
  )
)

patrick::with_parameters_test_that(
  "correct time test",
  {
    testdata <- file.path("..", "testdata")

    template_features <- file.path(testdata, "template", "RCX_shortened.parquet")
    template_features <- arrow::read_parquet(template_features)

    extracted <- read_parquet_files(files, "clusters", "_extracted_clusters.parquet")

    corrected <- lapply(extracted, function(x){
      correct_time(x, template_features)
    })

    expected <- read_parquet_files(files, "adjusted", ".parquet")
    expect_equal(corrected, expected, tolerance = 0.01)
  },
  patrick::cases(
    RCX_shortened = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened")
    )
  )
)

test_that("compute_sel", {
  testdata <- file.path("..", "testdata")
  template_features <- file.path(testdata, "template", "RCX_shortened.parquet")
  template_features <- arrow::read_parquet(template_features)

  clustered_table <- read_parquet_files(
    c("RCX_06_shortened"),
    "clusters",
    "_extracted_clusters.parquet"
  )[[1]]

  all_features <- compute_template_adjusted_rt(
    compute_comb(template_features, clustered_table),
    compute_sel(combined),
    "RCX_07_shortened"
  )

  subsets <- template_features |>
    dplyr::bind_rows(clustered_table |> dplyr::select(c(mz, rt, cluster, sample_id))) |>
    dplyr::arrange_at(c("cluster","mz")) |>
    dplyr::group_by(cluster) |>
    mutate(count = n_distinct(sample_id)) |>
    filter(count == 2) |>
    add_count() |>
    filter(n == 2) |>
    ungroup() |>
    group_by(sample_id) |>
    group_split()

  all_features_new <- cbind(subsets[[1]]$rt, subsets[[2]]$rt)
  all_features_new_order <- order(all_features_new[,2])
  all_features_new_arranged <- all_features_new[all_features_new_order,]

  this.feature <- compute_corrected_features(
    clustered_table,
    all_features_new_arranged[, 2],
    all_features_new_arranged[, 1] - all_features_new_arranged[, 2]
  )

  browser()
})