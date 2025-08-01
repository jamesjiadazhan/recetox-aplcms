
#' @description
#' Sort tibble based on sample_names
#' @export
sort_data <- function(sample_names, feature_tables) {
  index <- c()
  for (i in seq_along(sample_names))
  {
    index <- append(index, feature_tables[[i]]$sample_id[1])
  }

  index <- match(sample_names, index)
  feature_tables <- feature_tables[index]

  return(feature_tables)
}

#' Compute clusters of mz and rt and assign cluster id to individual features.
#'
#' @description
#' Uses tolerances to group features with mz and rt within the tolerance into clusters,
#' creating larger features from raw data points. Custom tolerances for mz and rt are
#' computed based on the given parameters.
#' @param feature_tables list List of tibbles containing features.
#' Each tibble should have columns [mz, rt, area].
#' @param mz_tol_relative float Relative mz tolerance to use for grouping features.
#' @param mz_tol_absolute float Absolute mz tolerance to use for grouping features.
#' @param mz_max_diff float Maximum difference between featuure mz values to belong to the same cluster.
#' @param rt_tol_relative float Relative retention time tolerance to use for grouping features.
#' @param do.plot bool Plot graphics or not.
#' @param sample_names list List of sample names.
#' @return Returns a list with following items:
#' \itemize{
#'   \item feature_tables - list - Feature tables with added columns [sample_id, cluster].
#'   \item rt_tol_relative - float - Newly determined relative rt tolerance.
#'   \item mz_tol_relative - float - Newly determined relative mz tolerance.
#' }
#' @export
compute_clusters <- function(feature_tables,
                             mz_tol_relative,
                             mz_tol_absolute,
                             mz_max_diff,
                             rt_tol_relative,
                             do.plot,
                             sample_names = NA) {

  # print the head of feature_tables
  print(head(feature_tables))
  
  number_of_samples <- length(feature_tables)
  all <- concatenate_feature_tables(feature_tables, sample_names)

  if (is.na(mz_tol_relative)) {
    mz_tol_relative <- find_mz_tolerance(
      all$mz,
      mz_max_diff = mz_max_diff,
      aver.bin.size = 4000,
      min.bins = 50,
      max.bins = 200,
      do.plot = do.plot
    )
    if (length(mz_tol_relative) == 0) {
      mz_tol_relative <- 1e-5
      warning("Automatic tolerance finding failed, 10 ppm was assigned.
                        May need to manually assign alignment mz tolerance level.")
    }
  } else if (do.plot) {
    draw_plot(
      main = "m/z tolerance level given",
      label = mz_tol_relative
    )
  }

  if (!is.na(rt_tol_relative) && do.plot) {
    draw_plot(
      main = "retention time \n tolerance level given",
      label = rt_tol_relative
    )
  }

  aver.bin.size <- 200
  min.bins <- 50
  max.bins <- 100
  max.num.segments <- 10000

  features <- dplyr::arrange_at(all, "mz")
  min_mz_tol <- compute_min_mz_tolerance(
    features$mz,
    mz_tol_relative,
    mz_tol_absolute
  )

  mz_breaks <- compute_breaks_3(features$mz, min_mz_tol)
  features$mz_group <- 0

  for (i in 2:length(mz_breaks)) {
    subset_indices <- (mz_breaks[i - 1] + 1):mz_breaks[i]
    features$mz_group[subset_indices] <- i
  }

  features <- features |> dplyr::arrange_at(c("mz_group", "rt"))

  mz_breaks <- mz_breaks[c(-1, -length(mz_breaks))]

  if (is.na(rt_tol_relative)) {
    rt_tol_relative <- compute_rt_tol_relative(
      mz_breaks,
      max.num.segments,
      aver.bin.size,
      number_of_samples,
      features$rt,
      min.bins,
      max.bins
    )
  }

  # compute breaks in rt domain
  rt_diffs <- diff(features$rt)
  rt_breaks <- which(rt_diffs > rt_tol_relative)

  # combine indices of all breaks in array and sort
  all.breaks <- c(0, unique(c(mz_breaks, rt_breaks)), nrow(features))
  all.breaks <- all.breaks[order(all.breaks)]

  features$cluster <- 0
  for (i in 2:length(all.breaks)) {
    features$cluster[(all.breaks[i - 1] + 1):all.breaks[i]] <- i
  }

  message(paste("m/z tolerance level: ", mz_tol_relative))
  message(paste("time tolerance level:", rt_tol_relative))

  # Select features from individual samples, sort by mz and rt and
  # return the sorted tables as individual tibbles.
  feature_tables <- features |>
    dplyr::select(-mz_group) |>
    dplyr::group_by(sample_id) |>
    dplyr::arrange_at(c("mz", "rt")) |>
    dplyr::group_split()

  feature_tables <- sort_data(sample_names, feature_tables)

  return(list(feature_tables = feature_tables, rt_tol_relative = rt_tol_relative, mz_tol_relative = mz_tol_relative))
}

#' Compute clusters using simple grouping based on numeric thresholds.
#' 
#' @describtion
#' Features are first grouped in mz dimension based on the tolerance.
#' First, the absolute tolerance is computed for each feature, then a new group is started
#' once the difference between consecutive features is above this threshold.
#' The same process is then repeated for the retention time dimension.
#' The individual indices are then combined into a single index in the `cluster` columns.
#' @param feature_tables list of tibbles feature tables coming from all samples.
#' @param sample_names list of strings Sample names of the feature tables used to distinguish the samples.
#' @param mz_tol_ppm float Relative tolerance for mz grouping in parts per million.
#' @param rt_tol float Tolerance in retention time dimension [seconds].
#' @return list of tibbles Feature tables passed initially with additional columns indicating the 
#' mz and rt groups as well as the combined cluster index.
#' @export
compute_clusters_simple <- function(feature_tables, sample_names, mz_tol_ppm, rt_tol) {
  all <- concatenate_feature_tables(feature_tables, sample_names) |> dplyr::arrange_at("mz")

  mz_tol_rel <- mz_tol_ppm * 1e-06
  mz_tol_abs <- all$mz * mz_tol_rel

  all |>
    dplyr::mutate(mz_group = cumsum(c(0, diff(mz)) > mz_tol_abs)) |>
    dplyr::group_by(mz_group) |>
    dplyr::arrange_at("rt") |>
    dplyr::mutate(rt_group = cumsum(c(0, diff(rt)) > rt_tol)) |>
    dplyr::group_by(mz_group, rt_group) |>
    dplyr::mutate(cluster = cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::arrange_at("cluster") |>
    dplyr::group_by(sample_id) |>
    dplyr::group_split()
}


# compute_clusters_v2 <- function(feature_tables, mz_tol_ppm, rt_tol) {

#   mz_tol <- mz_tol_ppm * 1e-06

#   match_fun <- list(
#     ~ abs(.x - .y) < mz_tol * .x,
#     ~ abs(.x - .y) < rt_tol
#   )

#   join_and_update <- function(df_a, df_b) {
#     merged <- fuzzyjoin::fuzzy_ful.l_join(df_a, df_b, by=c("mz", "rt"), match_fun = match_fun) |>
#      dplyr::rename(mz = mz.x, rt = rt.x)
#   }

#   aligned <- purrr::reduce(feature_tables, join_and_update)
# }

#   multi_match_fun <- function(x, y) {
#     return(abs(x[,1] - y[,1]) < 10e-05 & abs(x[,2] - y[,2]) < 2)
#   }

#   f4 <- fuzzyjoin::fuzzy_full_join(f1, f2, by=NULL, multi_by=c("mz", "rt"), match_fun = NULL, multi_match_fun = multi_match_fun)
