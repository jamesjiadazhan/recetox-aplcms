#' @import foreach

#' Create an empty tibble for the next alignment step. It will contain three tables with aligned metadata, intensities an RTs.
#' @param number_of_samples Number
#'  of different sample names.
#' @param metadata_colnames Metadata column names: "id", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "npeaks", sample_names
#' @param intensity_colnames "id" and sample names; will hold intensities.
#' @param rt_colnames "id" and sample names; will hold retention times.
#' @return An empty tibble with slots for metadata, intensities and RTs.
#' @export
create_empty_tibble <- function(number_of_samples, metadata_colnames, intensity_colnames, rt_colnames) {
  features <- new("list")
  features$metadata <- tibble::as_tibble(matrix(nrow = 0, ncol = length(metadata_colnames)), .name_repair = ~metadata_colnames)
  features$intensity <- tibble::as_tibble(matrix(nrow = 0, ncol = length(intensity_colnames)), .name_repair = ~intensity_colnames)
  features$rt <- tibble::as_tibble(matrix(nrow = 0, ncol = length(rt_colnames)), .name_repair = ~rt_colnames)
  return(features)
}

create_metadata <- function(sample_grouped, sample_names) {
  sample_presence <- sapply(sample_names,
    FUN=function(x) {
      as.numeric(any(sample_grouped$sample_id == x))
    }
  )

  metadata_row <- dplyr::summarise(
    sample_grouped,
    mzmean = mean(mz),
    mzmin = min(mz),
    mzmax = max(mz),
    rtmean = mean(rt),
    rtmin = min(rt),
    rtmax = max(rt),
    npeaks = n()
  ) %>% rename(mz = "mzmean", rt = "rtmean")

  metadata_row <- dplyr::bind_cols(metadata_row, as.list(sample_presence))
  return(metadata_row)
}

first_tibble_row_as_vector <- function(x) {
  return(as.vector(unlist(x[1,])))
}

#' Create a list containing 3 tibbles: metadata, intensities and RTs.
#' @param sample_grouped A dataframe with grouped mz and RT values for a particular cluster.
#' @param sample_names A list of sample names.
#' @return A list containing 3 tibbles: metadata, intensities and RTs.
#' @export
create_output <- function(sample_grouped, sample_names) {
  metadata_row <- create_metadata(sample_grouped, sample_names)

  intensity_row <- sample_grouped %>%
   group_by(sample_id) %>%
   summarise(intensity = sum(area)) %>%
   pivot_wider(names_from = "sample_id", values_from = "intensity")


  rt_row <- sample_grouped %>%
   group_by(sample_id) %>%
   summarise(rt = median(rt)) %>%
   pivot_wider(names_from = "sample_id", values_from = "rt")
  
  return(list(
    metadata_row = (metadata_row),
    intensity_row = (intensity_row),
    rt_row = (rt_row)
  ))
}

#' Validates if the data is present in more than "min_occurence" of samples.
#' @param samples A subset of the features_table.
#' @param min_occurrence A minimal number of profiles a feature has to be present in.
#' @return boolean value whether it is TRUE or FALSE.
#' @export
validate_contents <- function(samples, min_occurrence) {
  if (!is.null(nrow(samples))) {
    if (length(unique(samples$sample_id)) >= min_occurrence) {
      return(TRUE)
    }
    return(FALSE)
  }
  return(FALSE)
}

#' Compute the kernel density estimation and find the peaks and valleys of a smooth curve.
#' @param data A vector of m/z or RTs for a particular cluster.
#' @param bandwidth A bandwidth value for the KDE computation.
#' @return A list of peaks and valleys positions.
#' @export
find_optima <- function(data, bandwidth) {
  den <- density(data, bw = bandwidth)
  turns <- find.turn.point(den$y)
  return(list(peaks = den$x[turns$pks], valleys = den$x[turns$vlys]))
}

#' Subset data within lower and upper bound from density estimation
#' @param sample A subset of the features_table.
#' @param turns A list of peaks and valleys positions.
#' @param index Whether it subsets on m/z [1] or RT [2] column.
#' @param i Iterates over the peaks in the turns list.
#' @return Dataframe subsetted within lower and upper bound from density estimation.
#' @export
filter_based_on_density <- function(sample, turns, index, i) {
  lower_bound <- max(turns$valleys[turns$valleys < turns$peaks[i]])
  upper_bound <- min(turns$valleys[turns$valleys > turns$peaks[i]])
  selected <- which(sample[, index] > lower_bound & sample[, index] <= upper_bound)
  return(sample[selected, ])
}

#' Groups the features across samples based on RT.
#' @param sample A dataframe subsetted for the particular cluster.
#' @param rt_tol_relative The retention time tolerance level for peak alignment.
#' @param min_occurence A minimal number of profiles a feature has to be present in.
#' @param sample_names A list of sample names.
#' @param return A list containing 3 tibbles: metadata, intensities and RTs.
#' @export
select_rt <- function(sample, rt_tol_relative, min_occurrence, sample_names) {
  turns <- find_optima(sample$rt, bandwidth = rt_tol_relative / 1.414)
  for (i in seq_along(turns$peaks)) {
    sample_grouped <- filter_based_on_density(sample, turns, 2, i)
    if (validate_contents(sample_grouped, min_occurrence)) {
      return(create_output(sample_grouped, sample_names))
    }
  }
}

#' Groups the features across samples based on m/z.
#' @param sample A dataframe subsetted for the particular cluster.
#' @param mz_tol_relative The m/z tolerance level for peak alignment.
#' @param rt_tol_relative The retention time tolerance level for peak alignment.
#' @param min_occurence A minimal number of profiles a feature has to be present in.
#' @param sample_names A list of sample names.
#' @return A list containing 3 tibbles: metadata, intensities and RTs.
#' @export
select_mz <- function(sample, mz_tol_relative, rt_tol_relative, min_occurrence, sample_names) {
  turns <- find_optima(sample$mz, bandwidth = mz_tol_relative * median(sample$mz))
  for (i in seq_along(turns$peaks)) {
    sample_grouped <- filter_based_on_density(sample, turns, 1, i)
    if (validate_contents(sample_grouped, min_occurrence)) {
      return(select_rt(sample_grouped, rt_tol_relative, min_occurrence, sample_names))
    }
  }
}

#' Groups the mz and RT for particular cluster.
#' @param features The features table subsetted for a particular cluster.
#' @param mz_tol_relative The m/z tolerance level for peak alignment.
#' @param rt_tol_relative The retention time tolerance level for peak alignment.
#' @param min_occurrence A minimal number of profiles a feature has to be present in.
#' @param sample_names A list of sample names.
#' @return A list containing 3 tibbles: metadata, intensities and RTs.
#' @export
create_rows <- function(features,
                        mz_tol_relative,
                        rt_tol_relative,
                        min_occurrence,
                        sample_names) {
  if (validate_contents(features, min_occurrence)) {
    return(select_mz(features, mz_tol_relative, rt_tol_relative, min_occurrence, sample_names))
  }
  return(NULL)
}

#' Combines the output (i.e. metadata, intensity and RT) from different clusters to one respective tibble.
#' @return Tibbles combining the output (metadata, intensity and RT respectively) from different clusters.
#' @export
comb <- function(x, ...) {
  mapply(tibble::as_tibble, (mapply(rbind, x, ..., SIMPLIFY = FALSE)))
}

#' Align peaks from spectra into a feature table.
#'
#' @param features_table A list object. Each component is a matrix which is the output from compute_clusters().
#' @param min_occurrence  A feature has to show up in at least this number of profiles to be included in the final result.
#' @param sample_names list List of sample names.
#' @param mz_tol_relative The m/z tolerance level for peak alignment. The default is NA, which allows the
#'  program to search for the tolerance level based on the data. This value is expressed as the
#'  percentage of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level.
#' @param rt_tol_relative The retention time tolerance level for peak alignment. The default is NA, which
#'  allows the program to search for the tolerance level based on the data.
#' @param cluster The number of CPU cores to be used
#' @return A tibble with three tables containing aligned metadata, intensities an RTs.
#'
#' @export
create_aligned_feature_table <- function(features_table,
                                         min_occurrence,
                                         sample_names,
                                         rt_tol_relative,
                                         mz_tol_relative,
                                         cluster = 4) {
  if (!is(cluster, "cluster")) {
    cluster <- parallel::makeCluster(cluster)
    on.exit(parallel::stopCluster(cluster))

    # NOTE: side effect (doParallel has no functionality to clean up)
    doParallel::registerDoParallel(cluster)
    register_functions_to_cluster(cluster)
  }

  number_of_samples <- length(sample_names)
  metadata_colnames <- c("id", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "npeaks", sample_names)
  intensity_colnames <- c("id", sample_names)
  rt_colnames <- c("id", sample_names)

  aligned_features <- create_empty_tibble(number_of_samples, metadata_colnames, intensity_colnames, rt_colnames)

  # table with number of values per group
  groups_cardinality <- table(features_table$cluster)
  # count those with minimal occurrence
  sel.labels <- as.numeric(names(groups_cardinality)[groups_cardinality >= min_occurrence])

  # retention time alignment

  aligned_features <- foreach::foreach(
    i = seq_along(sel.labels), .combine = "comb", .multicombine = TRUE
  ) %do% {
    rows <- create_rows(
      dplyr::filter(features_table, cluster == sel.labels[i]),
      mz_tol_relative,
      rt_tol_relative,
      min_occurrence,
      sample_names
    )

    if (!is.null(rows)) {
      rows$metadata_row <- c(i, rows$metadata_row)
      rows$intensity_row <- c(i, rows$intensity_row)
      rows$rt_row <- c(i, rows$rt_row)
    }

    list(metadata = rows$metadata_row, intensity = rows$intensity_row, rt = rows$rt_row)
  }

  colnames(aligned_features$metadata) <- metadata_colnames
  colnames(aligned_features$intensity) <- intensity_colnames
  colnames(aligned_features$rt) <- rt_colnames

  return(aligned_features)
}
