#' @import tools snow splines parallel doParallel
NULL
#> NULL

#' Read the metadata table, retention time data matrix and intensity data matrix
#' and combine them into a single table
#' @param metadata Tibble Feature metadata table with information concerning the peaks.
#' @param rt_crosstab Tibble Data matrix with features on rows and samples on columns holding rt data.
#' @param int_crosstab Tibble Data matrix with features on rows and samples on columns holding intensity data.
#' @return Tibble A merged table containing all information.
#' @export 
as_feature_sample_table <- function(metadata, rt_crosstab, int_crosstab) {
  feature_names <- as.character(rt_crosstab$id)
  sample_names <- colnames(metadata)[-c(1:8)]

  feature_table <- data.frame(
    feature = feature_names,
    mz = metadata$mz,
    rt = metadata$rt
  )

  # series of conversions to produce a table type from data.frame
  rt_crosstab <- as.table(as.matrix(rt_crosstab[, -1]))
  int_crosstab <- as.table(as.matrix(int_crosstab[, -1]))

  crosstab_axes <- list(feature = feature_names, sample = sample_names)
  dimnames(rt_crosstab) <- dimnames(int_crosstab) <- crosstab_axes

  x <- as.data.frame(rt_crosstab, responseName = 'sample_rt')
  y <- as.data.frame(int_crosstab, responseName = 'sample_intensity')

  data <- merge(x, y, by = c('feature', 'sample'))
  data <- merge(feature_table, data, by = 'feature')
  return(data)
}

#' Check files whether they exist.
#' @param filenames list of filenames Filenames to check whether they exist.
#' @export
check_files <- function(filenames) {
  missing <- !file.exists(filenames)
  missing_filenames <- paste0('\t', filenames[missing], collapse = '\n')

  if (any(missing)) {
    stop("Cannot find the following files:\n", missing_filenames)
  }
}

#' Get the sample name as basename of the file.
#' @param filename string Name of the file.
#' @return string Sample name.
#' @export
get_sample_name <- function(filename) {
  tools::file_path_sans_ext(basename(filename))
}

#' Runs features extraction in unsupervised mode.
#' 
#' features extraction in unsupervised mode.
#' 
#' @param filenames The CDF file names.
#' @param cache Whether to cache the results during the parallel processing of the peak detection.
#' @param min_occurrence A feature has to show up in at least this number of profiles to be included in the final result.
#' @param min_pres This is a parameter of the run filter, to be passed to the function remove_noise().
#' @param min_run Run filter parameter. The minimum length of elution time for a series of signals grouped by m/z 
#'  to be considered a peak.
#' @param mz_tol m/z tolerance level for the grouping of data points. This value is expressed as the fraction of 
#'  the m/z value. This value, multiplied by the m/z value, becomes the cutoff level. The recommended value is 
#'  the machine's nominal accuracy level. Divide the ppm value by 1e6. For FTMS, 1e-5 is recommended.
#' @param baseline_correct After grouping the observations, the highest intensity in each group is found. 
#'  If the highest is lower than this value, the entire group will be deleted.
#' @param baseline_correct_noise_percentile The percentile of signal strength of those EIC that don't pass the 
#'  run filter, to be used as the baseline threshold of signal strength.
#' @param shape_model The mathematical model for the shape of a peak. There are two choices - "bi-Gaussian" and 
#'  "Gaussian". When the peaks are asymmetric, the bi-Gaussian is better. The default is "bi-Gaussian".
#' @param BIC_factor The factor that is multiplied on the number of parameters to modify the BIC criterion. 
#'  If larger than 1, models with more peaks are penalized more.
#' @param peak_estim_method The estimation method for the bi-Gaussian peak model. Two possible values: moment and EM.
#' @param min_bandwidth The minimum bandwidth to use in the kernel smoother.
#' @param max_bandwidth The maximum bandwidth to use in the kernel smoother.
#' @param sd_cut A vector of two. Features with standard deviation outside the range defined by the two numbers 
#'  are eliminated.
#' @param sigma_ratio_lim A vector of two. It enforces the belief of the range of the ratio between the left-standard 
#'  deviation and the right-standard deviation of the bi-Gaussian function used to fit the data.
#' @param component_eliminate In fitting mixture of bi-Gaussian (or Gaussian) model of an EIC, when a component accounts 
#'  for a proportion of intensities less than this value, the component will be ignored.
#' @param moment_power The power parameter for data transformation when fitting the bi-Gaussian or Gaussian mixture 
#'  model in an EIC.
#' @param mz_tol_relative The m/z tolerance level for peak alignment. The default is NA, which allows the program to search 
#'  for the tolerance level based on the data. This value is expressed as the percentage of the m/z value. This value, 
#'  multiplied by the m/z value, becomes the cutoff level.
#' @param rt_tol_relative The retention time tolerance level for peak alignment. The default is NA, which allows the program 
#'  to search for the tolerance level based on the data.
#' @param mz_tol_absolute As the m/z tolerance is expressed in relative terms (ppm), it may not be suitable when the 
#'  m/z range is wide. This parameter limits the tolerance in absolute terms. It mostly influences feature matching 
#'  in higher m/z range.
#' @param recover_mz_range The m/z around the feature m/z to search for observations. The default value is NA, in which 
#'  case 1.5 times the m/z tolerance in the aligned object will be used.
#' @param recover_rt_range The retention time around the feature retention time to search for observations. The default 
#'  value is NA, in which case 0.5 times the retention time tolerance in the aligned object will be used.
#' @param use_observed_range If the value is TRUE, the actual range of the observed locations of the feature in all 
#'  the spectra will be used.
#' @param recover_min_count Minimum number of raw data points to support a recovery.
#' @param intensity_weighted Whether to use intensity to weight mass density estimation.
#' @param do.plot Indicates whether plot should be drawn.
#' @param cluster The number of CPU cores to be used
#' @export
unsupervised <- function(
    filenames,
    cache = TRUE,
    min_occurrence = 2,
    min_pres = 0.5,
    min_run = 12,
    mz_tol = 1e-05,
    baseline_correct = 0,
    baseline_correct_noise_percentile = 0.05,
    shape_model = "bi-Gaussian",
    BIC_factor = 2,
    peak_estim_method = "moment",
    bandwidth = 0.5,
    min_bandwidth = NA,
    max_bandwidth = NA,
    sd_cut = c(0.01, 500),
    sigma_ratio_lim = c(0.01, 100),
    component_eliminate = 0.01,
    moment_power = 1,
    mz_tol_relative = NA,
    rt_tol_relative = NA,
    mz_tol_absolute = 0.01,
    recover_mz_range = NA,
    recover_rt_range = NA,
    use_observed_range = TRUE,
    recover_min_count = 3,
    intensity_weighted = FALSE,
    do_plot = FALSE,
    cluster = 4
) {
    if (!is(cluster, 'cluster')) {
        cluster <- parallel::makeCluster(cluster)
        on.exit(parallel::stopCluster(cluster))
    }

    # NOTE: side effect (doParallel has no functionality to clean up)
    doParallel::registerDoParallel(cluster)
    register_functions_to_cluster(cluster)

    # print the number of cores
    print(paste0("number of cores used: ", cluster))

    check_files(filenames)
    sample_names <- get_sample_name(filenames)
    number_of_samples <- length(sample_names)

    # print the number of samples
    print(paste0("number of samples: ", number_of_samples))

    # As not all objects or variables are automatically exported to the worker environments when using parallel processing in R, we need to export the needed variables to the cluster
    clusterExport(cluster, c(
      "cache", "min_occurrence", "min_pres","min_run","mz_tol","baseline_correct","baseline_correct_noise_percentile","shape_model","BIC_factor","peak_estim_method","bandwidth","min_bandwidth","max_bandwidth","sd_cut","sigma_ratio_lim","component_eliminate","moment_power","mz_tol_relative","rt_tol_relative","mz_tol_absolute","recover_mz_range","recover_rt_range","use_observed_range","recover_min_count","intensity_weighted","do_plot"
    ))
    # print the current settings for min_pres and min_run
    print(paste0("current settings: ", "min_pres = ", min_pres, ", min_run = ", min_run))

    # print the noise removal message
    message("**** noise removal in the raw data ****")

    # print the cache message
    print(paste0("cache is set to ", cache))

    # run the noise removal in parallel
    ## progress bar version
    profiles <- pbapply::pblapply(
        filenames, 
        function(filename) {
            remove_noise(
                filename = filename,
                min_pres = min_pres,
                min_run = min_run,
                mz_tol = mz_tol,
                baseline_correct = baseline_correct,
                baseline_correct_noise_percentile = baseline_correct_noise_percentile,
                intensity_weighted = intensity_weighted,
                do.plot = do_plot,
                cache = cache
            )
        }, 
        cl = cluster
    )

    # original version without progress bar
    # profiles <- snow::parLapply(cluster, filenames, function(filename) {
    #     remove_noise(
    #         filename = filename,
    #         min_pres = min_pres,
    #         min_run = min_run,
    #         mz_tol = mz_tol,
    #         baseline_correct = baseline_correct,
    #         baseline_correct_noise_percentile = baseline_correct_noise_percentile,
    #         intensity_weighted = intensity_weighted,
    #         do.plot = do_plot,
    #         cache = cache
    #     )
    # })


    # print the feature extraction message
    ## progress bar version
    message("**** feature extraction ****")
    feature_tables <- pbapply::pblapply(
        profiles, 
        function(profile) {
            prof.to.features(
                profile = profile,
                bandwidth = bandwidth,
                min_bandwidth = min_bandwidth,
                max_bandwidth = max_bandwidth,
                sd_cut = sd_cut,
                sigma_ratio_lim = sigma_ratio_lim,
                shape_model = shape_model,
                peak_estim_method = peak_estim_method,
                component_eliminate = component_eliminate,
                moment_power = moment_power,
                BIC_factor = BIC_factor,
                do.plot = do_plot
            )
        }, 
        cl = cluster
    )

    # message("**** feature extraction ****")
    # feature_tables <- snow::parLapply(cluster, profiles, function(profile) {
    #     prof.to.features(
    #         profile = profile,
    #         bandwidth = bandwidth,
    #         min_bandwidth = min_bandwidth,
    #         max_bandwidth = max_bandwidth,
    #         sd_cut = sd_cut,
    #         sigma_ratio_lim = sigma_ratio_lim,
    #         shape_model = shape_model,
    #         peak_estim_method = peak_estim_method,
    #         component_eliminate = component_eliminate,
    #         moment_power = moment_power,
    #         BIC_factor = BIC_factor,
    #         do.plot = do_plot
    #     )
    # })

    message("**** compute clusters of mz and rt and assign cluster id to individual features ****")
    extracted_clusters <- compute_clusters(
        feature_tables = feature_tables,
        mz_tol_relative = mz_tol_relative,
        mz_tol_absolute = mz_tol_absolute,
        mz_max_diff = 10 * mz_tol,
        rt_tol_relative = rt_tol_relative,
        do.plot = do_plot,
        sample_names = sample_names
    )

    message("**** select the most features as the template feature table ****")
    template_features <- compute_template(extracted_clusters$feature_tables)

    message("**** retention time correction based on the template feature table ****")
    corrected <- foreach::foreach(this.feature = extracted_clusters$feature_tables) %dopar% correct_time(
        this.feature,
        template_features
    )

    message("**** compute clusters of mz and rt and assign cluster id to individual features ****")
    adjusted_clusters <- compute_clusters(
        feature_tables = corrected,
        mz_tol_relative = extracted_clusters$mz_tol_relative,
        mz_tol_absolute = extracted_clusters$rt_tol_relative,
        mz_max_diff = 10 * mz_tol,
        rt_tol_relative = rt_tol_relative,
        do.plot = do_plot,
        sample_names = sample_names
    )

    message("**** feature alignment across all samples ****")
    aligned <- create_aligned_feature_table(
        dplyr::bind_rows(adjusted_clusters$feature_tables),
        min_occurrence,
        sample_names,
        adjusted_clusters$rt_tol_relative,
        adjusted_clusters$mz_tol_relative,
        cluster
    )

    # export the following variables to the cluster so that the weaker signal recovery can use them during the parallel processing
    ## sample_names, feature_tables, corrected, aligned, adjusted_clusters
    clusterExport(cluster, c("sample_names", "feature_tables", "corrected"))

    message("**** weaker signal recovery ****")
    recovered <- pbapply::pblapply(
        seq_along(filenames), 
        function(i) {
              recover.weaker(
                  filename = filenames[[i]],
                  sample_name = sample_names[i],
                  extracted_features = feature_tables[[i]],
                  adjusted_features = corrected[[i]],
                  metadata_table = aligned$metadata,
                  rt_table = aligned$rt,
                  intensity_table = aligned$intensity,
                  mz_tol = mz_tol,
                  mz_tol_relative = adjusted_clusters$mz_tol_relative,
                  rt_tol_relative = adjusted_clusters$rt_tol_relative,
                  recover_mz_range = recover_mz_range,
                  recover_rt_range = recover_rt_range,
                  use_observed_range = use_observed_range,
                  bandwidth = bandwidth,
                  min_bandwidth = min_bandwidth,
                  max_bandwidth = max_bandwidth,
                  recover_min_count = recover_min_count,
                  intensity_weighted = intensity_weighted
              )
        }, 
        cl = cluster
    )

    # message("**** weaker signal recovery ****")
    # recovered <- snow::parLapply(cluster, seq_along(filenames), function(i) {
    #     recover.weaker(
    #         filename = filenames[[i]],
    #         sample_name = sample_names[i],
    #         extracted_features = feature_tables[[i]],
    #         adjusted_features = corrected[[i]],
    #         metadata_table = aligned$metadata,
    #         rt_table = aligned$rt,
    #         intensity_table = aligned$intensity,
    #         mz_tol = mz_tol,
    #         mz_tol_relative = adjusted_clusters$mz_tol_relative,
    #         rt_tol_relative = adjusted_clusters$rt_tol_relative,
    #         recover_mz_range = recover_mz_range,
    #         recover_rt_range = recover_rt_range,
    #         use_observed_range = use_observed_range,
    #         bandwidth = bandwidth,
    #         min_bandwidth = min_bandwidth,
    #         max_bandwidth = max_bandwidth,
    #         recover_min_count = recover_min_count,
    #         intensity_weighted = intensity_weighted
    #     )
    # })

    # extract the adjusted features from the recovered list
    recovered_adjusted <- lapply(recovered, function(x) x$adjusted_features)

    message("**** compute clusters of mz and rt and assign cluster id to individual features ****")
    recovered_clusters <- compute_clusters(
        feature_tables = recovered_adjusted,
        mz_tol_relative = adjusted_clusters$mz_tol_relative,
        mz_tol_absolute = adjusted_clusters$rt_tol_relative,
        mz_max_diff = 10 * mz_tol,
        rt_tol_relative = rt_tol_relative,
        do.plot = do_plot,
        sample_names = sample_names
    )

    message("**** feature alignment based on the recovered features ****")
    recovered_aligned <- create_aligned_feature_table(
        dplyr::bind_rows(recovered_clusters$feature_tables),
        min_occurrence,
        sample_names,
        recovered_clusters$rt_tol_relative,
        recovered_clusters$mz_tol_relative,
        cluster
    )

    message("**** convert the aligned feature table to a feature table ****")
    aligned_feature_sample_table <- as_feature_sample_table(
        metadata = aligned$metadata,
        rt_crosstab = aligned$rt,
        int_crosstab = aligned$intensity
    )

    message("**** convert the recovered aligned feature table to a feature table ****")
    recovered_feature_sample_table <- as_feature_sample_table(
        metadata = recovered_aligned$metadata,
        rt_crosstab = recovered_aligned$rt,
        int_crosstab = recovered_aligned$intensity
    )

    message("**** return the apLCMSresults ****")
    list(
        extracted_features = recovered$extracted_features,
        corrected_features = recovered$adjusted_features,
        aligned_feature_sample_table = aligned_feature_sample_table,
        recovered_feature_sample_table = recovered_feature_sample_table,
        aligned_mz_tolerance = as.numeric(recovered_clusters$mz_tol_relative),
        aligned_rt_tolerance = as.numeric(recovered_clusters$rt_tol_relative)
    )
}
