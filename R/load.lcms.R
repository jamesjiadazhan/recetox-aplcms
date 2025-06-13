#' @import mzR
#' @import tibble
NULL
#> NULL

#' Loading LC/MS data.
#' 
#' This is an internal function. It loads LC/MS data into memory.
#' 
#' @param filename The CDF file name.
#' @return A tibble is returned with the following columns:
#' \itemize{
#'   \item mz - The vector of m/z values.
#'   \item rt - The vector of retention times.
#'   \item intensities - The vector of intensity values.
#' }
#' @export
load.lcms <- function(filename) {
  mz_conn <- mzR::openMSfile(filename = filename)
  b <- mzR::header(mz_conn)$retentionTime

  masses <- NULL
  intensi <- NULL
  labels <- NULL

  segs <- seq(0, length(b), by = 200)
  if ((length(b) %% 200) != 0)
    segs <- c(segs, length(b))

  for (n in 2:length(segs)) {
    a <- mzR::peaks(mz_conn, scans = (segs[n - 1] + 1):segs[n])

    this_masses <- NULL
    this_intensi <- NULL
    this_labels <- NULL

    for (i in seq_along(a)) {
      this_a <- a[[i]]

      if (!is.null(nrow(this_a))) {
        this_a <- this_a[this_a[, 2] > 1e-10,]
        if (is.null(nrow(this_a)))
          this_a <- matrix(this_a, nrow = 1)

        this_masses <- c(this_masses, this_a[, 1])
        this_intensi <- c(this_intensi, this_a[, 2])
        this_labels <- c(this_labels, rep(b[segs[n - 1] + i], nrow(this_a)))
      }else {
        b[segs[n - 1] + i] <- NA
      }
    }

    masses <- c(masses, this_masses)
    intensi <- c(intensi, this_intensi)
    labels <- c(labels, this_labels)
  }

  mzR::close(mz_conn)

  features <- tibble::tibble(mz = masses, rt = labels, intensities = intensi)
  return(features)
}

#' Loading MS data from raw files.
#'
#' This is an internal function. It loads MS data from raw files into memory.
#'
#' @param filename The raw file name.
#' @return A tibble is returned with the following columns:
#' \itemize{
#'   \item mz - The vector of m/z values.
#'   \item rt - The vector of retention times.
#'   \item intensities - The vector of intensity values.
#' }
#' @export
load.lcms.raw <- function(filename) {
  # Check if the rawrr package is installed
  if (!requireNamespace("rawrr", quietly = TRUE)) {
    stop("The 'rawrr' package is required but not installed. Please install it with install.packages('rawrr').")
  }

  # Check if rawrr is setup correctly
  if(rawrr::rawrrAssemblyPath() == "") {
    stop("The 'rawrr' package is not set up correctly. Please ensure that the rawrr package is installed and configured properly.")
  }

  # Check if the file exists
  if (!file.exists(filename)) {
    stop(paste("The file", filename, "does not exist."))
  }

  header <- rawrr::readFileHeader(filename)
  idx <- rawrr::readIndex(filename)
  scans <- rawrr::readSpectrum(filename, scan=idx$scan)

  mz <- c()
  intensities <- c()
  rt <- c()

  for (i in seq_along(scans)) {
    scan <- scans[[i]]
    if (length(scan$mZ) > 0 && length(scan$intensity) > 0) {
      mz <- c(mz, scan$mZ)
      intensities <- c(intensities, scan$intensity)
      rt <- c(rt, rep(idx$StartTime[i], length(scan$mZ)))
    }
  }

  features <- tibble::tibble(mz = mz, rt = rt, intensities = intensities)
  return(features)
}