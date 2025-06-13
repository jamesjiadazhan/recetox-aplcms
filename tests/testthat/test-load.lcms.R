create_test_case <- function(filename, mz_length, rt_length, intensities_length) {
  list(
    filename = filename,
    expected_type = "list",
    expected_lengths = c(mz = mz_length, rt = rt_length, intensities = intensities_length),
    expected_num_na = c(mz = 0, rt = 0, intensities = 0)
  )
}

# patrick::with_parameters_test_that(
#   "test load.lcms reads different file types",
#   {
#     if(packageVersion("mzR") >= "2.29.0" && tools::file_ext(filename) == "mzdata") {
#       print("mzR >= 2.29.0 no longer supports mzdata.")
#       skip()
#     }
#     # Arrange: Set up test inputs
#     testdata <- file.path("..", "testdata")
#     input_path <- file.path(testdata, "input", filename)

#     # Act: Execute the function with the test inputs
#     data <- load.lcms(input_path)

#     # Assert: Verify the function output matches expected results
#     # Check that the function returns an object of the expected type
#     actual_type <- data
#     testthat::expect_type(actual_type, expected_type)
    
#     # Check the lengths of the vectors in the list
#     actual_lengths <- lengths(data)
#     testthat::expect_equal(actual_lengths, expected_lengths)

#     # Check the number of NA values in each vector
#     actual_num_na <- sapply(data, function(x) sum(is.na(x)))
#     testthat::expect_equal(actual_num_na, expected_num_na)
#   },
  
#   patrick::cases(
#     test_case_1 = create_test_case("RCX_06_shortened.mzML", 879476, 879476, 879476),
#     test_case_2 = create_test_case("test_file.mzXML", 9647575, 9647575, 9647575),
#     test_case_3 = create_test_case("alg3.mzdata", 543894, 543894, 543894)
#   )
# )

testthat::test_that("load.lcms.raw fails if rawrr is not installed", {
  # Act & Assert: Expect an error when trying to load a non-existent file
  testthat::skip_if(requireNamespace("rawrr", quietly = TRUE), "The 'rawrr' package is already installed.")	
  testthat::expect_error(load.lcms.raw("test.raw"), "The 'rawrr' package is required but not installed. Please install it with install.packages('rawrr').")
})


testthat::test_that("load.lcms.raw fails if rawrr is not installed correctly.", {
  # Act & Assert: Expect an error when trying to load a non-existent file
  testthat::skip_if(!requireNamespace("rawrr", quietly = TRUE), "The 'rawrr' package needs to be installed for this check.")
  testthat::skip_if(rawrr::rawrrAssemblyPath() != "", "The 'rawrr' package is set up correctly, skipping this test.")
  testthat::expect_error(load.lcms.raw("test.raw"), "The 'rawrr' package is not set up correctly. Please ensure that the rawrr package is installed and configured properly.")
})

testthat::test_that("load.lcms.raw reads a raw file correctly", {
  # Arrange: Set up test inputs
  sample_raw_file <- rawrr::sampleFilePath()

  # Act: Execute the function with the test inputs
  actual <- load.lcms.raw(sample_raw_file)

  # Assert: Verify the function output matches expected results
  testthat::expect_equal(nrow(actual), 30689)
  testthat::expect_equal(ncol(actual), 3)
  testthat::expect_equal(length(actual$mz), 30689)
  testthat::expect_equal(length(actual$rt), 30689)
  testthat::expect_equal(length(actual$intensities), 30689)
})