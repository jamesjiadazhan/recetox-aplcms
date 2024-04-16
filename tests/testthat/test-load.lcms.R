patrick::with_parameters_test_that(
  "test load.lcms reads mzML file",
  {
    # Arrange: Set up test inputs
    testdata <- file.path("..", "testdata")
    input_path <- file.path(testdata, filename)

    # Act: Execute the function with the test inputs
    sut <- load.lcms(input_path)

    # Assert: Verify the function output matches expected results
    # Check that the function returns an object of the expected type
    testthat::expect_s3_class(sut, "tbl_df")
  },
  patrick::cases(
    test_case_1 = list(
      filename = c("mbr_test0_copy.mzml")
    ),
    test_case_2 = list(
      filename = c("test0_copy.mzxml")
    )
  )
)