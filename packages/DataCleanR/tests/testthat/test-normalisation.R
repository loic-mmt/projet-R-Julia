test_that("normalize_factor", {
  mapping_test <- c(
    "FT" = "Full-time",
    "PT" = "Part-time",
    "CT" = "Contract",
    "FL" = "Freelance"
  )
  codes_test <- c("FT", "PT", "FL", "CT", "PT", "XX")
  levels_test = c("Full-time", "Part-time", "Contract", "Freelance", "Unknown")
  result <- normalize_factor(codes_test, mapping_test, levels_test)
  result2 <- normalize_factor(codes_test, mapping_test, levels_test, TRUE)
  expect_s3_class(result, "factor")
  expect_equal(as.character(result), c("Full-time" "Part-time" "Freelance" "Contract"  "Part-time" "Unknown"))
  expect_equal(levels(result), levels_test)
  expect_true(is.ordered(result2))
  })