test_that("normalize_remote_ratio works correctly", {
  
  # Test 1: Conversion et bornes
  df <- data.frame(remote_ratio = c("50", "-10", "150", "abc"))
  result <- normalize_remote_ratio(df)
  expect_equal(result$remote_ratio[1], 50)
  expect_equal(result$remote_ratio[2], 0)
  expect_equal(result$remote_ratio[3], 100)
  expect_true(is.na(result$remote_ratio[4]))
  
  # Test 2: Mode binaire avec seuil 50
  df <- data.frame(remote_ratio = c("25", "50", "75", NA))
  result <- normalize_remote_ratio(df, binary = TRUE)
  expect_equal(result$remote_ratio[1], 0)
  expect_equal(result$remote_ratio[2], 100)
  expect_equal(result$remote_ratio[3], 100)
  expect_true(is.na(result$remote_ratio[4]))
})

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
  expect_true(is.factor(result))
  expect_equal(as.character(result), c("Full-time", "Part-time", "Freelance", "Contract",  "Part-time", "Unknown"))
  expect_equal(levels(result), levels_test)
  expect_true(is.ordered(result2))
})


test_that("normalize_country_codes", {
  data_test <- data.frame(
                          a = 1:3,
                          b = c("UNITED STATES", "GB", "France"),
                          stringsAsFactors = FALSE)
  out <- normalize_country_codes(data_test, col = "b")
  expect_true(all(is.na(out$b) | grepl("^[A-Z]{2}$", out$b)))
  expect_equal(out$b, c("US", "GB", "FR"))
})