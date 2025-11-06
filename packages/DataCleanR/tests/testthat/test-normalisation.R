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

test_that("normalize_factor works correctly", {
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

test_that("normalize_all works correctly", {
  data <- data.frame(
    company_location   = c("United States", "Germany", "France"),
    employee_residence = c("US", "DE", "FR"),
    job_title          = c("Data Analyst", "Machine Learning Engineer", "Director of Data Science"),
    remote_ratio       = c("100", "0", "50"),
    company_size       = c("S", "M", "L"),
    employment_type    = c("FT", "PT", "CT"),
    experience_level   = c("EN", "SE", "EX"),
    stringsAsFactors   = FALSE
  )

  result <- normalize_all(data)

  expect_true(all(c(
    "company_grouping", "employee_grouping",
    "company_location", "employee_residence",
    "job_title", "remote_ratio",
    "company_size", "employment_type",
    "experience_level"
  ) %in% names(result)))

  expect_true(all(result$company_location %in% union(levels_iso2, c("Unknow"))))
  expect_true(all(result$employee_residence %in% union(levels_iso2, c("Unknow"))))

  expect_true(all(result$company_grouping %in% union(regions_levels, c("Unknow"))))
  expect_true(all(result$employee_grouping %in% union(regions_levels, c("Unknow"))))

  expect_true(all(result$job_title %in% union(levels_job_title, c("Unknow"))))

  expect_true(all(result$company_size %in% union(size_levels, c("Unknow"))))

  expect_true(all(result$employment_type %in% union(levels_employement_type, c("Unknow"))))

  expect_true(all(result$experience_level %in% union(experience_labels_ordered, c("Unknow"))))

  expect_true(all(result$remote_ratio >= 0 & result$remote_ratio <= 100))

  expect_equal(nrow(result), nrow(data))
})