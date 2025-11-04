testthat("variance_homogeneity_test - levene works", {
  df <- data.frame(company_location = c("US", "US", "US", "FR"),
                   salary_in_usd = c(100, 200, 1000, 2000),
                   stringsAsFactors = FALSE)

    res <- variance_homogeneity_test(data, y = "salary_in_usd", group = "company_location", method = "levene")
    testthat::expect_s3_class(res, "anova")
    testthat::expect_true("Pr(>F)" %in% colnames(res))
    p <- res["g", "Pr(>F)"]
    testthat::expect_true(is.numeric(p) && !is.na(p))
    testthat::expect_gt(p, 0.05)
})