test_that("variance_homogeneity_test - levene works", {
  df <- data.frame(company_location = c("US", "US", "US", "FR"),
                   salary_in_usd = c(100, 200, 1000, 2000),
                   stringsAsFactors = FALSE)

    res <- variance_homogeneity_test(df, y = "salary_in_usd", group = "company_location", method = "levene")
    expect_s3_class(res, "anova")
    expect_true("Pr(>F)" %in% colnames(res))
    expect_true("F value" %in% colnames(res))
    expect_true("Mean Sq" %in% colnames(res))
    expect_true("Sum Sq" %in% colnames(res))
    expect_true("Df" %in% colnames(res))
    p <- res["g", "Pr(>F)"]
    expect_true(is.numeric(p) && !is.na(p))
    expect_gt(p, 0.05)
})