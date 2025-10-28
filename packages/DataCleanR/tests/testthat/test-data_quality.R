test_that("cap_outliers_salary: quantile caps both sides", {
  df <- data.frame(salary_in_usd = c(10, 12, 13, 1000))
  out <- cap_outliers_salary(df, lower = 0.25, upper = 0.75, verbose = FALSE)
  Q1 <- as.numeric(quantile(df$salary_in_usd, 0.25, type = 7))
  Q3 <- as.numeric(quantile(df$salary_in_usd, 0.75, type = 7))
  expect_equal(min(out$salary_in_usd), Q1, tolerance = 1e-9)
  expect_equal(max(out$salary_in_usd), Q3, tolerance = 1e-9)
})