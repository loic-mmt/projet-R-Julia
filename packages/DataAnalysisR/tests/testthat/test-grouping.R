test_that("summarize_salary: median by single group", {
  df <- data.frame(
    work_year = c(2020, 2020, 2021, 2021),
    salary_in_usd = c(100, 200, 1000, 2000),
    stringsAsFactors = FALSE
  )
  res <- summarize_salary(df, applyto = "salary_in_usd", method = "median", by = "work_year", verbose = FALSE)
  expect_equal(names(res), c("work_year", "salary_in_usd_median"))
  expect_equal(res$salary_in_usd_median[res$work_year == 2020], 150)
  expect_equal(res$salary_in_usd_median[res$work_year == 2021], 1500)
})

test_that("summarize_salary: mean by two grouping columns", {
  df <- data.frame(
    work_year = c(2020, 2020, 2021, 2021),
    company_location = c("US", "US", "US", "FR"),
    salary_in_usd = c(100, 200, 1000, 2000),
    stringsAsFactors = FALSE
  )
  res <- summarize_salary(df, method = "mean", by = c("work_year", "company_location"), verbose = FALSE)
  expect_equal(names(res), c("work_year", "company_location", "salary_in_usd_mean"))
  expect_equal(res$salary_in_usd_mean[res$work_year == 2020 & res$company_location == "US"], 150)
  expect_equal(res$salary_in_usd_mean[res$work_year == 2021 & res$company_location == "US"], 1000)
  expect_equal(res$salary_in_usd_mean[res$work_year == 2021 & res$company_location == "FR"], 2000)
})

test_that("summarize_salary: IQR with NA and na.rm=TRUE", {
  df <- data.frame(
    work_year = c(2020, 2020, 2020, 2020),
    salary_in_usd = c(NA, 10, 20, 40),
    stringsAsFactors = FALSE
  )
  res <- summarize_salary(df, method = "iqr", by = "work_year", na.rm = TRUE, verbose = FALSE)
  expect_equal(res$salary_in_usd_iqr, 15)
})


test_that("summarize_salary: validation errors", {
  df <- data.frame(
    work_year = c(2020, 2021),
    salary_in_usd = c(100, 200),
    stringsAsFactors = FALSE
  )
  # applyto doesn't exist
  expect_error(summarize_salary(df, applyto = "missing_col", by = "work_year", verbose = FALSE), "introuvable")
  # non-numeric applyto
  df_char <- df
  df_char$salary_in_usd <- as.character(df_char$salary_in_usd)
  expect_error(summarize_salary(df_char, applyto = "salary_in_usd", by = "work_year", verbose = FALSE), "doit être numérique")
  # missing group column
  expect_error(summarize_salary(df, by = "unknown_group", verbose = FALSE), "regroupement introuvable")
})


test_that("association_table : liste contenant table, v, chi2, df, p.value", {
  df <- data.frame(
    work_year = c(2020, 2020, 2021, 2021),
    company_location = c("US", "US", "US", "FR"),
    salary_in_usd = c(100, 200, 1000, 2000),
    stringsAsFactors = FALSE
  )
  out <- association_table(df, x = "work_year", y = "company_location", prop = "row")
})