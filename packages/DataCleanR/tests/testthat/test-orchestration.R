test_that("finalize_salary_tbl works correctly", {
  data_test <- data.frame(
  work_year = c(2023, 2023, 2023, 2022, 2022),
  experience_level = c("SE", "MI", "EN", "SE", "MI"),
  employment_type = c("FT", "FT", "FT", "FT", "CT"),
  job_title = c("Data Scientist", "Data Analyst", "Data Engineer", 
                "Machine Learning Engineer", "Data Scientist"),
  salary = c(120000, 80000, 90000, 110000, 75000),
  salary_currency = c("USD", "USD", "USD", "USD", "EUR"),
  salary_in_usd = c(120000, 80000, 90000, 110000, 82000),
  employee_residence = c("US", "US", "GB", "CA", "DE"),
  remote_ratio = c(100, 50, 0, 100, 50),
  company_location = c("US", "US", "GB", "US", "DE"),
  company_size = c("M", "S", "L", "M", "S"))
  data_test_missing <- data_test
  data_test_missing$work_year <- NULL
  salary_tbl <- finalize_salary_tbl(data_test)
  expect_s3_class(salary_tbl, "salary_tbl")
  expect_error(finalize_salary_tbl(data_test_missing))
})