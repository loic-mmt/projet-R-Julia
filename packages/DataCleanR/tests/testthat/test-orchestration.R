#Fonction cleaning_pipeline
test_that("cleaning_pipeline exécute toutes les étapes", {
  test_data <- data.frame(
    salary = c(50000, 60000),
    salary_currency = c("EUR", "USD"),
    salary_in_usd = c(55000, 60000),
    work_year = c(2023, 2023),
    remote_ratio = c(50, 100),
    experience_level = c("SE", "MI"),
    employment_type = c("FT", "FT"),
    job_title = c("Data Scientist", "ML Engineer"),
    employee_residence = c("US", "CA"),
    company_location = c("US", "CA"),
    company_size = c("M", "L")
  )
  
  result <- cleaning_pipeline(test_data, verbose = FALSE)
  
  expect_true(nrow(result) > 0)
  expect_true("salary_in_usd" %in% names(result))
})

test_that("cleaning_pipeline supprime les lignes invalides", {
  test_data <- data.frame(
    salary = c(50000, -1000),
    salary_currency = c("EUR", "USD"),
    salary_in_usd = c(55000, 60000),
    work_year = c(2023, 1990),
    remote_ratio = c(50, 150),
    experience_level = c("SE", "MI"),
    employment_type = c("FT", "FT"),
    job_title = c("Data Scientist", "ML Engineer"),
    employee_residence = c("US", "CA"),
    company_location = c("US", "CA"),
    company_size = c("M", "L")
  )
  
  result <- cleaning_pipeline(test_data, min_year = 2000, verbose = FALSE)
  
  expect_true(nrow(result) < nrow(test_data))
})

test_that("cleaning_pipeline fonctionne en mode silencieux", {
  test_data <- data.frame(
    salary = 50000, salary_currency = "EUR", salary_in_usd = 55000,
    work_year = 2023, remote_ratio = 50, experience_level = "SE",
    employment_type = "FT", job_title = "Data Scientist",
    employee_residence = "US", company_location = "US", company_size = "M"
  )
  
  expect_silent(cleaning_pipeline(test_data, verbose = FALSE))
})

test_that("cleaning_pipeline affiche des messages en mode verbose", {
  test_data <- data.frame(
    salary = 50000, salary_currency = "EUR", salary_in_usd = 55000,
    work_year = 2023, remote_ratio = 50, experience_level = "SE",
    employment_type = "FT", job_title = "Data Scientist",
    employee_residence = "US", company_location = "US", company_size = "M"
  )
  
  expect_message(cleaning_pipeline(test_data, verbose = TRUE))
})

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