#test pour la fonction validate_ranges
test_that("validate_ranges filtre les salaires invalides", {
  test_data <- data.frame(
    salary = c(50000, -1000, 0, 60000),
    remote_ratio = c(50, 50, 50, 50),
    work_year = c(2023, 2023, 2023, 2023)
  )
  result <- validate_ranges(test_data)
  
  expect_equal(nrow(result), 2)
  expect_true(all(result$salary > 0))
})

test_that("validate_ranges filtre les remote_ratio invalides", {
  test_data <- data.frame(
    salary = c(50000, 60000, 70000),
    remote_ratio = c(-10, 50, 150),
    work_year = c(2023, 2023, 2023)
  )
  result <- validate_ranges(test_data)
  
  expect_equal(nrow(result), 1)
  expect_true(all(result$remote_ratio >= 0 & result$remote_ratio <= 100))
})

test_that("validate_ranges filtre les années invalides", {
  test_data <- data.frame(
    salary = c(50000, 60000),
    remote_ratio = c(50, 50),
    work_year = c(1990, 2023)
  )
  result <- validate_ranges(test_data, min_year = 2000, max_year = 2024)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$work_year, 2023)
})

test_that("validate_ranges gère les valeurs NA", {
  test_data <- data.frame(
    salary = c(50000, NA),
    remote_ratio = c(50, 50),
    work_year = c(2023, 2023)
  )
  result <- validate_ranges(test_data)
  
  expect_equal(nrow(result), 1)
})

test_that("validate_ranges filtre plusieurs contraintes", {
  test_data <- data.frame(
    salary = c(50000, -1000, 60000),
    remote_ratio = c(50, 50, 150),
    work_year = c(2023, 2023, 2023)
  )
  result <- validate_ranges(test_data)
  
  expect_equal(nrow(result), 1)
})

test_that("validate_ranges lève une erreur si colonnes manquantes", {
  test_data <- data.frame(salary = c(50000, 60000))
  
  expect_error(validate_ranges(test_data))
})

test_that("validate_ranges affiche un message", {
  test_data <- data.frame(
    salary = c(50000, -1000),
    remote_ratio = c(50, 50),
    work_year = c(2023, 2023)
  )
  
  expect_message(validate_ranges(test_data))
})



test_that("cap_outliers_salary: quantile caps both sides", {
  df <- data.frame(salary_in_usd = c(10, 12, 13, 1000))
  out <- cap_outliers_salary(df, lower = 0.25, upper = 0.75, verbose = FALSE)
  Q1 <- as.numeric(quantile(df$salary_in_usd, 0.25, type = 7))
  Q3 <- as.numeric(quantile(df$salary_in_usd, 0.75, type = 7))
  expect_equal(min(out$salary_in_usd), Q1, tolerance = 1e-9)
  expect_equal(max(out$salary_in_usd), Q3, tolerance = 1e-9)
})