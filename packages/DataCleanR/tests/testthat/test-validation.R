test_that("read_raw_csv importe correctement", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  imported_df <- read_raw_csv(tmp)
  expect_equal(imported_df, df)
  expect_true(is.data.frame(imported_df))
})

test_that("validate_schema", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  names_to_test1 <- c("a", "c")
  names_to_test2 <- c("a", "b", "c")
  test1 <- validate_schema(df, names_to_test1)
  test2 <- validate_schema(df, names_to_test2)
  expect_equal(list(test1, test2), list("All required colums are present in the dataframe", "The dataframe in not complete and it's missing c"))
})

test_that("to_snake_case", {
  df <- data.frame("Nom Client" = c("Alice", "Bob"),
                   "MontantTotal" = c(100, 200),
                   "Date-De-Vente" = c("2024-01-01", "2024-01-02"))
  names(df) <- to_snake_case(names(df))
  expect_equal(names(df), c("nom_client", "montant_total", "date_de_vente"))
})


test_that("enforce_types", {
  # Création du dataframe de test
  df <- data.frame(
    numeric_col = c("1.5", "2.2", "3.1"),
    int_col = c("1", "2", "3"),
    factor_col = c("A", "B", "A"),
    char_col = c("Alice", "Bob", "Charlie"),
    stringsAsFactors = FALSE
  )
  
  # Application de la fonction
  df_clean <- enforce_types(df, num_threshold = 0.9, max_factor_levels = 10)
  
  # Tests
  expect_true(is.numeric(df_clean$numeric_col), info = "numeric_col doit être numeric")
  expect_true(is.integer(df_clean$int_col), info = "int_col doit être integer")
  expect_true(is.factor(df_clean$factor_col), info = "factor_col doit être factor")
  expect_true(is.factor(df_clean$char_col), info = "char_col doit être factor maintenant")
  
  expect_equal(df_clean$numeric_col, c(1.5, 2.2, 3.1))
  expect_equal(levels(df_clean$factor_col), c("A", "B"))
})



test_that("deduplicate_rows", {
  df <- data.frame(
    id = c(1, 1, 2, 2, 3),
    job = c("DE", "DE", "DS", "DS", "DE"),
    salary = c(100, 100, 200, 200, 300)
  )

  out <- DataCleanR::deduplicate_rows(df)

  expect_equal(nrow(out), 3)
  expect_equal(attr(out, "n_removed"), 2)
  expect_equal(out, df[c(1, 3, 5), ], ignore_attr = TRUE)
})