test_that("read_raw_csv importe correctement", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  imported_df <- read_raw_csv(tmp)
  expect_equal(imported_df, df)
  expect_true(is.data.frame(imported_df))
})

test_that("to_snake_case", {
  df <- data.frame("Nom Client" = c("Alice", "Bob"),
                   "MontantTotal" = c(100, 200),
                   "Date-De-Vente" = c("2024-01-01", "2024-01-02"))
  names(df) <- to_snake_case(names(df))
  expect_equal(names(df), c("nom_client", "montant_total", "date_de_vente"))
})