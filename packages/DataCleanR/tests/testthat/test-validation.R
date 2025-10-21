test_that("read_raw_csv importe correctement", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  imported_df <- read_raw_csv(tmp)
  expect_equal(imported_df, df)
  expect_true(is.data.frame(imported_df))
})