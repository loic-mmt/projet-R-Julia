test_that("export_csv writes to given path with ; and default filename", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)
  tmp <- tempfile("exports_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  res <- export_csv(df, path = tmp, verbose = FALSE)

  expect_true(dir.exists(tmp))
  expect_true(file.exists(file.path(tmp, "cleaned_data.csv")))
  expect_equal(res, file.path(tmp, "cleaned_data.csv"))

  back <- utils::read.csv2(file.path(tmp, "cleaned_data.csv"), stringsAsFactors = FALSE)
  expect_identical(back, df)
})

test_that("export_csv supports comma separator and custom filename", {
  df <- data.frame(x = c(1, 2.5, 3.2), stringsAsFactors = FALSE)
  tmp <- tempfile("exports_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  filename <- "out.csv"
  res <- export_csv(df, path = tmp, filename = filename, sep = ",", verbose = FALSE)

  expect_true(file.exists(file.path(tmp, filename)))
  expect_equal(res, file.path(tmp, filename))

  back <- utils::read.csv(file.path(tmp, filename), stringsAsFactors = FALSE)
  expect_identical(back, df)
})

test_that("export_csv respects overwrite = FALSE", {
  df <- data.frame(x = 1:2)
  tmp <- tempfile("exports_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  p <- export_csv(df, path = tmp, filename = "file.csv", verbose = FALSE)
  expect_true(file.exists(p))
  expect_error(
    export_csv(df, path = tmp, filename = "file.csv", overwrite = FALSE, verbose = FALSE),
    regexp = "existe"
  )
})
