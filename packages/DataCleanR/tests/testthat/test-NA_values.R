test_that("impute_missing: numeric median + categorical new_level", {
  df <- data.frame(
    x = c(1, NA, 3, NA),
    y = factor(c("a", NA, "a", NA), levels = c("a","b")),
    stringsAsFactors = FALSE
  )
  out <- impute_missing(df, num_method = "median",
                        cat_method = "new_level", cat_constant = "Unkown",
                        verbose = FALSE)

  expect_equal(out$x, c(1, 2, 3, 2))
  expect_true(is.factor(out$y))
  expect_true(all(c("a","b","Unkown") %in% levels(out$y)))
  expect_equal(as.character(out$y), c("a","Unkown","a","Unkown"))
})