test_that("say_hello salue correctement", {
  expect_equal(say_hello("Alice"), "Hello, Alice !")
  expect_type(say_hello("Bob"), "character")
})