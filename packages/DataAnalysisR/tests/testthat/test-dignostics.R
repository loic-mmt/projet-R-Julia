test_that("describe_overview.integer works correctly", {
  # Test avec vecteur entier normal
  vect_int <- c(1L, 2L, 3L, 4L, 5L, 5L)
  result <- describe_overview(vect_int)
  
  expect_type(result, "list")
  expect_named(result, c("taille", "moyenne", "mediane", "nb_groupes", 
                        "minimum", "maximum", "quantiles"))
  expect_equal(result$taille, 6)
  expect_equal(result$moyenne, mean(vect_int))
  expect_equal(result$mediane, median(vect_int))
  expect_equal(result$nb_groupes, 5)
  expect_equal(result$minimum, 1)
  expect_equal(result$maximum, 5)
  expect_equal(result$quantiles, quantile(vect_int))
})

test_that("describe_overview.numeric works correctly", {
  # Test avec vecteur numérique
  vect_num <- c(1.5, 2.5, 3.5, 4.5, 5.5, 5.5)
  result <- describe_overview(vect_num)
  
  expect_type(result, "list")
  expect_equal(result$taille, 6)
  expect_equal(result$moyenne, mean(vect_num))
  expect_equal(result$mediane, median(vect_num))
  expect_equal(result$nb_groupes, 5)
  expect_equal(result$minimum, 1.5)
  expect_equal(result$maximum, 5.5)
})

test_that("describe_overview.factor works correctly", {
  # Test avec facteur
  fact <- factor(c("A", "B", "A", "C", "B", "B"))
  result <- describe_overview(fact)
  
  expect_type(result, "list")
  expect_named(result, c("taille", "nb_niveaux", "levels", "frequences", 
                        "plus_frequent", "moins_frequent"))
  expect_equal(result$taille, 6)
  expect_equal(result$nb_groupes, 3)
  expect_equal(result$levels, c("A", "B", "C"))
  expect_equal(result$frequences, table(fact))
  expect_equal(result$plus_frequent, "B")
  expect_equal(result$moins_frequent, "C")
})

test_that("describe_overview.character works correctly", {
  # Test avec character
  char_vec <- c("oui", "non", "oui", "peut-être", "non", "non")
  result <- describe_overview(char_vec)
  
  expect_type(result, "list")
  expect_named(result, c("taille", "nb_unique", "frequences", 
                        "plus_frequent", "moins_frequent"))
  expect_equal(result$taille, 6)
  expect_equal(result$nb_groupes, 3)
  expect_equal(result$plus_frequent, "non")
  expect_equal(result$moins_frequent, "peut-être")
})