to_snake_case <- function(data) {
  data <- gsub(" ", "_", data)
  data <- gsub("-", "_", data)
  data <- gsub("__+", "_", data)
  data <- gsub("[^a-z0-9_]", "", data)
  data <- tolower(data)
  return(data)
}

test_that("to_snake_case", {
          df <- data.frame("Nom Client" = c("Alice", "Bob"),
                           "MontantTotal" = c(100, 200),
                           "Date-De-Vente" = c("2024-01-01", "2024-01-02"))
  names(df) <- to_snake_case(names(df))
  expect_true(names(df) == "nom_client", "montant_total", "date_de_vente")
})