
enforce_types <- function(data, num_threshold = 0.9, max_factor_levels = 20) {
  out <- data
  
  for (col in names(out)) {
    x <- out[[col]]
    
    # Ignorer si déjà au bon type
    if (is.numeric(x) || is.factor(x) || inherits(x, "Date")) {
      next
    }
    
    # Nettoyer les espaces
    if (is.character(x)) {
      x <- trimws(x)
    }
    
    # Calculer le nombre de valeurs non-NA
    valid_values <- x[!is.na(x) & x != ""]
    n_valid <- length(valid_values)
    
    if (n_valid == 0) {
      next  # Colonne vide
    }
    
    # 🔹 1. Essayer conversion numérique
    x_numeric <- suppressWarnings(as.numeric(x))
    n_numeric_valid <- sum(!is.na(x_numeric[!is.na(x) & x != ""]))
    
    # Si au moins num_threshold% des valeurs sont convertibles en numérique
    if (n_numeric_valid / n_valid >= num_threshold) {
      # Vérifier si ce sont des entiers
      if (all(x_numeric[!is.na(x_numeric)] == floor(x_numeric[!is.na(x_numeric)]))) {
        out[[col]] <- as.integer(x_numeric)
      } else {
        out[[col]] <- x_numeric
      }
      next
    }
    
    # 🔹 2. Vérifier si c'est un facteur potentiel
    n_unique <- length(unique(valid_values))
    
    # Convertir en facteur SEULEMENT si <= max_factor_levels valeurs uniques
    if (n_unique <= max_factor_levels) {
      out[[col]] <- as.factor(x)
      next
    }
    
    # 🔹 3. Sinon garder comme character
    out[[col]] <- as.character(x)
  }
  
  return(out)
}


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
  expect_true(is.character(df_clean$char_col), info = "char_col doit rester character")
  
  expect_equal(df_clean$numeric_col, c(1.5, 2.2, 3.1))
  expect_equal(levels(df_clean$factor_col), c("A", "B"))
})


