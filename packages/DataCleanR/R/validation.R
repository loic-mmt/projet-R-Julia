#' Import CSV as data.frame
#' @param path chemin vers le CSV
#' @return data.frame
#' @export
read_raw_csv <- function(file_path) {
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  return(data)
}


##'
#' Convertir du texte en snake_case
#'
#' Transforme un vecteur de chaînes (ex. noms de colonnes) en *snake_case* cohérent.
#' Règles appliquées : espaces et tirets → `_`, multiples `_` compressés,
#' Suppression des caractères non alphanumériques (hors `_`), puis passage en minuscules.
#'
#' @param data character Vecteur de chaînes à convertir (p. ex. `names(df)`).
#' @return character Vecteur converti en *snake_case*, de même longueur que `data`.
#' @examples
#' to_snake_case(c("Employee Residence", "Salary-USD", "Company Size"))
#' # [1] "employee_residence" "salary_usd" "company_size"
#' # Renommer des colonnes d'un data.frame :
#' # names(df) <- to_snake_case(names(df))
#' @seealso janitor::make_clean_names, snakecase::to_any_case
#' @export
to_snake_case <- function(data) {
  data <- gsub(" ", "_", data)
  data <- gsub("-", "_", data)
  data <- gsub("__+", "_", data)
  data <- gsub("([a-z])([A-Z])", "\\1_\\2", data)
  data <- gsub("[^a-z0-9_]", "", data)
  data <- tolower(data)
  return(data)
}





#' Convertir automatiquement les colonnes aux types appropriés
#' Cette fonction analyse chaque colonne d'un dataframe et la convertit
#' automatiquement vers le type le plus approprié (numeric, integer, factor ou character).
#'
#' @param data Un dataframe à convertir
#' @param num_threshold Seuil de conversion numérique. Une colonne est convertie
#'   en numérique si au moins ce pourcentage de valeurs sont numériques (défaut: 0.9)
#' @param max_factor_levels Nombre maximum de niveaux uniques pour qu'une colonne
#'   soit convertie en facteur
#'
#' @return Un dataframe avec les colonnes converties aux types appropriés
#'
#' @details
#' La fonction applique les règles suivantes dans l'ordre :
#' \itemize{
#'   \item Si >= num_threshold% des valeurs sont numériques : conversion en numeric ou integer
#'   \item Si <= max_factor_levels valeurs uniques : conversion en factor
#'   \item Sinon : reste en character
#' }
#'
#'
#' @export
enforce_types <- function(data, num_threshold = 0.9, max_factor_levels = 20) {
  #' Convertir automatiquement les colonnes aux types appropriés
  #'
  #' @param data Dataframe à convertir
  #' @param num_threshold Seuil pour conversion numérique (90% des valeurs doivent être numériques)
  #' @param max_factor_levels Nombre maximum de niveaux pour un facteur (défaut: 20)
  #' @return Dataframe avec types convertis
  
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
