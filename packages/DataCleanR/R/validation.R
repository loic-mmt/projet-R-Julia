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
  data <- gsub("[^a-z0-9_]", "", data)
  data <- tolower(data)
  return(data)
}