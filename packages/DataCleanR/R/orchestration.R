cleaning_pipeline <- function(data, min_year = 2000, max_year = as.integer(format(Sys.Date(), "%Y")), verbose = TRUE) {
  
  if (verbose) message("=== Début du pipeline de nettoyage ===")
  
  initial_rows <- nrow(data)
  if (verbose) message(sprintf("Nombre de lignes initial : %d", initial_rows))
  
  # Étape 1 : Validation du schéma
  if (verbose) message("\n[1/10] Validation du schéma...")
  data <- validate_schema(data)
  if (verbose) message(sprintf("  -> Schéma validé"))
  
  # Étape 2 : Standardiser les noms de colonnes
  if (verbose) message("\n[2/10] Standardisation des noms de colonnes...")
  data <- standardize_colnames(data)
  if (verbose) message(sprintf("  -> Colonnes standardisées"))
  
  # Étape 3 : Nettoyer les espaces
  if (verbose) message("\n[3/10] Nettoyage des espaces parasites...")
  data <- trim_whitespace(data)
  if (verbose) message(sprintf("  -> Espaces nettoyés"))
  
  # Étape 4 : Appliquer les types
  if (verbose) message("\n[4/10] Application des types de données...")
  data <- enforce_types(data)
  if (verbose) message(sprintf("  -> Types appliqués"))
  
  # Étape 5 : Supprimer les doublons
  if (verbose) message("\n[5/10] Suppression des doublons...")
  data <- deduplicate_rows(data)
  if (verbose) message(sprintf("  -> Lignes restantes : %d", nrow(data)))
  
  # Étape 6 : Normaliser les niveaux d'expérience
  if (verbose) message("\n[6/10] Normalisation des niveaux d'expérience...")
  data <- normalize_experience_level(data)
  if (verbose) message(sprintf("  -> Normalisé"))
  
  # Étape 7 : Normaliser les types d'emploi
  if (verbose) message("\n[7/10] Normalisation des types d'emploi...")
  data <- normalize_employment_type(data)
  if (verbose) message(sprintf("  -> Normalisé"))
  
  # Étape 8 : Normaliser la taille d'entreprise
  if (verbose) message("\n[8/10] Normalisation de la taille d'entreprise...")
  data <- normalize_company_size(data)
  if (verbose) message(sprintf("  -> Normalisé"))
  
  # Étape 9 : Normaliser le ratio remote
  if (verbose) message("\n[9/10] Normalisation du ratio remote...")
  data <- normalize_remote_ratio(data)
  if (verbose) message(sprintf("  -> Normalisé"))
  
  # Étape 10 : Normaliser les titres de poste
  if (verbose) message("\n[10/10] Normalisation des titres de poste...")
  data <- normalize_job_title(data)
  if (verbose) message(sprintf("  -> Normalisé"))
  
  # Étape 11 : Normaliser les codes pays
  if (verbose) message("\n[11/13] Normalisation des codes pays...")
  data <- normalize_country_codes(data)
  if (verbose) message(sprintf("  -> Normalisé"))
  
  # Étape 12 : Valider les plages
  if (verbose) message("\n[12/13] Validation des plages de valeurs...")
  data <- validate_ranges(data, min_year = min_year, max_year = max_year)
  if (verbose) message(sprintf("  -> Lignes restantes : %d", nrow(data)))
  
  # Étape 13 : Convertir les devises
  if (verbose) message("\n[13/13] Conversion des devises en USD...")
  data <- convert_currency_to_usd(data)
  if (verbose) message(sprintf("  -> Conversion terminée"))
  
  final_rows <- nrow(data)
  removed_rows <- initial_rows - final_rows
  
  if (verbose) {
    message("\n=== Pipeline terminé ===")
    message(sprintf("Lignes initiales : %d", initial_rows))
    message(sprintf("Lignes finales   : %d", final_rows))
    message(sprintf("Lignes supprimées: %d (%.1f%%)", removed_rows, (removed_rows/initial_rows)*100))
  }
  
  return(data)
}

#' Create a salary table object
#'
#' @param data a data frame containing salary data
#' @return An object of class "salary_tbl"
#' @export
finalize_salary_tbl <- function(data) {
  # Vérification des colonnes
  required_cols <- c("work_year", "experience_level", "employment_type", "job_title", 
                    "salary", "salary_currency", "salary_in_usd", "employee_residence", 
                    "remote_ratio", "company_location", "company_size")
  if (validate_schema(data, required_cols, TRUE)) {
    class(data) <- c("salary_tbl", class(data))
    return(data)
  }
  else {
    stop(validate_schema(data, required_cols))
  }
}