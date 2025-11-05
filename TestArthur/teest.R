cleaning_pipeline <- function(data, min_year = 2000, max_year = as.integer(format(Sys.Date(), "%Y")), verbose = TRUE) {

  if (verbose) message("Début du pipeline de nettoyage")
  initial_rows <- nrow(data)
  if (verbose) message(sprintf("Nombre de lignes initial : %d", initial_rows))

  # Définir les colonnes requises
  CORE_COLUMNS <- c(
    "work_year", "experience_level", "employment_type", "job_title",
    "salary", "salary_currency", "salary_in_usd"
  )

  # Étape 1 : Validation du schéma
  if (verbose) message("\n[1/13] Validation du schéma...")

  schema_validation <- DataCleanR::validate_schema(data, required_columns = CORE_COLUMNS, boolean_form = TRUE)

  if (!schema_validation) {
    error_msg <- DataCleanR::validate_schema(data, required_columns = CORE_COLUMNS, boolean_form = FALSE)
    stop(error_msg)
  }

  if (verbose) message(sprintf("  -> Schéma validé : %s", DataCleanR::validate_schema(data, required_columns = CORE_COLUMNS)))

  # Étape 2 : Standardiser les noms de colonnes
  if (verbose) message("\n[2/13] Standardisation des noms de colonnes...")
  data <- DataCleanR::standardize_colnames(data)
  if (verbose) message("  -> Colonnes standardisées")

  # Étape 3 : Appliquer les types
  if (verbose) message("\n[4/13] Application des types de données...")
  data <- DataCleanR::enforce_types(data)
  if (verbose) message("  -> Types appliqués")

 # Étape 4 : Supprimer les doublons
if (verbose) message("\n[5/13] Suppression des doublons...")
data <- as.data.frame(data)
data <- DataCleanR::deduplicate_rows(data)
if (verbose) message(sprintf("  -> Lignes restantes : %d", nrow(data)))

  # Étapes 5 à 9 : Normalisations
  if (verbose) message("\n[6/13] Normalisation des niveaux d'expérience...")
  data <- DataCleanR::normalize_experience_level(data)
  if (verbose) message("  -> Normalisé")

  if (verbose) message("\n[7/13] Normalisation des types d'emploi...")
  data <- DataCleanR::normalize_employment_type(data)
  if (verbose) message("  -> Normalisé")

  if (verbose) message("\n[8/13] Normalisation de la taille d'entreprise...")
  data <- DataCleanR::normalize_company_size(data)
  if (verbose) message("  -> Normalisé")

  if (verbose) message("\n[9/13] Normalisation du ratio remote...")
  data <- DataCleanR::normalize_remote_ratio(data)
  if (verbose) message("  -> Normalisé")

  if (verbose) message("\n[10/13] Normalisation des titres de poste...")
  data <- DataCleanR::normalize_job_title(data)
  if (verbose) message("  -> Normalisé")

  # Étape 10 : Normaliser les codes pays
  if (verbose) message("\n[11/13] Normalisation des codes pays...")
  data <- DataCleanR::normalize_country_codes(data)
  if (verbose) message("  -> Normalisé")

  # Étape 11 : Valider les plages
  if (verbose) message("\n[12/13] Validation des plages de valeurs...")
  data <- DataCleanR::validate_ranges(data, min_year = min_year, max_year = max_year)
  if (verbose) message(sprintf("  -> Lignes restantes : %d", nrow(data)))

  # Étape 12 : Conversion des devises en USD
  if (verbose) message("\n[13/13] Conversion des devises en USD...")
  data <- DataCleanR::convert_currency_to_usd(data)
  if (verbose) message("  -> Conversion terminée")

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