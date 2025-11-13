#' Normalize a vector to a factor. (use with "df$col <- normalize_to_factor(df$col, mapping, levels)")
#'
#' @param vector the vector to normalize
#' @param mapping the mapping to indicate which values to transform in which.
#' @param levels_in_factor les niveaux à indiquer
#' @param ordered_factor Indicate if the vector has to be ordonned, FALSE by default
#' @return normalized the normalized vector
#' @export
normalize_factor <- function(vector, mapping, levels_in_factor, ordered_factor = FALSE) {
  normalized <- mapping[vector]
  normalized[is.na(normalized)] <- "Unknown"
  normalized <- factor(normalized, levels = levels_in_factor, ordered = ordered_factor)
  return(normalized)
}

#' titre
#'
#' @param data data.frame contenant la colonne `remote_ratio`
#' @param binary logique, si TRUE convertit en 0/100 (par défaut FALSE)
#' @param threshold seuil pour le binaire (défaut 50)
#' @return data.frame avec `remote_ratio` corrigée
#' @export
normalize_remote_ratio <- function(data, binary = FALSE, threshold = 50) {
  if (!"remote_ratio" %in% names(data)) {
    stop("La colonne 'remote_ratio' n'existe pas dans le dataframe.")
  }
  # convert en numeric (les non-convertibles donneront NA)
  data$remote_ratio <- suppressWarnings(as.numeric(as.character(data$remote_ratio)))
  # bornes
  data$remote_ratio <- pmin(pmax(data$remote_ratio, 0), 100)
  if (binary) {
    data$remote_ratio <- ifelse(is.na(data$remote_ratio), NA, ifelse(data$remote_ratio >= threshold, 100, 0))
  }
  return(data)
}


#' Normalize everything of an salary_tbl
#'
#' @param data the salary_tbl to normalize
#' @return data_normalized the normalized salary_tbl
#' @export
normalize_all <- function(data) {
  #Normalize company location and creation of a regionnal grouping.
  data$company_location <- normalize_factor(data$company_location, mapping_total, levels_iso2)
  data$company_grouping <- normalize_factor(data$company_location, region_map, regions_levels)
  #Normalize employee residence and creation of a regionnal grouping.
  data$employee_residence <- normalize_factor(data$employee_residence, mapping_total, levels_iso2)
  data$employee_grouping <- normalize_factor(data$employee_residence, region_map, regions_levels)
  #Normalize job titles
  data$job_title <- normalize_factor(data$job_title, mapping_job_title, levels_job_title)
  #Normalize remote ratios
  data <- normalize_remote_ratio(data)
  #Normalize company sizes
  data$company_size <- normalize_factor(data$company_size, size_mapping, size_levels, TRUE)
  #Normalize employement types
  data$employment_type <- normalize_factor(data$employment_type, mapping_employement_type, levels_employement_type)
  #Normalize experience levels
  data$experience_level <- normalize_factor(data$experience_level, experience_mapping, experience_labels_ordered, TRUE)
  return(data)
}


