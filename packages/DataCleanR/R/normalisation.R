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

#'
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