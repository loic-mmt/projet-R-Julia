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