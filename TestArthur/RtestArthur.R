# Normaliser {FT, PT, CT, FL} en libellés/facteur.
normalize_employment_type <- function(x) {
  mapping <- c(
    "FT" = "Full-time",
    "PT" = "Part-time",
    "CT" = "Contract",
    "FL" = "Freelance"
  )
  normalized <- mapping[x]
  normalized[is.na(normalized)] <- "Unknown"
  normalized <- factor(
    normalized,
    levels = c("Full-time", "Part-time", "Contract", "Freelance", "Unknown")
  )
  return(normalized)
}
