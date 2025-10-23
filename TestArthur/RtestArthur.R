validate_schema <- function(dataframe, required_colums) {
    not_commun <- required_colums[!required_colums %in% names(dataframe)]
    presence <- ""
    if (lenght(not_commun) == 0) {
        presence <- "All required colums are present in the dataframe"
    }
    else {
        presence <- paste("The dataframe in not complete and it's missing", paste(not_commun, collapse = ", "))
    }
    return(presence)
}

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


