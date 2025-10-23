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

# — Supprimer les espaces parasites dans les colonnes texte.
trim_whitespace <- function(dataframe) {
    names(dataframe) <- gsub(' ', '', names(dataframe))
    return(dataframe)
}