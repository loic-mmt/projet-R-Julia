#' Import CSV as data.frame
#' @name read_raw_csv
#' @param file_path chemin vers le CSV
#' @return data.frame
#' @export
read_raw_csv <- function(file_path) {
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  return(data)
}


#' Test the presence of required colums in a dataframe
#'
#' @param dataframe Dataframe to test
#' @param required_colums vector with the required colums names to test in dataframe
#' @return presence Indication to the user on the preence of the colums
#' @export
validate_schema <- function(dataframe, required_colums) {
    not_commun <- required_colums[!required_colums %in% names(dataframe)]
    presence <- ""
    if (length(not_commun) == 0) {
        presence <- "All required colums are present in the dataframe"
    }
    else {
        presence <- paste("The dataframe in not complete and it's missing", paste(not_commun, collapse = ", "))
    }
    return(presence)
}

#' Convert strings to snake_case
#' @name to_snake_case
#' @param data character Vector of strings to convert.
#' @return character Vector in snake_case.
#' @export
to_snake_case <- function(data) {
  data <- gsub("[^A-Za-z0-9]+", "_", data)
  data <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", data)
  data <- tolower(data)
  data <- gsub("_+", "_", data)
  data <- gsub("^_+|_+$", "", data)
  return(data)
}

#' Enforce simple column types
#'
#' @param data data.frame Input data
#' @param num_threshold numeric Proportion threshold for numeric conversion (default 0.9)
#' @param max_factor_levels integer Max distinct levels to convert to factor (default 20)
#' @return data.frame Data with basic types enforced
#' @export
enforce_types <- function(data, num_threshold = 0.9, max_factor_levels = 20) {
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


#' Deduplicate data
#'
#' @param data data to deduplicate.
#' @param keys character|null Columns used as unique key; default NULL = all columns.
#' @param keep character Keep "first" or "last" occurrence when duplicates exist.
#' @return deduplicated data.
#' @export
deduplicate_rows <- function(data, keys = NULL, keep = c("first", "last")) {
  keep <- match.arg(keep)

  if (!is.data.frame(data)) {
    stop("'data' doit être un data.frame")
  }
  if (is.null(keys)) {
    keys <- names(data)
  } else {
    inconnues <- setdiff(keys, names(data))
    if (length(inconnues) > 0) {
      stop("Clés inconnues: ", paste(inconnues, collapse = ", "))
    }
  }
  from_last <- identical(keep, "last")
  dup <- duplicated(data[keys], fromLast = from_last)

  out <- data[!dup, , drop = FALSE]

  attr(out, "n_removed") <- sum(dup)
  return(out)
}