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


#' Normalize country codes to ISO-3166 alpha-2
#'
#' Standardise a country column to ISO2 (uppercase, trimmed), optionally add a
#' simple continent label, and validate values. Handles common synonyms and
#' converts from ISO3 or country names using internal lookup tables (no external deps).
#'
#' @param data A data.frame containing the column to normalize.
#' @param col Name of the input column in `data` (default: "company_location").
#' @param output_col Name of the output column to write (default: same as `col`).
#' @param add_region Logical; add a continent column (default: TRUE).
#' @param region_col Name of the continent column to create when `add_region=TRUE`.
#' @param strict Logical; if TRUE, stop on unknown/non-standard codes after normalization.
#' @param verbose Logical; if TRUE, print messages about changes and anomalies.
#'
#' @return The input `data` with a normalized ISO2 column (and optional region).
#' @export
normalize_country_codes <- function(data,
                                    col = "company_location",
                                    output_col = col,
                                    add_region = TRUE,
                                    region_col = "company_region",
                                    strict = FALSE,
                                    verbose = TRUE) {

  if (!is.data.frame(data)) stop("data doit être un data.frame")
  if (!col %in% names(data)) stop(sprintf("Colonne '%s' introuvable dans data", col))

  v_in <- as.character(data[[col]])
  v <- toupper(trimws(v))


  synonyms <- c(
    "UK" = "GB",
    "EL" = "GR",
    "KO" = "XK",
    "XKX" = "XK"
  )
  v <- ifelse(v %in% names(synonyms), unname(synonyms[v]), v)

  iso3_to_iso2 <- c(
    "ARE"="AE","ALB"="AL","ARM"="AM","ARG"="AR","ASM"="AS","AUT"="AT","AUS"="AU","BIH"="BA","BEL"="BE",
    "BOL"="BO","BRA"="BR","BHS"="BS","CAN"="CA","CAF"="CF","CHE"="CH","CHL"="CL","CHN"="CN","COL"="CO",
    "CRI"="CR","CYP"="CY","CZE"="CZ","DEU"="DE","DNK"="DK","DOM"="DO","DZA"="DZ","EST"="EE","EGY"="EG",
    "ESP"="ES","FIN"="FI","FRA"="FR","GBR"="GB","GHA"="GH","GRC"="GR","HKG"="HK","HND"="HN","HRV"="HR",
    "HUN"="HU","IDN"="ID","IRL"="IE","ISR"="IL","IND"="IN","IRQ"="IQ","IRN"="IR","ITA"="IT","JEY"="JE",
    "JPN"="JP","KEN"="KE","KWT"="KW","LTU"="LT","LUX"="LU","LVA"="LV","MAR"="MA","MDA"="MD","MKD"="MK",
    "MLT"="MT","MEX"="MX","MYS"="MY","NGA"="NG","NLD"="NL","NZL"="NZ","PHL"="PH","PAK"="PK","POL"="PL",
    "PRI"="PR","PRT"="PT","ROU"="RO","SRB"="RS","RUS"="RU","SWE"="SE","SGP"="SG","SVN"="SI","SVK"="SK",
    "THA"="TH","TUN"="TN","TUR"="TR","UKR"="UA","USA"="US","UZB"="UZ","VNM"="VN","ZAF"="ZA"
  )


  name_to_iso2 <- c(
    "UNITED STATES"="US","UNITED KINGDOM"="GB","GREAT BRITAIN"="GB","ENGLAND"="GB",
    "CZECH REPUBLIC"="CZ","CZECHIA"="CZ","RUSSIA"="RU","REPUBLIC OF KOREA"="KR","SOUTH KOREA"="KR",
    "NORTH KOREA"="KP","VIETNAM"="VN","VIET NAM"="VN","HONG KONG"="HK","TAIWAN"="TW",
    "UNITED ARAB EMIRATES"="AE","UAE"="AE","SAUDI ARABIA"="SA","TURKEY"="TR","TÜRKIYE"="TR",
    "BOSNIA AND HERZEGOVINA"="BA","NORTH MACEDONIA"="MK","MACEDONIA"="MK","BOLIVIA"="BO",
    "MOLDOVA"="MD","REPUBLIC OF MOLDOVA"="MD","LAOS"="LA","LAO PDR"="LA","CAPE VERDE"="CV",
    "IVORY COAST"="CI","CÔTE D'IVOIRE"="CI","SOUTH AFRICA"="ZA"
  )

  allowed_iso2 <- unique(c(

    "AL","AM","AT","BA","BE","BG","BY","CH","CY","CZ","DE","DK","EE","ES","FI","FR","GB","GR","HR","HU","IE","IS","IT","JE","LT","LU","LV","MD","MK","MT","NL","NO","PL","PT","RO","RS","RU","SE","SI","SK","UA",

    "AR","BO","BR","BS","CA","CL","CO","CR","DO","MX","PR","US","VE","UY","PE","EC","BZ","PA",

    "CF","DZ","EG","GH","KE","MA","NG","TN","ZA","CI","SN","TZ","UG","ET","CM","DZ","TN","MA",

    "AE","AS","BD","CN","HK","ID","IL","IN","IQ","IR","JP","KW","KR","KZ","LA","MY","PH","PK","QA","SA","SG","TH","TR","VN","XK","UZ","NZ","AU"
  ))


  idx_iso3 <- which(v %in% names(iso3_to_iso2))
  if (length(idx_iso3)) v[idx_iso3] <- unname(iso3_to_iso2[v[idx_iso3]])

  idx_name <- which(v %in% names(name_to_iso2))
  if (length(idx_name)) v[idx_name] <- unname(name_to_iso2[v[idx_name]])


  bad_fmt <- which(!grepl("^[A-Z]{2}$", v) | is.na(v))
  bad_val <- which(!(v %in% allowed_iso2) & !is.na(v))
  bad <- sort(unique(c(bad_fmt, bad_val)))

  if (length(bad)) {
    if (strict) {
      stop(sprintf("Codes pays invalides aux lignes: %s (exemples: %s)",
                   paste(utils::head(bad, 10), collapse = ", "),
                   paste(utils::head(unique(v[bad]), 5), collapse = ", ")))
    } else if (verbose) {
      message(sprintf("normalize_country_codes: %d valeur(s) non standard détectée(s), conservée(s) telle(s) quelle(s). Exemples: %s",
                      length(bad), paste(utils::head(unique(v[bad]), 5), collapse = ", ")))
    }
  }

  changed <- sum(v != toupper(trimws(as.character(v_in))), na.rm = TRUE)
  if (verbose && changed) message(sprintf("normalize_country_codes: %d valeur(s) modifiée(s).", changed))


  data[[output_col]] <- v

  if (add_region) {

    region_map <- c(

      setNames(rep("Europe", length.out = 0), character(0)),
      "AL"="Europe","AM"="Asia","AT"="Europe","BA"="Europe","BE"="Europe","BG"="Europe","BY"="Europe","CH"="Europe",
      "CY"="Europe","CZ"="Europe","DE"="Europe","DK"="Europe","EE"="Europe","ES"="Europe","FI"="Europe","FR"="Europe",
      "GB"="Europe","GR"="Europe","HR"="Europe","HU"="Europe","IE"="Europe","IS"="Europe","IT"="Europe","JE"="Europe",
      "LT"="Europe","LU"="Europe","LV"="Europe","MD"="Europe","MK"="Europe","MT"="Europe","NL"="Europe","NO"="Europe",
      "PL"="Europe","PT"="Europe","RO"="Europe","RS"="Europe","RU"="Europe","SE"="Europe","SI"="Europe","SK"="Europe","UA"="Europe",

      "AR"="South America","BO"="South America","BR"="South America","BS"="North America","CA"="North America",
      "CL"="South America","CO"="South America","CR"="North America","DO"="North America","MX"="North America",
      "PR"="North America","US"="North America","VE"="South America","UY"="South America","PE"="South America","EC"="South America","BZ"="North America","PA"="North America",

      "CF"="Africa","DZ"="Africa","EG"="Africa","GH"="Africa","KE"="Africa","MA"="Africa","NG"="Africa","TN"="Africa","ZA"="Africa","CI"="Africa","SN"="Africa","TZ"="Africa","UG"="Africa","ET"="Africa","CM"="Africa",

      "AE"="Asia","AS"="Oceania","BD"="Asia","CN"="Asia","HK"="Asia","ID"="Asia","IL"="Asia","IN"="Asia","IQ"="Asia","IR"="Asia",
      "JP"="Asia","KW"="Asia","KR"="Asia","KZ"="Asia","LA"="Asia","MY"="Asia","PH"="Asia","PK"="Asia","QA"="Asia","SA"="Asia",
      "SG"="Asia","TH"="Asia","TR"="Asia","VN"="Asia","XK"="Europe","UZ"="Asia","NZ"="Oceania","AU"="Oceania"
    )
    reg <- unname(region_map[ v ])
    reg[is.na(reg)] <- NA_character_
    data[[region_col]] <- reg
  }

  return(data)
}
