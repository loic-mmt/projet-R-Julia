normalize_country_codes <- function(data) {

}

devtools::load_all("~/Documents/UGA/LogSpe/Projet_R/projet-R-Julia/packages/DataCleanR")
data <- DataCleanR::read_raw_csv("data_raw/ds_salaries.csv")
names(data) <- DataCleanR::to_snake_case(names(data))
head(data)


# ---- Utils: lister les valeurs distinctes d'une colonne ---------------------
get_unique <- function(df, col, sort_values = TRUE, na_rm = TRUE) {
  if (!is.data.frame(df)) stop("df doit être un data.frame")
  if (!is.character(col) || length(col) != 1) stop("col doit être un nom de colonne (string)")
  if (!col %in% names(df)) stop(sprintf("Colonne '%s' introuvable. Dispo: %s", col, paste(names(df), collapse = ", ")))
  vals <- unique(df[[col]])
  if (na_rm) vals <- vals[!is.na(vals)]
  if (sort_values) vals <- sort(vals)
  return(vals)
}

# Exemple d'utilisation :
col_to_list <- "company_location"  # change simplement ce nom de colonne
unique_vals <- get_unique(data, col_to_list)
cat(sprintf("\n[%s] %d valeurs distinctes:\n", col_to_list, length(unique_vals)))
print(unique_vals)


"AE" "AM" "AR" "AS" "AT" "AU" "BA" "BE" "BG" "BO" "BR" "CA" "CF" "CH" "CL"
"CN" "CO" "CR" "CY" "CZ" "DE" "DK" "DO" "DZ" "EE" "EG" "ES" "FI" "FR" "GB"
"GH" "GR" "HK" "HN" "HR" "HU" "ID" "IE" "IL" "IN" "IQ" "IR" "IT" "JE" "JP"
"KE" "KW" "LT" "LU" "LV" "MA" "MD" "MK" "MT" "MX" "MY" "NG" "NL" "NZ" "PH"
"PK" "PL" "PR" "PT" "RO" "RS" "RU" "SE" "SG" "SI" "SK" "TH" "TN" "TR" "UA"
"US" "UZ" "VN"

"AE" "AL" "AM" "AR" "AS" "AT" "AU" "BA" "BE" "BO" "BR" "BS" "CA" "CF" "CH"
"CL" "CN" "CO" "CR" "CZ" "DE" "DK" "DZ" "EE" "EG" "ES" "FI" "FR" "GB" "GH"
"GR" "HK" "HN" "HR" "HU" "ID" "IE" "IL" "IN" "IQ" "IR" "IT" "JP" "KE" "LT"
"LU" "LV" "MA" "MD" "MK" "MT" "MX" "MY" "NG" "NL" "NZ" "PH" "PK" "PL" "PR"
"PT" "RO" "RU" "SE" "SG" "SI" "SK" "TH" "TR" "UA" "US" "VN"