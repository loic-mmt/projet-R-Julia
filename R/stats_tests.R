devtools::load_all("/home/arthur/Desktop/projet-R-Julia/packages/DataCleanR")
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
col_to_list <- "job_title"  # change simplement ce nom de colonne
unique_vals <- get_unique(data, col_to_list)
cat(sprintf("\n[%s] %d valeurs distinctes:\n", col_to_list, length(unique_vals)))
print(unique_vals)
