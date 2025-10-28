export_csv <- function(data,
                       path = "exports",
                       filename = "cleaned_data",
                       sep = ";",
                       row.names = FALSE,
                       overwrite = TRUE,
                       verbose = TRUE) {
  if (!is.data.frame(data)) stop("data doit être un data.frame")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  if (!grepl("\\.csv$", filename, ignore.case = TRUE)) filename <- paste0(filename, ".csv")
  filepath <- file.path(path, filename)

  if (!overwrite && file.exists(filepath)) stop(sprintf("Le fichier existe déjà : %s", filepath))

  if (sep == ";") {
    utils::write.csv2(data, file = filepath, row.names = row.names)
  } else if (sep == ",") {
    utils::write.csv(data, file = filepath, row.names = row.names)
  } else {
    utils::write.table(data, file = filepath, sep = sep, dec = ".", row.names = row.names, col.names = TRUE, qmethod = "double")
  }

  if (verbose) message(sprintf("Fichier écrit: %s", normalizePath(filepath)))
  invisible(filepath)
}