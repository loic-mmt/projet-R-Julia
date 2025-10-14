file <- "/Users/loic/Documents/UGA/Logiciel Spécialisé/Projet_R/projet-R-Julia/data_raw/ds_salaries.csv"
# Peek first line to guess separator
first <- readLines(file, n = 1, warn = FALSE)
sep <- if (grepl(";", first)) ";" else if (grepl("\t", first)) "\t" else ","

# Read with the right reader
if (sep == ";") {
  data <- read.csv2(file, header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8")
} else if (sep == "\t") {
  data <- read.delim(file, header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8")
} else {
  data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8")
}

# Quick diagnostics
cat("Separator detected:", if (sep == "\t") "[TAB]" else sep, "\n")
cat("Dimensions:", paste(dim(data), collapse = " x "), "\n")
print(names(data))
head(data, 3)
print(data)
data <- data.frame(data)

if ("work_year" %in% names(data)) {
  if (is.numeric(data$work_year)) {
    data_2021 <- subset(data, work_year == 2021)
  } else {
    data_2021 <- subset(data, work_year == "2021")
  }
  print(data_2021)
} else {
  warning("La colonne 'work_year' est introuvable dans les données importées.")
}
