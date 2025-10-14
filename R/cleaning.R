file <- "/Users/loic/Documents/UGA/Logiciel Spécialisé/Projet_R/projet-R-Julia/data_raw/ds_salaries.csv"

data <- read.csv2(file, header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8")


print(names(data))
data <- data.frame(data)

data_2020 <- subset(data, work_year == 2020)
data_2021 <- subset(data, work_year == 2021)
data_2022 <- subset(data, work_year == 2022)
data_2023 <- subset(data, work_year == 2023)

print(nrow(data_2020))
print(nrow(data_2021))
print(nrow(data_2022))
print(nrow(data_2023))

print(data)