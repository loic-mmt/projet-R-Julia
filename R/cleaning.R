data <- read.csv2()("/Users/loic/Documents/UGA/Logiciel Spécialisé/Projet_R/projet-R-Julia/data_raw/ds_salaries.csv")
print(data)
print(row.names(data))
data <- data.frame(data)

print(data[3:8, 4])
data_2021 <- subset(data, work_year == "2021")
print(data_2021)
