devtools::load_all("~/Documents/UGA/LogSpe/Projet_R/projet-R-Julia/packages/DataCleanR")
data <- DataCleanR::read_raw_csv("ds_salaries.csv")
names(data) <- DataCleanR::to_snake_case(names(data))
head(data)


