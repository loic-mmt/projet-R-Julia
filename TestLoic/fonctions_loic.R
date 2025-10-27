devtools::load_all("~/Documents/UGA/LogSpe/Projet_R/projet-R-Julia/packages/DataCleanR")
data <- DataCleanR::read_raw_csv("ds_salaries.csv")
names(data) <- DataCleanR::to_snake_case(names(data))
head(data)

limit <- 0.99
nb_concerned <- 0

for (i in seq_along(data$salary_in_usd)) {
  if ((mean(data$salary_in_usd) * (1 - limit)) <= data$salary_in_usd[i] & data$salary_in_usd[i] <= mean((data$salary_in_usd) * (1 + limit))) {
    nb_concerned <- nb_concerned + 0
  } else {
    nb_concerned <- nb_concerned + 1
  }
}
nb_concerned
mean(data$salary_in_usd)
max(data$salary_in_usd)
min(data$salary_in_usd)
