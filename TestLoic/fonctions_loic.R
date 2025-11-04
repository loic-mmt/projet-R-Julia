devtools::load_all("~/Documents/UGA/LogSpe/Projet_R/projet-R-Julia/packages/DataCleanR")
data <- DataCleanR::read_raw_csv("ds_salaries.csv")
names(data) <- DataCleanR::to_snake_case(names(data))
head(data)


effect_size_salary <- function(data,
                                      y,
                                      group,
                                      method = c("levene", "bartlett"),
                                      na.rm = TRUE) {
  df <- data.frame(company_location = c("US", "US", "US", "FR"),
                   salary_in_usd = c(100, 200, 1000, 2000),
                   stringsAsFactors = FALSE)
}