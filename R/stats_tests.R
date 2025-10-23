library(DataCleanR)
data <- read_raw_csv("data_raw/ds_salaries.csv")
head(data)
names(data) <- to_snake_case(names(data))
