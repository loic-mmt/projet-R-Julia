library(dplyr)
file <- "data_raw/ds_salaries.csv"

data <- read.csv2(file, header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8")


print(names(data))
data <- data.frame(data)
data <- data[, -5]
sum(is.na(as.double(data$salary_in_usd))) 

df <- data %>%
  mutate(salary_in_usd = as.double(salary_in_usd),
         remote_ratio = as.integer(remote_ratio),
         experience_level = factor(experience_level, levels = c("EN", "MI", "SE", "EX")),
         employment_type = factor(employment_type, levels = c("PT", "CT", "FT", "FL")),
         company_size = factor(company_size, levels = c("S", "M", "L")),
         work_year = factor(work_year, levels = c(2020, 2021, 2022, 2023))
  )

head(df)

data_2020 <- subset(data, work_year == 2020)
data_2021 <- subset(data, work_year == 2021)
data_2022 <- subset(data, work_year == 2022)
data_2023 <- subset(data, work_year == 2023)

print(nrow(data_2020))
print(nrow(data_2021))
print(nrow(data_2022))
print(nrow(data_2023))