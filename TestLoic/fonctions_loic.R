devtools::load_all("~/Documents/UGA/LogSpe/Projet_R/projet-R-Julia/packages/DataCleanR")
data <- DataCleanR::read_raw_csv("ds_salaries.csv")
names(data) <- DataCleanR::to_snake_case(names(data))
head(data)





res <- association_table(data, x = "company_location", y = "employment_type", prop = "row")
res$table       # effectifs
res$prop        # % par ligne (si demandé)
res$v           # Cramér's V
res$p.value     # p-value du test du χ²