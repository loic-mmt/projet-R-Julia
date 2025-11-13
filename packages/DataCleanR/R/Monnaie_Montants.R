#' @title Convertir les salaires en USD
#' @description Convertit les salaires de différentes devises en USD en utilisant des taux de change historiques annuels
#' @param data Data frame contenant les colonnes salary, salary_currency, salary_in_usd et work_year
#' @return Data frame avec les salaires convertis en USD
#' @export
convert_currency_to_usd <- function(data) {
  # Ignore linter warning - exchange_rates_to_usd is exported from Mapping.R
  # nolint start: object_usage_linter
  if (!all(c("salary", "salary_currency", "salary_in_usd", "work_year") %in% names(data))) {
    stop("Les colonnes 'salary', 'salary_currency', 'salary_in_usd' et 'work_year' doivent exister.")
  }
  data$salary <- as.numeric(as.character(data$salary))
  data$salary_currency <- as.character(data$salary_currency)
  data$work_year <- as.integer(data$work_year)

  # Utilisation des données de mapping exportées
  data <- merge(data, exchange_rates_to_usd,
                by.x = c("salary_currency", "work_year"),
                by.y = c("currency", "year"),
                all.x = TRUE)

  data$salary_in_usd <- data$salary * data$rate
  data$calculated_usd <- NULL
  data$rate <- NULL

  return(data)
}