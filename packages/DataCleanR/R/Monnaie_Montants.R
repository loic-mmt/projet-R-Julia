#' @title Convertir les salaires en USD
#' @description Convertit les salaires de différentes devises en USD en utilisant des taux de change historiques annuels
#' @param data Data frame contenant les colonnes salary, salary_currency, salary_in_usd et work_year
#' @return Data frame avec les salaires convertis en USD
#' @export
convert_currency_to_usd <- function(data) {
  if (!all(c("salary", "salary_currency", "salary_in_usd", "work_year") %in% names(data))) {
    stop("Les colonnes 'salary', 'salary_currency', 'salary_in_usd' et 'work_year' doivent exister.")
  }
  data$salary <- as.numeric(as.character(data$salary))
  data$salary_currency <- as.character(data$salary_currency)
  data$work_year <- as.integer(data$work_year)
  
  # Liste étendue des devises
  currencies <- c(
    "AUD", "BRL", "CAD", "CHF", "CLP", "CZK", "DKK", "EUR",
    "GBP", "HKD", "HUF", "ILS", "INR", "JPY", "MXN", "NOK",
    "SGD", "THB", "TRY", "USD", "AED", "ARS", "BGN", "CNY",
    "COP", "EGP", "IDR", "KRW", "MAD", "MYR", "NZD", "PHP",
    "PKR", "PLN", "RON", "RUB", "SAR", "SEK", "TWD", "VND",
    "ZAR"
  )
  
  # Taux de change annuels vers USD pour 2020-2024
  exchange_rates <- data.frame(
      year = rep(2020:2023, each = length(currencies)),
      currency = rep(currencies, times = 4),
      rate = c(
        # 2020
        0.65,0.18,0.75,1.10,0.0013,0.043,0.16,0.88,0.75,0.13,0.0028,0.28,0.013,0.0095,0.053,0.11,0.72,0.029,0.029,1.00,0.27,0.014,0.58,0.14,0.00047,0.032,0.000069,0.00083,0.11,0.24,0.64,0.021,0.0061,0.26,0.23,0.013,0.27,0.11,0.033,0.000043,0.063,
        # 2021
        0.70,0.19,0.79,1.12,0.0014,0.045,0.15,0.85,0.73,0.13,0.0029,0.28,0.012,0.0091,0.055,0.12,0.74,0.030,0.028,1.00,0.27,0.010,0.57,0.15,0.00049,0.031,0.000070,0.00085,0.11,0.24,0.68,0.020,0.0059,0.24,0.22,0.014,0.27,0.12,0.036,0.000044,0.065,
        # 2022
        0.68,0.20,0.78,1.14,0.0013,0.046,0.15,0.95,0.76,0.13,0.0025,0.29,0.012,0.0076,0.054,0.11,0.73,0.031,0.030,1.00,0.27,0.0074,0.54,0.14,0.00048,0.050,0.000064,0.00078,0.10,0.22,0.62,0.018,0.0057,0.22,0.20,0.017,0.27,0.10,0.033,0.000042,0.058,
        # 2023 
        0.66,0.21,0.77,1.15,0.0013,0.045,0.16,0.92,0.75,0.13,0.0027,0.28,0.012,0.00822,0.053,0.12,0.72,0.029,0.029,1.00,0.27,0.0028,0.53,0.14,0.00046,0.032,0.000065,0.00076,0.10,0.21,0.61,0.018,0.0056,0.23,0.21,0.011,0.27,0.10,0.032,0.000041,0.053
             )
  )
    
    data <- merge(data, exchange_rates,
                  by.x = c("salary_currency", "work_year"),
                  by.y = c("currency", "year"),
                  all.x = TRUE)
    
    data$salary_in_usd <- data$salary * data$rate
    data$calculated_usd <- NULL
    data$rate <- NULL
    
    return(data)
}
