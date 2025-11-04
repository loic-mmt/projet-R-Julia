#' @title Valider les plages de valeurs
#' @description Vérifie que les données respectent les contraintes de plage (salary > 0, remote_ratio entre 0 et 100, années plausibles)
#' @param data Data frame contenant les colonnes à valider
#' @param min_year Année minimale acceptable (par défaut 2000)
#' @param max_year Année maximale acceptable (par défaut année courante)
#' @return Data frame filtré contenant uniquement les lignes valides
#' @export

validate_ranges <- function(data, min_year = 2000, max_year = as.integer(format(Sys.Date(), "%Y"))) {
  if (!all(c("salary", "remote_ratio", "work_year") %in% names(data))) {
    stop("Les colonnes 'salary', 'remote_ratio' et 'work_year' doivent exister.")
  }
  
  data$salary <- as.numeric(data$salary)
  data$remote_ratio <- as.numeric(data$remote_ratio)
  data$work_year <- as.integer(data$work_year)
  
  initial_rows <- nrow(data)
  
  # Filtrer les lignes valides
  valid_data <- data[
    data$salary > 0 &
      data$remote_ratio >= 0 &
      data$remote_ratio <= 100 &
      data$work_year >= min_year &
      data$work_year <= max_year &
      !is.na(data$salary) &
      !is.na(data$remote_ratio) &
      !is.na(data$work_year),
  ]
  
  removed_rows <- initial_rows - nrow(valid_data)
  
  if (removed_rows > 0) {
    message(sprintf("%d ligne(s) supprimée(s) car hors des plages valides.", removed_rows))
  }
  
  return(valid_data)
}


#' Cap salary outliers (quantile)
#'
#' Winsorise une colonne numérique en plafonnant par quantiles.
#' On calcule \code{L = quantile(x, lower)} et \code{U = quantile(x, upper)}
#' puis on remplace les valeurs en dehors de \code{[L, U]} (selon \code{clip_side}).
#' Retourne le data.frame filtré.
#'
#' @param data data.frame.
#' @param col Nom de la colonne numérique à traiter (défaut: "salary_in_usd").
#' @param lower Probabilité du quantile bas (défaut 0.01).
#' @param upper Probabilité du quantile haut (défaut 0.99).
#' @param clip_side "both" (défaut), "upper" ou "lower".
#' @param na_rm logique; ignorer les NA pour le calcul des quantiles (défaut TRUE).
#' @param verbose logique; afficher un résumé des opérations.
#'
#' @return Le `data.frame` modifié (mêmes dimensions, valeurs extrêmes filtrées).
#' @export
cap_outliers_salary <- function(data,
                                col = "salary_in_usd",
                                lower = 0.01,
                                upper = 0.99,
                                clip_side = c("both", "upper", "lower"),
                                na_rm = TRUE,
                                verbose = TRUE) {
  if (!is.data.frame(data)) stop("data doit être un data.frame")
  if (!col %in% names(data)) stop(sprintf("Colonne '%s' introuvable", col))
  if (!is.numeric(data[[col]])) stop(sprintf("'%s' doit être numérique", col))

  clip_side <- match.arg(clip_side)

  caps_quantile <- function(x) {
    L <- as.numeric(stats::quantile(x, probs = lower, na.rm = na_rm, type = 7))
    U <- as.numeric(stats::quantile(x, probs = upper, na.rm = na_rm, type = 7))
    c(lower = L, upper = U)
  }
  apply_caps <- function(x, caps) {
    L <- caps[["lower"]]; U <- caps[["upper"]]
    xi <- x
    idx_low <- !is.na(xi) & xi < L
    idx_up  <- !is.na(xi) & xi > U
    if (clip_side %in% c("both","lower")) xi[idx_low] <- L
    if (clip_side %in% c("both","upper")) xi[idx_up]  <- U
    list(x = xi, n_low = sum(idx_low), n_up = sum(idx_up))
  }

  x <- data[[col]]
  caps <- caps_quantile(x)
  res  <- apply_caps(x, caps)
  data[[col]] <- res$x

  if (verbose) message(sprintf(
    "cap_outliers_salary: %d bas / %d haut  (caps [%g, %g])",
    res$n_low, res$n_up, caps[["lower"]], caps[["upper"]]))

  data
}