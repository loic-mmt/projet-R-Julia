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