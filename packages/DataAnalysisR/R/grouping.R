#'summarize_salary - mean/median/IQR
#' @param applyto nom de colonne à laquelle appliquer la moyenne/medianne...
#' @param method moyenne/median/iqr.
#' @param by nom des colonnes à grouper pour appliquer.
#' @param na.rm TRUE, enlève les valeurs manquantes
#' @param verbose afficher un résumé des opérations.
#'
#' @return Le résultat par méthode groupé.
#' @export
summarize_salary <- function(x,
                             applyto = "salary_in_usd",
                             method = c("median", "iqr", "mean"),
                             by = "work_year",
                             na.rm = TRUE,
                             verbose = TRUE) {

  if (!is.data.frame(x)) stop("data doit être un data.frame")
  if (!applyto %in% names(x)) stop(sprintf("Colonne '%s' introuvable", applyto))
  if (!all(by %in% names(x))) {
    missing_by <- paste(setdiff(by, names(x)), collapse = ", ")
    stop(sprintf("Colonne(s) de regroupement introuvable(s): %s", missing_by))
  }
  if (!is.numeric(x[[applyto]])) stop(sprintf("'%s' doit être numérique", applyto))

  method <- match.arg(tolower(method), c("median", "iqr", "mean"))

  fun <- switch(method,
                mean   = function(v) base::mean(v, na.rm = na.rm),
                median = function(v) stats::median(v, na.rm = na.rm),
                iqr    = function(v) stats::IQR(v, na.rm = na.rm))


  out <- stats::aggregate(x[[applyto]], by = x[by], FUN = fun)

  value_name <- paste0(applyto, "_", method)
  names(out) <- c(by, value_name)

  if (verbose) {
    grp_txt <- paste(by, collapse = ", ")
    message(sprintf("summarize_salary: %s par %s sur '%s'", toupper(method), grp_txt, applyto))
  }

  out
}


#' association_table — Table croisée + Cramér's V pour deux catégorielles
#'
#' Construit un tableau de contingence entre deux colonnes catégorielles et
#' calcule la force d'association via Cramér's V (borné dans \code{[0,1]}),
#' ainsi que la statistique du \eqn{\chi^2} et sa p-valeur.
#'
#' @param data data.frame en entrée.
#' @param x Nom de la première colonne catégorielle (character).
#' @param y Nom de la seconde colonne catégorielle (character).
#' @param include_na Inclure les NA comme modalité ? (FALSE par défaut).
#' @param prop Choix des proportions à retourner : \code{"none"}, \code{"row"},
#'   \code{"col"} ou \code{"total"}.
#' @param simulate_p Utiliser une p-valeur simulée (Monte-Carlo) si le tableau est
#'   clairsemé (FALSE par défaut).
#' @param B Nombre de répétitions pour la p-valeur simulée (2000 par défaut).
#'
#' @return Une liste avec au minimum : \code{table}, \code{v}, \code{chi2},
#'   \code{df}, \code{p.value}. Si \code{prop != "none"}, un élément \code{prop}
#'   est ajouté (proportions par ligne/colonne/total).
#' @export
association_table <- function(data, x, y,
                              include_na = FALSE,
                              prop = c("none", "row", "col", "total"),
                              simulate_p = FALSE,
                              B = 2000) {

  if (!is.data.frame(data)) stop("data doit être un data.frame")
  if (!all(c(x, y) %in% names(data))) stop("Colonnes manquantes dans data")
  prop <- match.arg(prop)

  a <- data[[x]]
  b <- data[[y]]
  if (!include_na) {
    ok <- !is.na(a) & !is.na(b)
    a <- a[ok]; b <- b[ok]
  }

  tab <- if (include_na) table(a, b, useNA = "ifany") else table(a, b)
  if (nrow(tab) < 2 || ncol(tab) < 2) {
      stop("Cramér's V nécessite au moins un tableau 2x2.")
    }

  n <- sum(tab)
  chi <- suppressWarnings(
    chisq.test(tab, correct = FALSE, simulate.p.value = simulate_p, B = B)
  )

  chi2 <- as.numeric(chi$statistic)
  r <- nrow(tab); c <- ncol(tab)
  v <- sqrt(chi2 / (n * min(r - 1, c - 1)))

  res <- list(
    table   = tab,
    v       = as.numeric(v),
    chi2    = chi2,
    df      = as.integer(chi$parameter),
    p.value = as.numeric(chi$p.value)
  )

  if (prop == "row")   res$prop <- prop.table(tab, 1)
  if (prop == "col")   res$prop <- prop.table(tab, 2)
  if (prop == "total") res$prop <- prop.table(tab)

  return(res)
}