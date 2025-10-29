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
