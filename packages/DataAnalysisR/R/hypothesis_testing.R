#'variance_homogeneity_test - levene/bartlett
#' @param data dataset
#' @param y colonne à appliquer.
#' @param group nom de colonne à grouper pour appliquer.
#' @param method type de test
#' @param na.rm TRUE, enlève les valeurs manquantes
#'
#' @return Le résultat par méthode groupé.
#' @export
variance_homogeneity_test <- function(data,
                                      y,
                                      group,
                                      method = c("levene", "bartlett"),
                                      na.rm = TRUE) {
  method <- match.arg(method)
  if (!is.data.frame(data)) stop("'data' doit être un data.frame")
  if (!y %in% names(data)) stop(sprintf("Colonne '%s' introuvable", y))
  if (!group %in% names(data)) stop(sprintf("Colonne '%s' introuvable", group))

  d <- data[, c(y, group)]
  if (na.rm) d <- d[stats::complete.cases(d), , drop = FALSE]
  x <- d[[y]]
  g <- as.factor(d[[group]])

  if (method == "bartlett") {
    return(stats::bartlett.test(x ~ g))
  } else {

    med <- tapply(x, g, stats::median)
    z <- abs(x - med[g])

    return(stats::anova(stats::lm(z ~ g)))
  }
}