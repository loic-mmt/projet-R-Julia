devtools::load_all("/Users/loic/Documents/UGA/LogSpe/Projet_R/projet-R-Julia/packages/DataCleanR")
devtools::load_all("/Users/loic/Documents/UGA/LogSpe/Projet_R/projet-R-Julia/packages/DataAnalysisR")
df <- read_raw_csv("ds_salaries.csv")
df <- data.frame(df)


#' Tailles d'effet multi-groupes (η², ω², ε² de Kruskal)
#'
#' @description
#' Calcule des tailles d'effet pour une variable salaire :
#' - "eta2"  (η², ANOVA à 1 facteur), pour données “pas trop tordues”, hypothèses ANOVA à peu près tenues.
#' - "omega2" (ω², version moins biaisée), pour une mesure moins biaisée que η².
#' - "epsilon2" (ε² de Kruskal, non paramétrique), pour distributions asymétriques / outliers / hétéroscédasticité marquée.
#'
#' @param data data.frame avec au moins `y` et `group`.
#' @param y    Nom de la colonne numérique (salaire).
#' @param group Nom de la colonne de groupes (facteur/chaine).
#' @param method c("eta2","omega2","epsilon2").
#' @param na.rm TRUE pour retirer les NA avant calcul.
#'
#' @return objet "effect_size" (list): method, estimate, ci_low, ci_high,
#'         n_per_group, assumptions, note, data.name
#' @export
 effect_size_salary <- function(data,
                                y,
                                group,
                                method = c("eta2", "omega2", "epsilon2"),
                                na.rm = TRUE) {
  method <- match.arg(method)

  if (!is.data.frame(data)) stop("'data' doit être un data.frame")
  if (!y %in% names(data)) stop(sprintf("Colonne '%s' introuvable", y))
  if (!group %in% names(data)) stop(sprintf("Colonne '%s' introuvable", group))

  df <- data[, c(y, group)]
  names(df) <- c("y", "g")
  if (na.rm) df <- df[stats::complete.cases(df), , drop = FALSE]
  if (!nrow(df)) stop("Aucune observation après suppression des NA")
  if (!is.numeric(df$y)) stop("'y' doit être numérique")
  df$g <- as.factor(df$g)
  if (nlevels(df$g) < 2) stop("'group' doit contenir au moins 2 niveaux")

  n_per_group <- as.integer(table(df$g))

  res <- switch(
    method,
    eta2     = .compute_eta2_(df$y, df$g),
    omega2   = .compute_omega2_(df$y, df$g),
    epsilon2 = .compute_epsilon2_(df$y, df$g)
  )

  out <- list(
    method = method,
    estimate = res$estimate,
    ci_low = res$ci[1],
    ci_high = res$ci[2],
    n_per_group = n_per_group,
    assumptions = res$assumptions,
    note = res$note,
    data.name = sprintf("%s ~ %s", y, group)
  )
  class(out) <- c("effect_size", "list")
  out
}

.compute_eta2_ <- function(y, g) {
  fit <- stats::lm(y ~ g)
  a <- stats::anova(fit)
  ss_between <- as.numeric(a[rownames(a) == "g", "Sum Sq"])
  ss_within  <- as.numeric(a[rownames(a) == "Residuals", "Sum Sq"])
  if (!is.finite(ss_between) || !is.finite(ss_within)) {
    eta2 <- NA_real_
  } else {
    ss_total <- ss_between + ss_within
    eta2 <- if (ss_total > 0) ss_between / ss_total else NA_real_
  }
  list(
    estimate = eta2,
    ci = c(NA_real_, NA_real_),
    assumptions = "ANOVA: indépendance, normalité ~résiduelle, homogénéité des variances.",
    note = "η² (part de variance expliquée)."
  )
}

.compute_omega2_ <- function(y, g) {
  fit <- stats::lm(y ~ g)
  a <- stats::anova(fit)
  ss_between <- as.numeric(a[rownames(a) == "g", "Sum Sq"]) 
  df_between <- as.numeric(a[rownames(a) == "g", "Df"]) 
  ss_within  <- as.numeric(a[rownames(a) == "Residuals", "Sum Sq"]) 
  df_within  <- as.numeric(a[rownames(a) == "Residuals", "Df"]) 
  ms_within  <- ss_within / df_within
  ss_total   <- ss_between + ss_within
  omega2 <- (ss_between - df_between * ms_within) / (ss_total + ms_within)

  if (is.finite(omega2)) omega2 <- max(0, min(1, omega2)) else omega2 <- NA_real_
  list(
    estimate = omega2,
    ci = c(NA_real_, NA_real_),
    assumptions = "ANOVA: indépendance, normalité ~résiduelle, homogénéité des variances.",
    note = "ω² (version moins biaisée que η²)."
  )
}

.compute_epsilon2_ <- function(y, g) {
  kt <- stats::kruskal.test(y ~ g)
  H  <- as.numeric(kt$statistic)
  N  <- length(y)
  k  <- nlevels(g)
  eps2 <- (H - (k - 1)) / (N - 1)
  if (is.finite(eps2)) eps2 <- max(0, min(1, eps2)) else eps2 <- NA_real_
  list(
    estimate = eps2,
    ci = c(NA_real_, NA_real_),
    assumptions = "Non paramétrique: indépendance; robuste aux outliers/asymétries.",
    note = "ε² de Kruskal (alternative robuste quand ANOVA n'est pas adaptée)."
  )
}

#' @export
 print.effect_size <- function(x, ...) {
  fmt <- function(v) ifelse(is.na(v), "NA", sprintf("%.3f", v))
  cat(sprintf("Effect size [%s] = %s  (CI %s; %s)\n",
              x$method, fmt(x$estimate), fmt(x$ci_low), fmt(x$ci_high)))
  cat(sprintf("n par groupe: %s\n", paste(x$n_per_group, collapse = ", ")))
  if (!is.null(x$assumptions)) cat(sprintf("Hypothèses: %s\n", x$assumptions))
  if (!is.null(x$note))        cat(sprintf("Note: %s\n", x$note))
  invisible(x)
 }

# Exemples d'utilisation (laisser en commentaire dans une librairie)
res <- effect_size_salary(df, y = "salary_in_usd", group = "company_location", method = "eta2")
# res <- effect_size_salary(df, y = "salary_in_usd", group = "company_location", method = "omega2")
# res <- effect_size_salary(df, y = "salary_in_usd", group = "company_location", method = "epsilon2")


#' Ajuster une régression quantile pour le salaire (robuste)
#'
#' @param data data.frame
#' @param y    nom de la colonne salaire (numérique)
#' @param predictors vecteur de colonnes prédictrices (si NULL: toutes sauf y)
#' @param tau  quantile à modéliser (0<tau<1), ex: 0.5 = médiane
#' @param na.rm supprimer les lignes incomplètes
#' @return un objet `rq` (package quantreg)
#' @details Requiert le package **quantreg**. Installez-le si besoin : install.packages("quantreg").
#' @export
fit_salary_quantile <- function(data, y, predictors = NULL, tau = 0.5, na.rm = TRUE,
                                method = c("br", "fn"), auto_clean = TRUE) {
  method <- match.arg(method)

  if (!is.data.frame(data)) stop("'data' doit être un data.frame")
  if (!y %in% names(data)) stop(sprintf("Colonne '%s' introuvable", y))
  if (!is.numeric(data[[y]])) stop("'y' doit être numérique")
  if (!is.numeric(tau) || length(tau) != 1L || tau <= 0 || tau >= 1) stop("'tau' doit être un réel dans (0,1)")
  if (!requireNamespace("quantreg", quietly = TRUE)) {
    stop("Cette fonction nécessite le package 'quantreg'. Faites install.packages('quantreg').")
  }

  df <- data
  if (na.rm) {
    cols_needed <- unique(c(y, predictors))
    if (is.null(predictors)) cols_needed <- names(df)
    df <- df[stats::complete.cases(df[, cols_needed, drop = FALSE]), , drop = FALSE]
  }

  # Nettoyage simple des prédicteurs pour éviter les matrices singulières
  preds <- if (auto_clean) .sanitize_predictors_(df, y, predictors) else {
    if (is.null(predictors)) setdiff(names(df), y) else predictors
  }
  if (!length(preds)) stop("Aucun prédicteur utilisable : vérifiez 'predictors' ou désactivez auto_clean.")

  fml <- .build_formula_(y = y, predictors = preds, data = df)

  # Essai de fit : méthode choisie, puis fallback si 'Singular design matrix'
  fit <- tryCatch(
    quantreg::rq(fml, data = df, tau = tau, method = method),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("Singular design matrix", msg, ignore.case = TRUE)) {
        alt <- if (method == "br") "fn" else "br"
        quantreg::rq(fml, data = df, tau = tau, method = alt)
      } else {
        stop(e)
      }
    }
  )

  attr(fit, "data.name") <- sprintf("%s ~ %s (tau=%.2f)", y, paste(all.vars(fml)[-1], collapse = "+"), tau)
  attr(fit, "predictors") <- preds
  attr(fit, "auto_clean") <- auto_clean
  fit
}

# Helper: retire colonnes problématiques (constantes, facteurs quasi-identité, etc.)
.sanitize_predictors_ <- function(df, y, predictors = NULL) {
  preds <- if (is.null(predictors)) setdiff(names(df), y) else predictors
  preds <- setdiff(preds, y)  # sécurité si y a été passé par erreur

  # 1) Retirer colonnes constantes / à un seul niveau
  keep <- vapply(preds, function(p) {
    v <- df[[p]]
    if (is.numeric(v)) {
      return(is.finite(stats::var(v, na.rm = TRUE)) && stats::var(v, na.rm = TRUE) > 0)
    }
    if (is.factor(v) || is.character(v)) {
      return(length(unique(v)) > 1)
    }
    TRUE
  }, logical(1))
  preds <- preds[keep]

  # 2) Eviter facteurs à très haute cardinalité (souvent quasi-identifiants)
  keep2 <- vapply(preds, function(p) {
    v <- df[[p]]
    if (is.factor(v) || is.character(v)) {
      lv <- length(unique(v))
      return(lv < max(50, floor(nrow(df) / 2)))
    }
    TRUE
  }, logical(1))
  preds <- preds[keep2]

  if (!length(preds)) stop("Nettoyage trop agressif: aucun prédicteur restant. Spécifiez 'predictors' manuellement.")
  preds
}
# --- Helper utilisé par fit_salary_quantile (Loïc) ---
# Construit la formule y ~ x1 + x2 + ...
# - Si 'predictors' est NULL : on utilise toutes les colonnes de 'data' sauf 'y'.
# - Vérifie que les colonnes existent et lève un message clair sinon.
.build_formula_ <- function(y, predictors = NULL, data) {
  if (!is.character(y) || length(y) != 1L) stop("'y' doit être un nom de colonne (chaîne)")
  if (!y %in% names(data)) stop(sprintf("Colonne '%s' introuvable dans 'data'", y))

  if (is.null(predictors)) {
    preds <- setdiff(names(data), y)
    if (length(preds) == 0L) stop("Aucun prédicteur disponible. Fournissez 'predictors'.")
  } else {
    if (!is.character(predictors)) stop("'predictors' doit être un vecteur de noms de colonnes")
    missing <- setdiff(predictors, names(data))
    if (length(missing)) stop(sprintf("Colonnes manquantes: %s", paste(missing, collapse = ", ")))
    preds <- predictors
  }

  rhs <- paste(preds, collapse = " + ")
  stats::as.formula(sprintf("%s ~ %s", y, rhs))
}