#' Impute missing values for numeric and categorical columns
#'
#' Impute `NA` values column-wise with simple, dependency-free strategies.
#' Numeric: "median", "mean" ou "constant".
#' Catégorielles (character/factor): "mode", "constant" ou "new_level" (étiquette par défaut: "Missing").
#' Logiques: imputées à la valeur majoritaire.
#'
#' @param data data.frame d'entrée.
#' @param cols (optionnel) noms de colonnes à imputer. Par défaut: toutes.
#' @param exclude (optionnel) colonnes à exclure après `cols`.
#' @param num_method "median", "mean" ou "constant". Défaut: "median".
#' @param cat_method "mode", "constant" ou "new_level". Défaut: "mode".
#' @param num_constant valeur numérique utilisée si `num_method="constant"` (ou si toute la colonne est NA). Défaut: 0.
#' @param cat_constant valeur utilisée si `cat_method` est "constant" ou "new_level" (ou si toute la colonne est NA). Défaut: "Missing".
#' @param verbose afficher un résumé par colonne. Défaut: TRUE.
#' @return Le data.frame avec les NA imputés.
#' @export
impute_missing <- function(data,
                           cols = NULL,
                           exclude = NULL,
                           num_method = c("median", "mean", "constant"),
                           cat_method = c("mode", "constant", "new_level"),
                           num_constant = 0,
                           cat_constant = "Missing",
                           verbose = TRUE) {
  if (!is.data.frame(data)) stop("data doit être un data.frame")

  num_method <- match.arg(num_method)
  cat_method <- match.arg(cat_method)

  # mode (sur character/factor), égalité -> ordre alphabétique
  mode_char <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return(NA_character_)
    tb <- sort(table(x), decreasing = TRUE)
    max_count <- tb[1]
    candidates <- names(tb)[tb == max_count]
    sort(candidates)[1]
  }

  cols_all <- names(data)
  cols_use <- if (is.null(cols)) cols_all else intersect(cols, cols_all)
  if (!is.null(exclude)) cols_use <- setdiff(cols_use, exclude)
  if (!length(cols_use)) return(data)

  for (nm in cols_use) {
    x <- data[[nm]]
    n_na <- sum(is.na(x))
    if (!n_na) {
      if (verbose) message(sprintf("impute_missing: '%s' -> 0 NA", nm))
      next
    }

    # Numeric
    if (is.numeric(x)) {
      repl <- switch(
        num_method,
        median   = if (sum(!is.na(x)) > 0) stats::median(x, na.rm = TRUE)  else num_constant,
        mean     = if (sum(!is.na(x)) > 0) base::mean(x, na.rm = TRUE)   else num_constant,
        constant = num_constant
      )
      x[is.na(x)] <- repl
      data[[nm]] <- x
      if (verbose) message(sprintf("impute_missing: '%s' (numeric) -> %d NA imputés (%s%s%g)",
                                   nm, n_na, num_method,
                                   if (num_method == "constant") "=" else ": ", repl))
      next
    }

    # Logical -> majorité
    if (is.logical(x)) {
      if (sum(!is.na(x)) == 0) {
        repl_log <- FALSE
      } else {
        count_true  <- sum(x, na.rm = TRUE)
        count_false <- sum(!x, na.rm = TRUE)
        repl_log <- count_true >= count_false
      }
      x[is.na(x)] <- repl_log
      data[[nm]] <- x
      if (verbose) message(sprintf("impute_missing: '%s' (logical) -> %d NA imputés (%s)", nm, n_na, repl_log))
      next
    }

    # Character / Factor -> catégorielle
    is_fac <- is.factor(x)
    lev <- if (is_fac) levels(x) else NULL
    x_chr <- as.character(x)

    repl_chr <- switch(
      cat_method,
      mode = {
        m <- mode_char(x_chr)
        if (is.na(m)) cat_constant else m
      },
      constant  = cat_constant,
      new_level = cat_constant
    )

    x_chr[is.na(x_chr)] <- repl_chr

    if (is_fac) {
      new_levels <- lev
      if (!(repl_chr %in% new_levels)) new_levels <- c(new_levels, repl_chr)
      data[[nm]] <- factor(x_chr, levels = new_levels)
    } else {
      data[[nm]] <- x_chr
    }

    if (verbose) message(sprintf("impute_missing: '%s' (categorical) -> %d NA imputés (%s%s%s)",
                                 nm, n_na, cat_method,
                                 if (cat_method == "mode") ": " else "=",
                                 repl_chr))
  }

  data
}