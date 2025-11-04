#' Describe overview global
#'
#' @param x Un objet
#' @export
describe_overview <- function(x) UseMethod("describe_overview")

#' Describe overview pour des vecteur d'integer
#'
#' @param vect un vecteur d'integer
#' @export
describe_overview.integer <- function(vect) {
  list(
    taille = length(vect),
    moyenne = mean(vect),
    mediane = median(vect),
    nb_groupes = length(unique(vect)),
    minimum = min(vect),
    maximum = max(vect),
    quantiles = quantile(vect)
  )
}

#' Describe overview pour des vecteur de numeric/double
#'
#' @param vect un vecteur de numeric/double
#' @export
describe_overview.numeric <- function(vect) {
  list(
    taille = length(vect),
    moyenne = mean(vect),
    mediane = median(vect),
    nb_groupes = length(unique(vect)),
    minimum = min(vect),
    maximum = max(vect),
    quantiles = quantile(vect)
  )
}
#' Describe overview pour des facteurs
#'
#' @param fact un facteur
#' @export
describe_overview.factor <- function(fact) {
  frequences <- table(fact)
  list(
    taille = length(fact),
    nb_groupes = nlevels(fact),
    levels = levels(fact),
    frequences = frequences,
    plus_frequent = names(which.max(frequences)),
    moins_frequent = names(which.min(frequences))
  )
}

#' Describe overview pour des character
#'
#' @param char un vecteur character
#' @export
describe_overview.character <- function(char) {
  frequences <- table(char)
  list(
    taille = length(char),
    nb_groupes = length(unique(char)),
    frequences = frequences,
    plus_frequent = names(which.max(frequences)),
    moins_frequent = names(which.min(frequences))
  )
}

#' Méthode par défaut
#'
#' @param x N'importe quel objet ne rentrant pas dans les classes implémentées
#' @export
describe_overview.default <- function(x) {
  stop("describe_overview n'est pas implémenté pour: ", class(x))
}