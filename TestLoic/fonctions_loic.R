deduplicate_rows <- function(data, keys = NULL, keep = c("first", "last")) {
  keep <- match.arg(keep)

  if (!is.data.frame(data)) {
    stop("'data' doit être un data.frame")
  }
  if (is.null(keys)) {
    keys <- names(data)
  } else {
    inconnues <- setdiff(keys, names(data))
    if (length(inconnues) > 0) {
      stop("Clés inconnues: ", paste(inconnues, collapse = ", "))
    }
  }
  from_last <- identical(keep, "last")
  dup <- duplicated(data[keys], fromLast = from_last)

  out <- data[!dup, , drop = FALSE]

  attr(out, "n_removed") <- sum(dup)
  return(out)
}

df <- data.frame(
  id = c(1,1,2,2,3),
  x  = c("a","a","b","b","c"),
  y  = c(10,10,20,20,30)
)
names(df)

# Dédup sur toutes les colonnes
df2 <- deduplicate_rows(df)
attr(df2, "n_removed")  # attendu: 2
nrow(df2)               # attendu: 3

# Dédup sur une clé partielle
df3 <- deduplicate_rows(df, keys = "id", keep = "last")
df3
