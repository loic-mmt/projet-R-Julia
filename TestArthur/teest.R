#' validate_schema global
#'
#' @param x Un dataset
#' @export
validate_schema <- function(x) UseMethod("validate_schema")

#' validate_schema pour les salary_tbl
#'
#' @param dataframe Un salary_tbl
#' @export
validate_schema.salary_tbl <- function(dataframe) {
  
}