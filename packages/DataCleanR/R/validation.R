#' 
#'
#' A simple example function that greets a person.
#'
#' @param name A character string with the person's name.
#'
#' @return A character string saying hello.
#' @export
#'
#' @examples
#' say_hello("Alice")
read_raw_csv <- function(file_path) {
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  return(data)
}
