#' Say hello
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
say_hello <- function(name) {
  paste("Hello,", name, "!")
}
