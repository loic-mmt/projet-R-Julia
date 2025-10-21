#' Import CSV as data.frame
#' @param path chemin vers le CSV
#' @return data.frame
#' @export
read_raw_csv <- function(file_path) {
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  return(data)
}
