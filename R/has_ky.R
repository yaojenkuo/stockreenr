#' Determines If a Character Ends with -KY
#'
#' This function will return a logical value indicating if a character ends with "-KY".
#' @param x Company name.
#' @keywords has_ky
#' @export
#' @examples
#' has_ky("矽力-KY")
#' has_ky("大立光")

has_ky <- function(x) {
  ans <- grepl(pattern = "-KY$", x)
  return(ans)
}