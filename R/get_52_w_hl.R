#' Get 52 Week Price High and Low from Google Finance.
#'
#' This function will return the 52-week price high and low of a certain stock.
#' @param x Stock ticker.
#' @keywords get_52_w_hl
#' @export
#' @examples
#' get_52_w_hl(2330)
#' get_52_w_hl(3008)

get_52_w_hl <- function(x) {
  company_url <- sprintf("https://finance.google.com/finance?q=TPE%%3A%s", x)
  stock_values <- company_url %>%
    read_html() %>%
    html_nodes(css = ".val") %>%
    html_text() %>%
    gsub(pattern = "\n", ., replacement = "")
  high_low <- stock_values[2] %>%
    strsplit(split = " - ") %>%
    gsub(pattern = ",", ., replacement = "") %>%
    unlist() %>%
    as.numeric()
  return(high_low)
}