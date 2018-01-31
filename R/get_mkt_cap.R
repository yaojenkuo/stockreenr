#' Get Market Capitalization from Google Finance.
#'
#' This function will return the market cap in 100 million NTD of a certain stock.
#' @param x Stock ticker.
#' @keywords get_mkt_cap
#' @export
#' @examples
#' get_mkt_cap(2330)
#' get_mkt_cap(3008)

get_mkt_cap <- function(x) {
  company_url <- sprintf("https://finance.google.com/finance?q=TPE%%3A%s", x)
  stock_values <- company_url %>%
    read_html() %>%
    html_nodes(css = ".val") %>%
    html_text() %>%
    gsub(pattern = "\n", ., replacement = "")
  mkt_cap <- stock_values[5]
  if (grepl(mkt_cap, pattern = "T")) {
    mkt_cap <- mkt_cap %>%
      gsub(pattern = "T", ., replacement = "") %>%
      as.numeric() %>%
      `*` (10000)
    return(mkt_cap)
  } else if (grepl(mkt_cap, pattern = "B")) {
    mkt_cap <- mkt_cap %>%
      gsub(pattern = "B", ., replacement = "") %>%
      as.numeric() %>%
      `*` (10)
    return(mkt_cap)
  } else if (grepl(mkt_cap, pattern = "M")) {
    mkt_cap <- mkt_cap %>%
      gsub(pattern = "M", ., replacement = "") %>%
      as.numeric() %>%
      `/` (100)
    return(mkt_cap)
  } else {
    mkt_cap <- mkt_cap %>%
      as.numeric() %>%
      `/` (100000)
    return(mkt_cap)
  }
}