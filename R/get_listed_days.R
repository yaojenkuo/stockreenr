#' Get Listed Days from Yahoo! Stock.
#'
#' This function will return listed days of a certain stock.
#' @param x Stock ticker.
#' @keywords get_listed_days
#' @export
#' @examples
#' get_listed_days(2330)
#' get_listed_days(3008)

get_listed_days <- function(x) {
  company_url <- sprintf("https://tw.stock.yahoo.com/d/s/company_%s.html", x)
  listed_css <- "table:nth-child(1) tr:nth-child(4) td:nth-child(2)"
  # get html document
  html_doc <- company_url %>%
    read_html()
  # get listed date
  listed_date <- html_doc %>%
    html_node(css = listed_css) %>%
    html_text() %>%
    strsplit(split = "/")
  # format listed date
  yr_part <- as.numeric(listed_date[[1]][1]) + 1911
  listed_date <- sprintf("%s-%s-%s", yr_part, listed_date[[1]][2], listed_date[[1]][3])
  listed_date <- as.Date(listed_date)
  listed_days <- as.numeric(Sys.Date()) - as.numeric(listed_date)
  return(listed_days)
}