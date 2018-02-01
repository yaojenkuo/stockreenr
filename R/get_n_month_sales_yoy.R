#' Get Monthly Sales YOY from Yahoo! Stock.
#'
#' This function will return n months of sales yoy in thousands NTD of a certain stock.
#' @param x Stock ticker.
#' @param recent_n Recent n months, default 3.
#' @keywords get_n_month_sales_yoy
#' @export
#' @examples
#' get_n_month_sales_yoy(2330)
#' get_n_month_sales_yoy(3008)

get_n_month_sales_yoy <- function(x, recent_n = 3) {
  company_url <- sprintf("https://tw.stock.yahoo.com/d/s/earning_%s.html", x)
  html_doc <- company_url %>%
    read_html()
  current_yr <- html_doc %>%
    html_nodes(css = "td td tr~ tr+ tr .ttt:nth-child(6)") %>%
    html_text() %>%
    gsub(pattern = "%", ., replacement = "") %>%
    as.numeric()
  last_yr <- html_doc %>%
    html_nodes(css = "td td tr~ tr+ tr .ttt:nth-child(3)") %>%
    html_text() %>%
    gsub(pattern = "%", ., replacement = "") %>%
    as.numeric()
  sales_yoy <- c(last_yr, current_yr)
  period_len <- length(sales_yoy)
  if (recent_n > sales_yoy_len) {
    return(sprintf("月份數不能超過%s"), period_len)
  } else {
    return(sales_yoy[(period_len - recent_n + 1):period_len])
  }
}