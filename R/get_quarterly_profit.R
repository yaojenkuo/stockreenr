#' Get Quarterly Profit After Tax from Statementdog.
#'
#' This function will return 16 quarters of profit after tax in thousands NTD of a certain stock.
#' @param x Stock ticker.
#' @param recent_n Recent n quarters, default 16.
#' @keywords get_quarterly_profit
#' @export
#' @examples
#' get_quarterly_profit(2330)
#' get_quarterly_profit(3008)

get_quarterly_profit <- function(x, recent_n = 16) {
  this_yr <- Sys.Date() %>%
    format('%Y') %>%
    as.integer()
  four_yr_ago <- this_yr - 5
  statement_dog_url <- sprintf("https://statementdog.com/api/v1/fundamentals/%s/%i/1/%i/4/cf?queried_by_user=true", x, four_yr_ago, this_yr)
  statement_data <- fromJSON(statement_dog_url)
  profit_after_tax <- statement_data$`64`$data[,2] %>%
    as.numeric()
  period_len <- length(profit_after_tax)
  if (recent_n > vec_len) {
    return(sprintf("季數不能超過%s"), period_len)
  }
  return(profit_after_tax[(period_len - recent_n + 1):period_len])
}