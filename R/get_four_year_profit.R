#' Get Profit After Tax from Statementdog.
#'
#' This function will return 4 years of profit after tax in thousands NTD of a certain stock.
#' @param x Stock ticker.
#' @keywords get_four_year_profit
#' @export
#' @examples
#' get_four_year_profit(2330)
#' get_four_year_profit(3008)

get_four_year_profit <- function(x) {
  this_yr <- Sys.Date() %>%
    format('%Y') %>%
    as.integer()
  four_yr_ago <- this_yr - 5
  statement_dog_url <- sprintf("https://statementdog.com/api/v1/fundamentals/%s/%i/1/%i/4/cf?queried_by_user=true", x, four_yr_ago, this_yr)
  statement_data <- fromJSON(statement_dog_url)
  profit_after_tax <- statement_data$`184`$data[,2] %>%
    as.numeric()
  years <- four_yr_ago:(four_yr_ago + 3)
  names(profit_after_tax) <- years
  return(profit_after_tax)
}