#' Get Inventory Turnover Ratio from Statementdog.
#'
#' This function will return the inventory turnover ratio of a certain stock.
#' @param x Stock ticker.
#' @keywords get_inv_to_ratio
#' @export
#' @examples
#' get_inv_to_ratio(2330)
#' get_inv_to_ratio(3008)

get_inv_to_ratio <- function(x, recent_n = 8) {
  this_yr <- Sys.Date() %>%
    format('%Y') %>%
    as.integer()
  three_yr_ago <- this_yr - 4
  statement_dog_url <- sprintf("https://statementdog.com/api/v1/fundamentals/%s/%i/1/%i/4/cf?queried_by_user=true", x, three_yr_ago, this_yr)
  statement_data <- fromJSON(statement_dog_url)
  inv_to_ratio <- statement_data$`92`$data[, 2] %>%
    as.numeric()
  period_len <- length(inv_to_ratio)
  if (recent_n > period_len) {
    return(sprintf("季數不能超過%s"), period_len)
  } else {
    return(inv_to_ratio[(period_len - recent_n + 1):period_len])
  }
}