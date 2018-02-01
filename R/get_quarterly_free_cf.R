#' Get Quarterly Free Cash Flow from Statementdog
#'
#' This function will return quarterly free cash flow of a certain stock.
#' @param x Stock ticker.
#' @param recent_n Recent n quarters, default 16.
#' @keywords get_quarterly_free_cf
#' @export
#' @examples
#' get_quarterly_free_cf(2330)
#' get_quarterly_free_cf(3008)

get_quarterly_free_cf <- function(x, recent_n = 16) {
  this_yr <- Sys.Date() %>%
    format('%Y') %>%
    as.integer()
  four_yr_ago <- this_yr - 5
  statement_dog_url <- sprintf("https://statementdog.com/api/v1/fundamentals/%s/%i/1/%i/4/cf?queried_by_user=true", x, four_yr_ago, this_yr)
  statement_data <- fromJSON(statement_dog_url)
  free_cf <- statement_data$`81`$data[,2] %>%
    as.numeric()
  period_len <- length(free_cf)
  if (recent_n > 16) {
      return(sprintf("季數不能超過%s"), period_len)
  } else {
      return(free_cf[(period_len - recent_n + 1):period_len])
  }
}