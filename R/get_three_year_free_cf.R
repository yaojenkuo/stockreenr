#' Get Three Years of Free Cash Flow
#'
#' This function will return 3 years of free cash flow of a certain stock.
#' @param x Stock ticker.
#' @keywords get_three_year_free_cf
#' @export
#' @examples
#' get_three_year_free_cf(2330)
#' get_three_year_free_cf(3008)

get_three_year_free_cf <- function(x) {
  this_yr <- Sys.Date() %>%
    format('%Y') %>%
    as.integer()
  three_yr_ago <- this_yr - 4
  statement_dog_url <- sprintf("https://statementdog.com/api/v1/fundamentals/%s/%i/1/%i/4/cf?queried_by_user=true", x, three_yr_ago, this_yr)
  statement_data <- fromJSON(statement_dog_url)
  free_cf <- statement_data$`199`$data[,2] %>%
    as.numeric()
  years <- three_yr_ago:(three_yr_ago + 2)
  names(free_cf) <- years
  return(free_cf)
}