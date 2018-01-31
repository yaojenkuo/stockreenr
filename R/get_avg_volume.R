#' Get Average Trading Volume from TSE.
#'
#' This function will return the average trading volume of a certain stock for last month.
#' @param x Stock ticker.
#' @keywords get_avg_volume
#' @export
#' @examples
#' get_avg_volume(2330)
#' get_avg_volume(3008)

get_avg_volume <- function(x) {
  start_of_prev_month <- floor_date(Sys.Date() - months(1), "month") %>%
    gsub(pattern = "-", ., replacement = "")
  csv_url <- sprintf("http://www.twse.com.tw/exchangeReport/STOCK_DAY?response=csv&date=%s&stockNo=%s", start_of_prev_month, x)
  df <- read.csv(csv_url, header = TRUE, skip = 1, fileEncoding = 'big5')
  volume <- df[, 2] %>%
    gsub(pattern = ",", ., replacement = "") %>%
    as.numeric() %>%
    mean(na.rm = TRUE) %>%
    `/` (1000)
  return(volume)
}