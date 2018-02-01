#' Get Top Ranking TSE Stocks Regarding to Price From Yahoo! Stock.
#'
#' This function will return the top n TSE stocks as a data.frame regarding to stock price.
#' @param n Defaults to 100.
#' @keywords get_tse_top
#' @export
#' @examples
#' get_tse_top()
#' get_tse_top(n = 30)

get_tse_top <- function(n = 100) {
  y_stock_url <- "https://tw.stock.yahoo.com/d/i/rank.php?t=pri&e=tse&n=100"
  name_css <- ".name"
  price_css <- ".name+ td"
  # get html document
  html_doc <- y_stock_url %>%
    read_html()
  # get ticker and name
  ticker_name <- html_doc %>%
    html_nodes(css = name_css) %>%
    html_text() %>%
    strsplit(split = " ")
  ticker <- vector()
  name <- vector()
  for (i in 1:length(ticker_name)) {
    ticker <- c(ticker, ticker_name[[i]][1])
    name <- c(name, ticker_name[[i]][2])
  }
  # get stock_price
  price <- html_doc %>%
    html_nodes(css = price_css) %>%
    html_text() %>%
    as.numeric()
  # get result_df
  result_df <- data.frame(ticker = ticker,
                          name = name,
                          price = price,
                          stringsAsFactors = FALSE)
  return(result_df[1:n, ])
}