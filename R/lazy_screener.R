#' Collects All Necessary Metrics for Mr.Huang's Strategy
#'
#' This function will return a data.frame ready for advance filtering, plotting, or export.
#' @param n Top price-ranking n companies.
#' @keywords lazy_screener
#' @export
#' @examples
#' lazy_screener()

lazy_screener <- function(n = 5) {
  df <- get_tse_top(n = n)
  # single return section
  df$has_ky <- df$name %>%
    as.data.frame() %>%
    apply(MARGIN = 1, FUN = has_ky)
  col_names <- c("listed_days", "mkt_cap", "avg_volume")
  call_functions <- paste0("get_", col_names)
  for (i in 1:length(col_names)) {
    df[, col_names[i]] <- df$ticker %>%
      as.data.frame() %>%
      apply(MARGIN = 1, FUN = get(call_functions[i]))
    sleep_sec <- runif(1, min = 5, max = 10)
    Sys.sleep(sleep_sec)
  }
  
  # multiple returns section
  call_functions <- c("get_52_w_hl", "get_eps", "get_four_year_profit", "get_inv_to_ratio", "get_n_month_sales_yoy", "get_op_income_ratio", "get_three_year_free_cf")
  for (i in 1:length(call_functions)) {
    apply_mat <- df$ticker %>%
      as.data.frame() %>%
      apply(MARGIN = 1, FUN = get(call_functions[i])) %>%
      t()
    df <- cbind(df, apply_mat)
    sleep_sec <- runif(1, min = 5, max = 10)
    Sys.sleep(sleep_sec)
  }
  names(df) <- c("ticker", "name", "price", "has_ky", "listed_days", "mkt_cap", "avg_volume", "low_52_week", "high_52_week", "eps_1", "eps_2", "eps_3", "eps_4", "eps_5", "eps_6", "eps_7", "eps_8", "profit_1", "profit_2", "profit_3", "profit_4", "inv_to_1", "inv_to_2", "inv_to_3", "inv_to_4", "inv_to_5", "inv_to_6", "inv_to_7", "inv_to_8", "sales_yoy_1", "sales_yoy_2", "sales_yoy_3", "op_income_1", "op_income_2", "op_income_3", "op_income_4", "op_income_5", "op_income_6", "op_income_7", "op_income_8", "free_cf_1", "free_cf_2", "free_cf_3")
  return(df)
}