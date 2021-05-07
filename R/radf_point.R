#' Point radf
#'
#' @description Calcualte radf values for point in time.
#' @param symbols Stock symbols
#' @param end_date End date.
#' @param window Window size
#' @param price_lag Number of price lags to use
#' @param use_log Should log of prices be used
#' @param api_key FMP cloud API key
#' @param ... arguments for get_market_equities function
#' @return Radf values; output from exuber package
#' @import exuber
#' @import data.table
#' @import leanr
#' @importFrom utils tail
#' @examplea
#' radf_point("AAPL", Sys.Date(), 100, 1, TRUE, Sys.getenv("APIKEY"), time = "hour")
#' @export
radf_point <- function(symbols, end_date, window, price_lag, use_log, api_key, ...) {

  # solve No visible binding for global variable
  formated <- NULL

  # set start and end dates
  dots <- as.list(match.call()[-1L])
  if (!("time" %in% names(dots)) || dots$time == 'hour') {
    start_dates <- seq.Date(as.Date(end_date) - (window / 4), as.Date(end_date) - 5, by = 5)
    if (tail(start_dates, 1) != (as.Date(end_date) - 5)) {
      start_dates <- c(start_dates, Sys.Date() - 5)
    }
    end_dates <- start_dates + 5
  }

  # get market data
  ohlcv <- lapply(symbols, function(symbol) {
    lapply(seq_along(start_dates),
           function(i) get_market_equities(symbol,
                                           from = start_dates[i],
                                           to = end_dates[i],
                                           api_key = api_key))
  })
  prices <- lapply(ohlcv, rbindlist)
  names(prices) <- symbols
  prices <- rbindlist(prices, idcol = TRUE)
  setnames(prices, ".id", "symbol")
  prices <- unique(prices)
  prices[, formated := as.POSIXct(formated)]
  prices <- prices[format(formated, "%H:%M:%S") %between% c("10:00:00", "15:00:00")]
  setorderv(prices, c("symbol", "formated"))

  ################# DEOSTN WORK FOR MULTIPLE SYMBOLS ############
  # calculate exuber
  if (use_log) {
    close <- log(prices$c)
  } else {
    close <- prices$c
  }
  y <- radf(close, lag = price_lag)
  stats <- exuber::tidy(y)
  bsadf <- data.table::last(exuber::augment(y))[, 4:5]
  result <- cbind(datetime = max(prices$formated), stats, bsadf)
  result$id <- NULL
  return(result)
}


