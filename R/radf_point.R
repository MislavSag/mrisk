#' Point radf
#'
#' @description Calcualte radf values for point in time.
#' @param symbols Stock symbols
#' @param end_date End date.
#' @param window Window size
#' @param price_lag Number of price lags to use
#' @param use_log Should log of prices be used
#' @param api_key FMP cloud API key
#' @param time Time frequency
#' @return Radf values; output from exuber package
#' @import exuber
#' @import data.table
#' @import httr
#' @importFrom utils tail
#' @examplea
#' radf_point("AAPL", Sys.Date(), 100, 1, TRUE, Sys.getenv("APIKEY"), time = "hour")
#' radf_point("BTCUSD", Sys.Date(), 100, 1, TRUE, Sys.getenv("APIKEY"), time = "hour")
#' radf_point("BTCUSD", Sys.Date(), 100, 1, TRUE, Sys.getenv("APIKEY"), time = "minute")
#' @export
radf_point <- function(symbols, end_date, window, price_lag, use_log, api_key, time = "hour") {

  # solve No visible binding for global variable
  formated <- symbol <- NULL

  # set start and end dates
  # dots <- as.list(match.call()[-1L])
  if (time == 'hour') {
    start_dates <- seq.Date(as.Date(end_date) - (window / 3), as.Date(end_date) - 5, by = 5)
    if (tail(start_dates, 1) != (as.Date(end_date) - 5)) {
      start_dates <- c(start_dates, Sys.Date() - 5)
    }
    end_dates <- start_dates + 5
  } else if (time == "minute") {
    start_dates <- as.Date(end_date) - 5
    end_dates <- end_date
  }

  # get market data
  if (symbols == 'BTCUSD' | symbols == 'btcusd') {
    if (time == "hour") {
      time_crypto = "h"
      from_crypto = as.character(as.Date(end_date) - 20)
      to_crypto = end_date
    } else if (time == "minute") {
      time_crypto = "m"
      from_crypto = as.character(Sys.time() - 10000)
      to_crypto <- as.character(Sys.time())
    }
    ohlcv <- get_market_crypto(symbols,
                               multiply = 1,
                               time = time_crypto,
                               from = from_crypto,
                               to = to_crypto,
                               api_key = api_key,
                               limit = 1000)
    data.table::setnames(ohlcv, 'ct', 'formated')
    ohlcv[, symbol := 'BTCUSD']
    prices <- tail(ohlcv, window)
  } else {
    ohlcv <- lapply(symbols, function(symbol) {
      lapply(seq_along(start_dates),
             function(i) get_market_equities(symbol,
                                             from = start_dates[i],
                                             to = end_dates[i],
                                             time = time,
                                             api_key = api_key))
    })
    prices <- lapply(ohlcv, rbindlist)
    names(prices) <- symbols
    prices <- rbindlist(prices, idcol = TRUE)
    setnames(prices, ".id", "symbol")
    prices <- unique(prices)
    prices[, formated := as.POSIXct(formated, tz = "EST")]
    if (time == "hour") {
      prices <- prices[format(formated, "%H:%M:%S") %between% c("10:00:00", "15:00:00")]
    }
    setorderv(prices, c("symbol", "formated"))
  }

  ################# DEOSTN WORK FOR MULTIPLE SYMBOLS ############
  # calculate exuber
  if (use_log) {
    close <- log(prices$c)
  } else {
    close <- prices$c
  }

  # add newest data becuase FP cloud can't reproduce newst data that fast
  nytime <- format(Sys.time(), tz="America/New_York", usetz=TRUE)
  if (time == "hour" && hour(nytime) > hour(max(prices$formated))) {
    last_price <- GET(paste0("https://financialmodelingprep.com/api/v3/quote-short/", symbols, "?apikey=", api_key))
    last_price <- content(last_price)[[1]]$price
    if (use_log) {
      close <- c(close, log(last_price))
    } else {
      close <- c(close, last_price)
    }
    max_datetime <- round.POSIXt(nytime, units = "hours")
  } else if (time == "minute" && minute(nytime) > minute(max(prices$formated))) {
    last_price <- GET(paste0("https://financialmodelingprep.com/api/v3/quote-short/", symbols, "?apikey=", api_key))
    last_price <- content(last_price)[[1]]$price
    if (use_log) {
      close <- c(close, log(last_price))
    } else {
      close <- c(close, last_price)
    }
    max_datetime <- round.POSIXt(nytime, units = "mins")
  } else {
    max_datetime <- max(prices$formated)
  }
  close <- close[(length(close)-window+1):length(close)]
  y <- exuber::radf(close, lag = price_lag)
  stats <- exuber::tidy(y)
  bsadf <- data.table::last(exuber::augment(y))[, 4:5]
  max_datetime <- as.POSIXct(as.character(max_datetime), tz = 'EST')
  attributes(max_datetime)$tzone <- 'UCT'
  result <- cbind(datetime = max_datetime, stats, bsadf)
  result$id <- NULL
  return(result)
}
# symbols = "SPY"
# end_date = as.character(Sys.Date())
# window = 100
# price_lag = 1
# use_log = 1
# api_key = Sys.getenv("APIKEY")
# time = "hour"
# radf_point("AAPL", Sys.Date(), 100, 1, TRUE, Sys.getenv("APIKEY"), time = "hour")
# radf_point("AAPL", Sys.Date(), 100, 1, TRUE, Sys.getenv("APIKEY"), time = "minute")
# radf_point("BTCUSD", Sys.Date(), 100, 1, TRUE, Sys.getenv("APIKEY"), time = "hour")
# radf_point("BTCUSD", Sys.Date(), 100, 1, TRUE, Sys.getenv("APIKEY"), time = "minute")
