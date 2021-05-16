#' Get market crypto data from FMP cloud
#'
#' @description Get market crypto data from FMP cloud API.
#' @param symbol Crypto symbol
#' @param multiply Multiply time yo get final frequency
#' @param time Time frequency
#' @param from Start date, character
#' @param to End date, character
#' @param api_key FMP cloud API key
#' @param limit = Response limit, default is 500. (Maximum 1000)
#' @return Data table of OHLCV prices
#' @import httr
#' @import data.table
#' @examples
#' get_market_crypto(symbol = 'btcusd', multiply = 1, time = 'h', from = as.character(Sys.Date() - 7),
#'                   to = as.character(Sys.Date()), api_key = Sys.getenv("APIKEY"), limit = 100)
#' @export
get_market_crypto <- function(symbol, multiply = 1, time = 'hour', from = as.character(Sys.Date() - 7),
                              to = as.character(Sys.Date()), api_key, limit) {

  options(scipen = 100)
  from <- as.numeric(as.POSIXct(from)) * 1000
  to <- as.numeric(as.POSIXct(to)) * 1000
  x <- GET(paste0('https://financialmodelingprep.com/api/v4/historical-price-crypto/',
                  symbol, '/', multiply, time, '/', from, '/', to),
           query = list(apikey = api_key, limit = limit))
  if (x$status_code == 404) {
    return(NULL)
  } else if (x$status_code == 200) {
    x <- content(x)
    x <- rbindlist(x)
    x$ot <- as.POSIXct(x$ot / 1000, origin = "1970-01-01", tz = 'America/New_York')
    x$ct <- as.POSIXct(x$ct / 1000 + 1, origin = "1970-01-01", tz = 'America/New_York')
    return(x)
  } else {
    x <- RETRY("GET",
               paste0('https://financialmodelingprep.com/api/v4/historical-price/',
                      symbol, '/', multiply, '/', time, '/', from, '/', to),
               query = list(apikey = api_key),
               times = 5)
    if (x$status_code == 200) {
      x <- content(x)
      return(rbindlist(x$results))
    } else {
      stop('Error in reposne. Status not 200 and not 404')
    }
  }
}
