#' Get market data from FMP cloud
#'
#' @description Get market data from FMP cloud API.
#' @param symbol Stock symbol
#' @param multiply Multiply time yo get final frequency
#' @param time Time frequency
#' @param from Start date, character
#' @param to End date, character
#' @param api_key FMP cloud API key
#' @return Data table of OHLCV prices
#' @import httr
#' @import data.table
#' @examples
#' get_market_equities("AAPL", api_key = Sys.getenv("APIKEY"))
#' @export
get_market_equities <- function(symbol, multiply = 1, time = 'hour', from = as.character(Sys.Date() - 7),
                                to = as.character(Sys.Date()), api_key) {

  x <- GET(paste0('https://financialmodelingprep.com/api/v4/historical-price/',
                  symbol, '/', multiply, '/', time, '/', from, '/', to),
           query = list(apikey = api_key))
  if (x$status_code == 404) {
    return(NULL)
  } else if (x$status_code == 200) {
    x <- content(x)
    return(rbindlist(x$results))
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
