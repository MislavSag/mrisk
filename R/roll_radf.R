#' Exuber on rolling window
#'
#' @description Calculate radf values on rolling window using exuber apckage
#' @param price Stock price
#' @param use_log Should log of prices be used
#' @param window Window size
#' @param price_lags Number of price lags to use
#' @param no_cores Number of cores to use
#' @return Radf values; output from exuber package
#' @import exuber
#' @import data.table
#' @import runner
#' @import parallel
#' @export
roll_radf <- function(price, use_log, window, price_lags, no_cores = 4L) {

  # convert prices to log prices
  if (use_log) {
    price <- log(price)
  }

  # calculate radf values on log window
  cl <- makeCluster(no_cores)
  clusterExport(cl, c("price", "use_log", "window", "price_lags"), envir = environment())
  roll_radf <- runner(
    x = as.data.frame(price),
    f = function(x) {
      y <- exuber::radf(x, lag = price_lags)
      stats <- exuber::tidy(y)
      bsadf <- data.table::last(exuber::augment(y))[, 4:5]
      y <- cbind(stats, bsadf)
      return(y)
    },
    k = window,
    na_pad = TRUE,
    cl = cl
  )
  stopCluster(cl)

  # merge all radf values
  roll_radf <- lapply(roll_radf, data.table::as.data.table)
  roll_radf <- data.table::rbindlist(roll_radf, fill = TRUE)[, `:=`(V1 = NULL, id = NULL)]
  return(roll_radf)
}
