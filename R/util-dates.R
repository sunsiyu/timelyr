#' Create dates
#'
#' Create dates with different methods
#'
#' @param start start date
#' @param end end date
#' @param by if method is sequence, seq() argument by
#' @param n if method is random, vector of length n dates to create
#' @param method partial matching methods to be used
#' @return a vector of dates
#' @examples
#' d1 <- create_dates("2015-01-01", "2015-04-01", by="1 week")
#' d2 <- create_dates("2015-01-01", "2015-04-01", n = 10, method = "ran")
#' @export
create_dates <- function(start, end, by, n,
                         method = c("sequence", "random")[1], ...)
{
  start <- as.Date(start)
  end <- as.Date(end)
  if (pmatch(method, c("sequence", "random")) == 1)
    return (seq(start, end, by=by, ...))
  if (pmatch(method, c("sequence", "random")) == 2) {
    tmp <- sample(1:ceiling(difftime(end, start, "days")), n)
    res <- start + tmp
    res <- res[order(res)]
    return (res)
  }
}


#' Create times
#'
#' Create times with different methods
#'
#' @param start start time index
#' @param end end time index
#' @param by if method is sequence, seq() argument by
#' @param n if method is random, vector of length n dates to create
#' @param method partial matching methods to be used
#' @return a vector of dates
#' @examples
#' t1 <- create_times("2015-01-01 00:00:00", "2015-01-01 23:59:59", by=1, n=10)
#' @export
create_times <- function(start, end, by, n, tz="UTC",
                         method = c("sequence", "random")[1], ...)
{
  if (!inherits(start, "POSIXct")) {
    start <- as.POSIXct(start, tz = tz)
  }

  if (!inherits(end, "POSIXct")) {
    end <- as.POSIXct(end, tz = tz)
  }

  if (pmatch(method, c("sequence", "random")) == 1)
    return (seq(start, end, by=by, ...))

  if (pmatch(method, c("sequence", "random")) == 2) {
    tmp <- sample(1:ceiling(difftime(end, start, "days")), n)
    res <- start + tmp
    res <- res[order(res)]
    return (res)
  }
}

getyear <- function(date, long=TRUE)
{
  date <- as.Date(date)
  if (long)
    return(format(date, "%Y"))
  else
    return(format(date, "%y"))
}

getmonth<- function(date, abbr=F)
{
  date <- as.Date(date)
  if (abbr)
    return(format(date, "%b"))
  else
    return(format(date, "%m"))
}

getday <- function(date)
{
  date <- as.Date(date)
  return(format(date, "%d"))
}




