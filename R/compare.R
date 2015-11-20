#' Create Plot to Compare different Time Series
#'
#' Create multiple time series on the same scale on the same plot, even the
#'  components of the times (year or month or date) are different.
#'
#' @param df a data.frame with first col times, second col values.
#' @return Invisible NULL
#' @export
compareplot <- function(df, var="year", type="continue")
{
  stopifnot(nrow(df) > 1)
  stopifnot(var %in% c("year", "month", "day"))
  stopifnot(type %in% c("continue", "control"))
  tmpdf <- df
  tmpdf$times <- df[, 1]
  tmpdf$values <- df[, 2]
  tmpdf$times <- ymd(tmpdf$times)
  years <-format(tmpdf$times, "%Y")
  noyears <- format(tmpdf$times, "%b%d")
  months <- format(tmpdf$times, "%b")
  days <- weekdays(tmpdf$times)
  if (var == "year") {
    color = format(tmpdf$times, "%Y")
  } else if (var == "month") {
    color = format(tmpdf$times, "%b")
  } else if (var == "day") {
    color = weekdays(tmpdf$times)
  }

  if (type == "continue")
    p <- ggplot(tmpdf, aes(times, values)) + geom_line(aes(color = color))

  if (type == "control")
    p <- ggplot(tmpdf, aes(noyears, values)) + geom_line(aes(color = color))

  return(p)
}
