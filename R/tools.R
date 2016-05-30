#' Get File Extention
#'
#' @param path character of length 1
#' @return character of file extension
#' @examples
#' fileext <- getfileext("example.R")
#' @export
getfileext <- function(path) {
  stopifnot(is.character(path))
  splitpath <- strsplit(path, split="\\.")[[1]]
  if (length(splitpath) <= 1)
    return(NULL)
  else
    return(splitpath[length(splitpath)])
}

#' Create Process Status Plot
#'
#' Create process status plot, with each bar indicating each process, and each
#' color indicating different categories of status.
#'
#' @param df a data.frame
#' @return Invisible NULL
#' @examples
#' statusplot(data.frame(name = c("name1", "name2", "name3"),
#'  start = c("20150101", "20150201", "20150301"),
#'  end = c("20150110", "20150220", "20150330"),
#'  flag = c("good", "med", "bad"), stringsAsFactors = F))
#' @export
statusplot <- function(df)
{
  df$name <- as.factor(df$name)
  if (length(levels(df$name)) == 1){
    ymin = rep(1, nrow(df))
  } else {
    ymin = which(levels(df$name) == df$name)
  }

  df$start <- as.Date(as.character(df$start), "%Y%m%d")
  df$end <- as.Date(as.character(df$end), "%Y%m%d")
  df$flag <- ordered(as.character(df$flag), levels=c("good", "med", "bad"))

  p <- ggplot(df, aes(xmin = as.Date(start),
                      xmax = as.Date(end),
                      ymin = ymin,
                      ymax = ymin + 0.9,
                      fill = factor(flag)))
  p <- p + geom_rect() +
    scale_y_continuous(breaks = seq(1.5, length(levels(df$name))+0.5, 1), labels = levels(df$name)) +
    theme(axis.text.x = element_text(angle = 90)) +
    xlab("Date") +
    ylab("Names")

  return(p)
}
