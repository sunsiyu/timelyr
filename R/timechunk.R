#' Return consecutive time chunks
#'
#' Split data frame into consecutive chunks based on the column time and two
#'  pre-defined chunk-related thresholds.
#'
#' @param df a data frame with a column named 'time' and of class 'POSIXct'
#' @param max_gap maximum time gap allowed to be counted as one chunk [s]
#' @param min_len minimum length of a chunk
#' @return a list of splitted chunks
#' @examples
#' \dontrun{
#'  chn1 <- getchunk(df)
#'  chn2 <- getchunk(df, max_gap = 100, min_len = 0)
#' }
#' @export
getchunk <- function(df, max_gap = 600, min_len = 10)
{
  stopifnot(is.data.frame(df))

  if (!("time" %in% names(df) && inherits(df$time, "POSIXct"))) {
    stop ("df must have a column named \"time\" and of class 'POSIXct'!")
  }

  if (nrow(df) < 1) {
    return(list())
  }

  borders <- getborders(df, max_gap, min_steps, ind_time = F)

  chunks <- lapply(seq(along = borders$start), function(i) {
    df[borders$start[i]:borders$end[i], , drop = FALSE]})

  return(chunks)
}


#' Return borders of time chunks
#'
#' Return the start and end time index of each chunk
#'
#' @param df a data frame with one column of "time" and of class POSIXct
#' @param max_gap maximum time gap allowed to be counted as one chunk [s]
#' @param min_len minimum length of a chunk
#' @param ind_time a logical value, if TRUE, time index will be returned
#' @return a data frame with two columns:
#'  - start: start time index of each chunk
#'  - end: end time index of each chunk
#' @examples
#' \dontrun{
#'  br1 <- getborders(df)
#'  br2 <- getborders(df, max_gap = 100, min_len = 0, ind_time = T)
#' }
#' @export
getborders <- function(df, max_gap = 600, min_len = 10, ind_time = F)
{
  stopifnot(is.data.frame(df))
  if (!("time" %in% names(df) && inherits(df$time, "POSIXct"))) {
    stop ("df must have a column named \"time\" and of class 'POSIXct'!")
  }
  stopifnot(is.numeric(max_gap) && is.numeric(min_len))

  if (nrow(df) < 1) {
    return(data.frame(start = numeric(), end = numeric()))
  }

  # find chunks with gap smaller than max_gap
  tdiff <- c(0, difftime(df$time[-1], df$time[-nrow(df)], units = "secs"))
  borders <- unique(c(1, which(tdiff > max_gap), nrow(df)))

  # find chunks with length larger than min_len
  start <- borders[which(diff(borders) > min_len)]
  end <- borders[which(diff(borders) > min_len)+1]-1

  if (ind_time) {
    start <- df$time[start]
    end <- df$time[end]
  }

  return(data.frame(start = start, end = end))
}

