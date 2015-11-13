getfileext <- function(path) {
  stopifnot(is.character(path))
  splitpath <- strsplit(path, split="\\.")[[1]]
  if (length(splitpath) <= 1)
    return(NULL)
  else
    return(splitpath[length(splitpath)])
}

df <- data.frame(name = c("name1", "name2", "name3"),
                 start = c("20150101", "20150201", "20150301"),
                 end = c("20150110", "20150220", "20150330"),
                 flag = c("good", "med", "bad"))

statusplot <- function(df)
{
  df$name <- as.factor(df$name)
  df$start <- as.Date(df$start, "%Y%m%d")
  df$end <- as.Date(df$end, "%Y%m%d")
  df$flag <- ordered(df$flag, levels=c("good", "med", "bad"))
  p <- ggplot(df, aes(xmin = as.Date(start),
                      xmax = as.Date(end),
                      ymin = which(df$flag==levels(df$flag)),
                      ymax = which(df$flag==levels(df$flag)) - 0.9))
  p + geom_rect(fill = "yellow") +
    scale_x_date(breaks = "1 month") +
    scale_y_continuous(breaks = seq(0.5, length(levels(df$name))-0.5, 1), labels = levels(df$name)) +
    theme(axis.text.x = element_text(angle = 90)) +
    xlab("Date") +
    ylab("Names")
}
