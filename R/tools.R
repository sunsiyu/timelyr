getfileext <- function(path) {
  stopifnot(is.character(path))
  splitpath <- strsplit(path, split="\\.")[[1]]
  if (length(splitpath) <= 1)
    return(NULL)
  else
    return(splitpath[length(splitpath)])
}


