read.fortunes <- function(file = NULL)
{
  if(!is.null(file) && file.exists(file)) {
    fortunes <- file
  } else {
    path <- .find.package("fortunes")
    datafiles <- list.files(file.path(path, "data"))
    if(!is.null(file) && file.exists(file.path(path, "data", file))) {
      fortunes <- file.path(path, "data", file)
    } else {
      if(!is.null(file)) stop(paste("sorry, `", file, "' not found", sep = ""))
      file <- datafiles[sapply(strsplit(datafiles, "\\."), function(x) (x[length(x)] == "csv"))]
      fortunes <- file.path(path, "data", file)
    }
  }

  rval <- NULL
  for(file in fortunes) {
    rval <- rbind(rval, read.table(file = file, header = TRUE, sep = ";", quote = "\"", colClasses = "character"))
  }
  return(rval)
}

fortunes.data <- read.fortunes()

fortune <- function(which = NULL, fortunes.data = NULL)
{
  if(is.null(fortunes.data)) fortunes.data <- get("fortunes.data", pos = "package:fortunes")
  if(is.null(which)) which <- sample(1:nrow(fortunes.data), 1)
  if(length(which) > 1) which <- which[1]
  rval <- fortunes.data[which, ]
  class(rval) <- "fortune"
  return(rval)
}

print.fortune <- function(x, width = NULL, ...)
{
  if(is.null(width)) width <- options()$width
  if(width < 10) stop("`width' must be greater than 10")

  if(is.na(x$context)) {
    x$context <- ""
  } else {
    x$context <- paste(" (", x$context, ")", sep = "")
  }
  if(is.na(x$source)) {
    x$source <- ""
  }
  if(is.na(x$date)) {
    x$date <- ""
  } else {
    x$date <- paste(" (", x$date, ")", sep = "")
  }
  if(any(is.na(x))) stop("`quote' and `author' are required")

  line1 <- x$quote
  line2 <- paste("   -- ", x$author, x$context, sep = "")
  line3 <- paste("      ", x$source, x$date, sep = "")

  ## Problem: account for chase where line contains "\n"
  linesplit <- function(line, width, gap = "      ") {
    if(nchar(line) < width) return(line)
    rval <- NULL
    while(nchar(line) > width) {
      line <- strsplit(line, " ")[[1]]
      if(any((nchar(line) + 1 + nchar(gap)) > width)) stop("`width' is too small for fortune")
      breakat <- which(cumsum(nchar(line) + 1) > width)[1] - 1
      rval <- paste(rval, paste(line[1:breakat], collapse = " "), "\n", sep = "")
      line <- paste(gap, paste(line[-(1:breakat)], collapse = " "), sep = "")
    }
    rval <- paste(rval, line, sep = "")
    return(rval)
  }

  line1 <- strsplit(line1, "<x>")[[1]]
  for(i in 1:length(line1))
    line1[i] <- linesplit(line1[i], width, gap = "")
  line1 <- paste(line1, collapse = "\n")
  line2 <- linesplit(line2, width)
  line3 <- linesplit(line3, width)

  cat(paste("\n", line1, "\n",
                  line2, "\n",
                  line3, "\n\n", sep = ""))
}

