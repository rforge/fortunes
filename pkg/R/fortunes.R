### Use some newer R functions, not yet requiring 'R >= *' in DESCRIPTION 'Depends:'
if((Rv <- getRversion()) < "3.2.1") {
    lengths <- function (x, use.names = TRUE) vapply(x, length, 1L, USE.NAMES = use.names)
    if(Rv < "3.1.0") {
        anyNA <- function(x) any(is.na(x))
        if(Rv < "3.0.0") {
            rep_len <- function(x, length.out) rep(x, length.out=length.out)
            if(Rv < "2.15")
                paste0 <- function(...) paste(..., sep = '')
        }
    }
} ## R < 3.2.1
rm(Rv)

read.fortunes <- function(file = NULL)
{
  if(!is.null(file)) {
    fortunes <- file[file.exists(file)]
  } else {
    path <- system.file("fortunes", package = "fortunes")
    datafiles <- list.files(path)
    if(!is.null(file) && file.exists(file.path(path, file))) {
      fortunes <- file.path(path, file)
    } else {
      if(length(file) > 0L) stop("sorry, ", sQuote(file), " not found")
      file <- datafiles[grep("\\.csv$", datafiles)]
      if(length(file) == 0L) stop("sorry, no fortunes data found")
      fortunes <- file.path(path, file)
    }
  }

  rval <- NULL
  for(file in fortunes) {
    rval <- rbind(rval, read.table(file, header = TRUE, sep = ";",
				   quote = "\"", colClasses = "character"))
  }
  rval
}

fortunes.env <- new.env()

fortune <- function(which = NULL, fortunes.data = NULL, fixed = TRUE,
                    showMatches = FALSE, author = character(), ...)
{
  if(is.null(fortunes.data)) {
    if(is.null(fortunes.env$fortunes.data)) fortunes.env$fortunes.data <- read.fortunes()
    fortunes.data <- fortunes.env$fortunes.data
  }

  if(is.null(which)) which <- sample.int(nrow(fortunes.data), 1)
  else if(is.character(which)) {
    if(length(author)) {
      if(is.null(fd.auth <- fortunes.data[,"author"]))
          warning("The 'fortunes.data' do not have an \"author\" column")
      else
          fortunes.data <-
              fortunes.data[ grep(author, fd.auth, useBytes=TRUE, fixed=fixed), ]
    }
    fort <- apply(fortunes.data, 1, function(x) paste(x, collapse = " "))
    which1 <- grep(which, fort, useBytes = TRUE, fixed = fixed, ...)
    if(length(which1) < 1)
      which1 <- grep(tolower(which), tolower(fort), useBytes = TRUE, fixed = TRUE)
    if(showMatches) cat("Matching row numbers:",
			paste(which1, collapse=", "), "\n")
    which <- which1
    if(length(which) > 1) which <- sample(which)[1]
  }
  if(length(which) > 0 && which %in% seq(along = rownames(fortunes.data))) {
    structure(fortunes.data[which, ], class = "fortune")
  } else character(0)
}

print.fortune <- function(x, width = NULL, ...)
{
  if(is.null(width)) width <- getOption("width")
  if(width < 10) stop("'width' must be greater than 10")

  x$context <- if(is.na(x$context)) "" else paste0(" (", x$context, ")")
  if(is.na(x$source)) {
    x$source <- ""
  }
  x$date <- if(is.na(x$date)) "" else paste0(" (", x$date, ")")
  if(anyNA(x)) stop("'quote' and 'author' are required")

  line1 <- x$quote
  line2 <- paste0("   -- ", x$author, x$context)
  line3 <- paste0("      ", x$source, x$date)

  ## Problem: account for chase where line contains "\n"
  linesplit <- function(line, width, gap = "      ") {
    if(nchar(line) < width) return(line)
    rval <- NULL
    while(nchar(line) > width) {
      line <- strsplit(line, " ")[[1]]
      if(any((nchar(line) + 1 + nchar(gap)) > width))
          stop("'width' is too small for fortune")
      breakat <- which.max(cumsum(nchar(line) + 1) > width) - 1L
      rval <- paste0(rval, paste(line[1:breakat], collapse = " "), "\n")
      line <- paste0(gap, paste(line[-(1:breakat)], collapse = " "))
    }
    paste0(rval, line)
  }

  line1 <- strsplit(line1, "<x>")[[1]]
  for(i in 1:length(line1))
    line1[i] <- linesplit(line1[i], width, gap = "")
  line1 <- paste(line1, collapse = "\n")
  line2 <- linesplit(line2, width)
  line3 <- linesplit(line3, width)

  cat(paste0("\n",
             line1, "\n",
             line2, "\n",
             line3, "\n\n"))
}

toLatex.fortune <- function(object, number = FALSE, width = c(1, 0.85), ...) {
  width <- rep(width, length.out = 2)
  escape_latex <- function(x) {
    x <- gsub("\\\\ ", "\\textbackslash\\ ", x, fixed=TRUE)
    x <- gsub("\\\\", "\\textbackslash ", x, fixed=TRUE)
    x <- gsub("\\n", "\\textbackslash n", x, fixed=TRUE)
    x <- gsub("#", "\\#", x, fixed=TRUE)
    x <- gsub("$", "\\$", x, fixed=TRUE)
    x <- gsub("&", "\\&", x, fixed=TRUE)
    x <- gsub("~ ", "\\textasciitilde\\ ", x, fixed=TRUE)
    x <- gsub("~", "\\textasciitilde ", x, fixed=TRUE)
    x <- gsub("_", "\\_", x, fixed=TRUE)
    x <- gsub("^", "\\verb|^|", x, fixed=TRUE)
    x <- gsub("%", "\\%", x, fixed=TRUE)
    x <- gsub("{", "\\{", x, fixed=TRUE)
    x <- gsub("}", "\\}", x, fixed=TRUE)
    x <- gsub(" '", " `", x, fixed=TRUE)
    x <- gsub(" \"", " ``", x, fixed=TRUE)
    x <- gsub("...", "{\\dots}", x, fixed=TRUE)
    x <- gsub(" - ", " -- ", x, fixed=TRUE)
    x
  }
  if(is.na(object$context)) {
    object$context <- ""
  }
  if(is.na(object$source)) {
    object$source <- ""
  }
  object$date <- if(is.na(object$date)) "" else paste0(" (", object$date, ")")

  if(anyNA(object)) stop("'quote' and 'author' are required")
  quote <- strsplit(object$quote,"<x>")[[1]]
  quote <- c(rbind(t(quote),
                   t(rep("",length(quote)))))
  z <- paste0("\\begin{minipage}{", width[1], "\\textwidth}")
  z <- c(z, paste0(
    if(number) paste0("\\makebox[0pt][r]{\\tiny ", attr(object, "row.names"), "} ") else "",
    escape_latex(quote[1]))
  )
  z <- c(z, escape_latex(quote[-1]))
  z <- c(z, paste0("\\hfill---\\parbox[t]{", width[2], "\\textwidth}{\\emph{",
    escape_latex(object$author), "}"),
    if(object$context == "") "" else paste0("(", escape_latex(object$context), ")"),
    "",
    paste0(escape_latex(object$source), escape_latex(object$date), "}"),
    "\\end{minipage}")
  class(z) <- "Latex"
  z
}

