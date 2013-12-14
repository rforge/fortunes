\name{fortunes}
\alias{fortunes}
\alias{read.fortunes}
\alias{fortune}
\alias{print.fortune}
\alias{toLatex.fortune}
\title{R Fortunes}
\description{Read and print R fortunes.}
\usage{
  fortune(which = NULL, fortunes.data = NULL, fixed = TRUE,
          showMatches = FALSE, ...)
  \method{print}{fortune}(x, width = NULL, ...)
  read.fortunes(file = NULL)
}
\arguments{
  \item{which}{an integer specifying the row number of \code{fortunes.data}.
    Alternatively \code{which} can be a character and \code{grep} is used
    to try to find a suitable row.}
  \item{fortunes.data}{data frame containing a fortune in each row.  By default
    the fortune data from the \pkg{fortunes} package are used.}
  \item{fixed}{logical passed to \code{\link{grep}} if \code{which} is a character,
    indicating if it should work (if \code{TRUE}, as by default) with a simple character string
    or (if \code{FALSE}) with regular expressions.}
  \item{showMatches}{if \code{which} is character, a logical indicating
    if \code{fortune()} should print all the row numbers of
    \code{fortunes.data} which match the \code{grep} search.}
  \item{\dots}{potential further arguments passed to \code{\link{grep}}.}
  \item{x}{an object of class \code{"fortune"}, usually a single row
    from \code{fortunes.data}.}
  \item{width}{integer specifying the character width.  By default
    \code{getOption("width")} is used.}
  \item{file}{A character string giving a fortune database in csv format (in UTF-8 encoding).
    By default all csv files in the data directory of the fortune package are used.}
}

\value{
  \code{fortune()} returns an object of class \code{"fortune"} which is
  a row from a data frame of fortunes (like those read in from \code{read.fortunes}).

  \code{read.fortunes()} returns a data frame of fortunes, each row contains:
  \item{quote}{the quote, main part of the fortune,}
  \item{author}{the author of the quote,}
  \item{context}{the context in which it was quoted (if available, otherwise \code{NA}),}
  \item{source}{where it was quoted (if available, otherwise \code{NA}),}
  \item{date}{when it was quoted (if available, otherwise \code{NA}).}
}

\examples{
fortune()
fortune("Ripley")
fortune(17)

\donttest{
## requires at least R 3.0.0
fortune("parse", showMatches = TRUE)# -> shows at least 5 matches
## The first three "all together":
lapply(setNames(, c(38, 106, 129)), fortune)
}
}
\keyword{misc}
