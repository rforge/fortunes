\name{fortunes}
\alias{fortunes}
\alias{read.fortunes}
\alias{fortunes.data}
\alias{fortune}
\alias{print.fortune}
\title{R Fortunes}
\description{Read and print R fortunes.}
\usage{
  fortune(which = NULL, fortunes.data = NULL)
  \method{print}{fortune}(x, width = NULL, ...)
  read.fortunes(file = NULL)
}
\arguments{
  \item{which}{An integer specifying the row number of \code{fortunes.data}.
    Alternatively \code{which} can be a character and \code{grep} is used
    to try to find a suitable row.}
  \item{fortunes.data}{A data frame containing a fortune in each row. By default
    the fortune data from the fortune package are used.}
  \item{x}{An object of class \code{"fortune"}, usually a single row from \code{fortunes.data}.}
  \item{width}{Integer specifying the character width. By default \code{getOption("width")}
    is used.}
  \item{...}{Currently not used.}
  \item{file}{A character string giving a fortune database in csv format.
    By default all csv files in the data directory of the
    fortune package are used.}
}

\value{
  \code{fortune()} returns an object of class \code{"fortune"} which is
  a row from a data frame of fortunes (like those read in from \code{read.fortune}).

  \code{read.fortune()} returns a data frame of fortunes, each row contains
  \itemize
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
}
\keyword{misc}
