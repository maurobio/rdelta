\name{DeltaParser}
\alias{DeltaParser}
\title{Create a DELTA Parser Object}
\description{
Creates a DeltaParser object for parsing DELTA files.
}
\usage{
DeltaParser(file)
}
\arguments{
  \item{file}{Path to the DELTA file to parse.}
}
\value{
An object of class \code{Rcpp_DeltaParser}.
}
\examples{
\dontrun{
parser <- DeltaParser("path/to/delta/file.txt")
}
}