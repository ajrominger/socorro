% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simpECDF.R
\name{simpECDF}
\alias{simpECDF}
\title{Empirical cumulative distribution function}
\usage{
simpECDF(x, complement = FALSE)
}
\arguments{
\item{x}{numeric vector}

\item{complement}{logical, should the eCDF be calculated from left to right (default) or right to left}
}
\value{
A matrix with first column corresponding to the values of \code{x} and second column giving the 
cumulative density
}
\description{
Calculate a simple empirical cumulative distribution function that returns a matrix instead of a 
\code{stepfun}.
}
\details{
Calculate a simple empirical cumulative distribution function that returns a matrix instead of a 
\code{stepfun}.
}
\examples{
x <- rnorm(100)
plot(simpECDF(x))

}
\seealso{
ecdf, cumsum
}
\author{
Andy Rominger <ajrominger@gmail.com>
}
