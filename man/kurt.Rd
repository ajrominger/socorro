% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kurt.R
\name{kurt}
\alias{kurt}
\title{Kurtosis}
\usage{
kurt(x, na.rm = FALSE)
}
\arguments{
\item{x}{numeric vector}

\item{na.rm}{logical, should NA values be removed prior to computation}
}
\value{
Numeric vector of length one giving the kurtosis value
}
\description{
Calculate the kurtosis of a random sample
}
\details{
Calculate the kurtosis of a random sample
}
\examples{
x <- rnorm(100)
kurt(x)

}
\seealso{
mean, var, skew
}
\author{
Andy Rominger <ajrominger@gmail.com>
}
