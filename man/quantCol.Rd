% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantCol.R
\name{quantCol}
\alias{quantCol}
\title{Return colors based on a palette and quantitative variable}
\usage{
quantCol(x, pal, trans = c("linear", "log", "quadratic", "cubic"),
  xlim = NULL)
}
\arguments{
\item{x}{the quantitative variable to be assigned colors}

\item{pal}{the colors to be supplied to colorRamp}

\item{trans}{the transformation to use on \code{x}}

\item{xlim}{limits on the variable x, can be omitted, in which case the range of x is used}
}
\value{
A character vector of the desired colors
}
\description{
Assign colors to the values of a variable
}
\details{
This function linearly scales a variable to [0, 1] (potentially after a non-
linear transformation) and uses those scaled values as input into colorRamp
}
\examples{
x <- 1:10
plot(x, col = quantCol(x, c('blue', 'red')))

}
\seealso{
colorRamp
}
\author{
Andy Rominger <ajrominger@gmail.com>
}
