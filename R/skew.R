#' @title Skewness
#'  
#' @description Calculate the skewness of a random sample
#' 
#' @details Calculate the skewness of a random sample
#' 
#' @param x numeric vector
#' @param na.rm logical, should NA values be removed prior to computation
#' 
#' @return Numeric vector of length one giving the kurtosis value
#' 
#' @examples
#' x <- rnorm(100)
#' skew(x)
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @seealso mean, var, kurt
#' @export

skew <- function(x,na.rm=FALSE) {
	xdiff <- x - mean(x, na.rm=na.rm)
	m3 <- mean(xdiff^3, na.rm=na.rm)
	m2 <- mean(xdiff^2, na.rm=na.rm)
	m3/(m2^(3/2))
}