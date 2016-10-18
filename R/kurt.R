#' @title Kurtosis
#'  
#' @description Calculate the kurtosis of a random sample
#' 
#' @details Calculate the kurtosis of a random sample
#' 
#' @param x numeric vector
#' @param na.rm logical, should NA values be removed prior to computation
#' 
#' @return Numeric vector of length one giving the kurtosis value
#' 
#' @examples
#' x <- rnorm(100)
#' kurt(x)
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @seealso mean, var, skew
#' @export

kurt <- function(x, na.rm = FALSE) {
	xdiff <- x - mean(x, na.rm = na.rm)
	m4 <- mean(xdiff^4, na.rm = na.rm)
	m2 <- mean(xdiff^2, na.rm = na.rm)
	m4/(m2^2) - 3
}
