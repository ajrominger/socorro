#' @title Empirical cumulative distribution function
#'  
#' @description Calculate a simple empirical cumulative distribution function that returns a matrix instead of a 
#' \code{stepfun}.
#' 
#' @details Calculate a simple empirical cumulative distribution function that returns a matrix instead of a 
#' \code{stepfun}.
#' 
#' @param x numeric vector
#' @param complement logical, should the eCDF be calculated from left to right (default) or right to left
#' 
#' @return A matrix with first column corresponding to the values of \code{x} and second column giving the 
#' cumulative density
#' 
#' @examples
#' x <- rnorm(100)
#' plot(simpECDF(x))
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @seealso ecdf, cumsum
#' @export

simpECDF <- function(x, complement = FALSE) {
	if(is.table(x)) {
		tabz <- x
	} else {
		tabz <- table(x)
	}
	if(complement) {
		yval <- cumsum(rev(as.numeric(tabz)))/sum(tabz)
		yval <- rev(yval)
	} else {
		yval <- cumsum(as.numeric(tabz))/sum(tabz)
	}
	xval <- as.numeric(names(tabz))
	
	return(cbind('x' = rev(xval), 'cdf(x)' = rev(yval)))
}
