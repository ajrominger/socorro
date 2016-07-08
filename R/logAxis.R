#' @title Log axes
#'  
#' @description Add good looking log scale axes to a plot
#' 
#' @details Powers of 10 receive a long tick mark and an axis label while integers that are not 
#' powers of 10 receive a short tick and no label
#' 
#' @param side the side on which to add an axis (following \code{axis})
#' @param ... arguments passed to \code{axis}
#' 
#' @examples
#' plot(1, xlim=c(1, 100), xaxt='n')
#' logAxis(1)
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @seealso axis, plot
#' @export

logAxis <- function(side, ...) {
	if(side %in% c(1, 3)) usr <- graphics::par('usr')[1:2]
	if(side %in% c(2, 4)) usr <- graphics::par('usr')[3:4]
	
	maj.minX <- ceiling(usr[1])
	maj.maxX <- floor(usr[2])
	
	if(side %in% c(1, 3)) graphics::axis(side, xaxp=c(10^maj.minX, 10^maj.maxX, 1), ...)
	if(side %in% c(2, 4)) graphics::axis(side, yaxp=c(10^maj.minX, 10^maj.maxX, 1), ...)
	
	if(maj.minX - usr[1] >= log(2, 10)) {
		min.incX <- rep(log(2:9, 10), length(maj.minX:maj.maxX)+1)
		min.incX <- min.incX + unlist(lapply((maj.minX-1):maj.maxX, rep, times=8))
		min.incX <- min.incX[min.incX >= usr[1] & min.incX <= usr[2]]
	} else {
		min.incX <- rep(log(2:9, 10), length(maj.minX:maj.maxX))
		min.incX <- min.incX + unlist(lapply(maj.minX:maj.maxX, rep, times=8))
		min.incX <- min.incX[min.incX >= usr[1] & min.incX <= usr[2]]
	}
	
	graphics::axis(side, at=10^min.incX, labels=FALSE, col.ticks=par('fg'), tcl=par('tcl')*0.6)
}
