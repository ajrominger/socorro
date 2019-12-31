#' @title Log axes
#'  
#' @description Add good looking log scale axes to a plot
#' 
#' @details Powers of 10 receive a long tick mark and an axis label while integers that 
#' are not powers of 10 receive a short tick and no label
#' 
#' @param side the side on which to add an axis (following \code{axis}, but taking vectors
#' of length grater than 1, e.g. \code{1:2})
#' @param expLab logical, should axis labels be in exponential form (e.g. \eqn{10^1}; 
#' \code{expLab = TRUE}) 
#' @param minor logical, should minor tick marks be added
#' or in standard form (\code{expLab = FALSE}, the default)
#' @param ... arguments passed to \code{axis}
#' 
#' @examples
#' plot(1, xlim=c(1, 100), xaxt='n')
#' logAxis(1)
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @seealso axis, plot
#' @export

logAxis <- function(side, expLab = FALSE, minor = TRUE, ...) {
	if(length(side) > 1) {
	    foo <- lapply(side, function(i) logAxis(i, expLab, minor, ...))
	} else {
	    if(side %in% c(1, 3)) {
	        usr <- graphics::par('usr')[1:2]
	    } else {
	        usr <- graphics::par('usr')[3:4]
	    }
	    
	    maj.minX <- ceiling(usr[1])
	    maj.maxX <- floor(usr[2])
	    maj.seq <- maj.minX:maj.maxX
	    if(length(maj.seq) > 5) {
	        maj.seq <- pretty(maj.seq, n = 5)
	        noMinor <- TRUE
	    } else {
	        noMinor <- FALSE
	    }
	    
	    if(!minor) noMinor <- TRUE
	    
	    if(expLab) {
	        labels <- sapply(maj.seq, 
	                         function(p) eval(substitute(expression(10^p), 
	                                                     list(p = p))))
	        
	    } else {
	        labels <- TRUE
	    }
	    
	    graphics::axis(side, at=10^(maj.seq), labels = labels, ...)
	    
	    if(!noMinor) {
	        if(maj.minX - usr[1] >= log(2, 10)) {
	            min.incX <- rep(log(2:9, 10), length(maj.minX:maj.maxX)+1)
	            min.incX <- min.incX + unlist(lapply((maj.minX-1):maj.maxX, rep, times=8))
	            min.incX <- min.incX[min.incX >= usr[1] & min.incX <= usr[2]]
	        } else {
	            min.incX <- rep(log(2:9, 10), length(maj.minX:maj.maxX))
	            min.incX <- min.incX + unlist(lapply(maj.minX:maj.maxX, rep, times=8))
	            min.incX <- min.incX[min.incX >= usr[1] & min.incX <= usr[2]]
	        }
	        
	        graphics::axis(side, at=10^min.incX, labels=FALSE, col.ticks=par('fg'), 
	                       tcl=par('tcl')*0.6)
	    }
	}
}
