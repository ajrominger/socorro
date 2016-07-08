#' @title Axis arrows
#'  
#' @description Add arrows to axes to indicate direction
#' 
#' @details This function puts arrows in the margin space and will try to fit the arrows nicely, 
#' but assumes some space has been left for them (e.g. through \code{mar} argument to \code{par})
#' 
#' @param side numeric indicating which sides(s) to put arrows on (following convention of \code{axis})
#' @param backward logical, should the arrow be backwards (i.e. pointing down if side is 2 or 4, or 
#' pointing left if side is 1 or 3)
#' @param ... arguments passed to function \code{arrows}
#' 
#' @examples
#' plot(1)
#' axisArrows(side=1:2, length=0.1)
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @seealso axis, arrows, logAxis
#' @export

axisArrows <- function(side=1:2, backward=FALSE, ...) {
    usr <- graphics::par('usr')
    
    if(graphics::par('xlog')) usr[1:2] <- 10^usr[1:2]
    if(graphics::par('ylog')) usr[3:4] <- 10^usr[3:4]
    
    if(backward) {
        xx <- usr[2:1]
        yy <- usr[4:3]
    } else {
        xx <- usr[1:2]
        yy <- usr[3:4]
    }
    
    graphics::par(xpd=NA)
    
    if(1 %in% side) graphics::arrows(x0=xx[1] + 0.1*diff(xx), y0=min(yy) - 0.04*diff(yy), 
                                     x1=xx[2] - 0.1*diff(xx), ...)
    
    if(3 %in% side) graphics::arrows(x0=xx[1]+0.1*diff(xx), y0=max(yy) + 0.04*diff(yy), 
                                     x1=xx[2]-0.1*diff(xx), ...)
    
    if(2 %in% side) graphics::arrows(x0=min(xx) - 0.04*diff(xx), y0=yy[1] + 0.1*diff(yy),
                                     y1=yy[2] - 0.1*diff(yy), ...)
    
    if(4 %in% side) graphics::arrows(x0=max(xx) + 0.04*diff(xx), y0=yy[1] + 0.1*diff(yy),
                                     y1=yy[2] - 0.1*diff(yy), ...)
    
    graphics::par(xpd=FALSE)
}
