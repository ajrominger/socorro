#' @title Add an alpha chanel to any color
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
#' @return A character vector of the desired colors
#' 
#' @examples
#' ## demonstrates use of the alpha argument: we can specify simple colors and use them in plots
#' ## along with semi-transparent versions of them
#' cols <- c('red', 'blue', 'green')
#' plot(1:3, col=cols, bg=colAlpha(cols, alpha=c(0.7, 0.4, 0.1)), pch=21, cex=2, lwd=2)
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @seealso hsv, rgb, col2rgb
#' @export

colAlpha <- function(col, alpha=1) {
	grDevices::rgb(t(grDevices::col2rgb(col)), alpha=alpha*255, maxColorValue=255)
}