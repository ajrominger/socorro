#' @title Add an alpha chanel to any color
#'  
#' @description Make any color have transparancy by adding an alpha chanel
#' 
#' @details This function converts a color to red, blue, green components and then adds the desired alpha level
#' 
#' @param col the color to be modified
#' @param alpha the alpha level to be added
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
