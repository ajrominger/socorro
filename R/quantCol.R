#' @title Return colors based on a palette and quantitative variable
#'  
#' @description Assign colors to the values of a variable
#' 
#' @details This function linearly scales a variable to [0, 1] (potentially after a non-
#' linear transformation) and uses those scaled values as input into colorRamp
#' 
#' @param x the quantitative variable to be assigned colors
#' @param pal the colors to be supplied to colorRamp
#' @param trans 
#' @param xlim limits on the variable x, can be omitted, in which case the range of x is used
#' 
#' @return A character vector of the desired colors
#' 
#' @examples
#' x <- 1:10
#' plot(x, col = quantCol(x, c('blue', 'red')))
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @seealso colorRamp
#' @export

quantCol <- function(x, pal, trans = c('linear', 'log', 'quadratic', 'cubic'), xlim = NULL) {
    trans <- match.arg(trans, c('linear', 'log', 'quadratic', 'cubic'))
    
    ## generate function to transform data
    tfun <- switch(trans,
                   'linear' = function(x) x,
                   'log' = function(x) log(x),
                   'quadratic' = function(x) x^2,
                   'cubic' = function(x) x^3)
    
    ## calculate limits on x variable
    if(is.null(xlim)) xlim <- range(x)
    
    ## linearly scale transformed x to be bounded [0, 1]
    y <- 1 / diff(range(tfun(xlim))) * (tfun(x) - min(tfun(xlim)))
    
    ## remove NAs to be dealt with later
    yna <- is.na(y)
    y[yna] <- 0
    
    ## generate colors
    out <- rgb(grDevices::colorRamp(pal)(y), maxColorValue = 255)
    
    ## make missing values transparent
    out[yna] <- 'transparent'
    
    return(out)
}
