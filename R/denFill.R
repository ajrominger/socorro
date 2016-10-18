#' @title Density plot with confidence interval filled in
#'  
#' @description Plot the interpolated density of data (most likely results from a Monte Carlo simulation 
#' or estimation) along with a confidence interval (specified by \code{alpha}) shadded in.
#' 
#' @details This function can plot desnity and quantiles for any vector and would be well suited for bootstrapping, 
#' permutational null models and also credible intervals from Bayesian MCMC samples
#' 
#' @param dist numeric vector for which density is to be plotted
#' @param alpha critical value
#' @param alternative specify whether to plot a two-sided CI, upper or lower
#' @param col color for plotting
#' @param lwd line width
#' @param transp transparancy of shadded region
#' @param add logical, whether to add to an existing plot
#' @param ... further arguments passed to \code{plot}
#' 
#' 
#' @examples
#' x <- rnorm(100)
#' denFill(x)
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @seealso density, quantile
#' @export

denFill <- function(dist, alpha = 0.05, alternative  =  c('two.sided', 'less', 'greater'), 
                    col = 'black', lwd = 2, transp = 0.3, add = FALSE, ...) {    
    alternative <- match.arg(alternative, c('two.sided', 'less', 'greater'))
    
    ## specify the densities
    den1 <- stats::density(dist)
    quant1 <- stats::quantile(dist, c(alpha/2, 1-alpha/2))
    
    ## establish plot window, axes, etc.
    if(!add) graphics::plot(den1, col = 'transparent', ...)
    
    ## compute polygon coords for CI
    temp1 <- cbind(den1$x, den1$y)
    
    temp1.mid <- temp1[which(quant1[1] < temp1[, 1] & temp1[, 1] < quant1[2]), ]
    x1.cord <- c(min(temp1.mid[, 1]), temp1.mid[, 1], max(temp1.mid[, 1]))
    y1.cord <- c(0, temp1.mid[, 2], 0)
    
    ##	compute color
    coll <- colAlpha(col, transp)
    
    polygon(x1.cord, y1.cord, col = colp, border = NA)
    segments(min(temp1.mid[, 1]), 0, min(temp1.mid[, 1]), temp1.mid[1, 2], col = coll)
    segments(max(temp1.mid[, 1]), 0, max(temp1.mid[, 1]), temp1.mid[length(temp1.mid[, 2]), 2], col = coll)
    
    ## add lines last for better visual presentation
    lines(temp1, lwd = lwd, col = coll)
}
