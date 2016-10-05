#' @title Covert tidy data organization to matrix
#'  
#' @description Covert data organized by row (one observation per row) to a matrix where each unique group combination is summarized by the sum of its observations
#' 
#' @details See description
#' 
#' @param g1 grouping factor 1
#' @param g2 grouping factor 2
#' @param obs the observed values to sum
#' 
#' @return Numeric matrix with each cell the sum of observations belonging to g1 = i and g2 = j
#' 
# @examples
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @export

tidy2mat <- function(g1, g2, obs) {
    y <- tapply(obs, list(g1, g2), sum)
    y[is.na(y)] <- 0
    return(y)
}
