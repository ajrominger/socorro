#' @title Wald confidence interval
#'  
#' @description Functions to calculate the Wald confidence interval of an MLE
#' 
#' @details Can calcuate either a Wald CI for a single parameter or multiple parameters. The \code{mle} and 
#' \code{hessian} arguments can be obtained from numerical optimization of the likelihood function using, 
#' e.g., \code{optim} or \code{nlm}
#' 
#' @param mle maximum likelihood parameter estimate
#' @param hessian estiamte of the Hessian at the MLE
#' @param alpha critical value
#' @param marginal logical, should marginal CI be calculated for each parameter or should an ellipse 
#' be calculated simultaneously for all parameters
#' 
#' @return A matrix of either the upper and lower confidence limits (for \code{marginal=TRUE}) or a 
#' matrix with x,y coordinates fo the ellipse (for \code{marginal=FALSE})
#' 
# @examples
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @seealso optim, nlm
#' @export

wald <- function(mle, hessian, alpha=0.05, marginal=TRUE) {
	if(!marginal) {
	    var.cov <- solve(hessian)
	    
	    out <- ellipse::ellipse(var.cov, centre=mle, level=1-alpha)
	    
	    return(out)
	} else {
	    se <- sqrt(diag(solve(hessian)))
	    zcrit <- stats::qnorm(1-alpha/2)
	    
	    out <- matrix(c(mle-zcrit*se, mle+zcrit*se), nrow=length(mle))
	    colnames(out) <- paste(c(alpha/2, 1-alpha/2)*100, '%', sep='')
	    
	    return(out)
	}
}
