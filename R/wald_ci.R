##	function to calculate the Wald confidence interval
##	of an MLE

wald.ci <- function(mle,hessian,alpha) {
	se <- sqrt(diag(solve(hessian)))
	zcrit <- qnorm(1-alpha/2)
	
	out <- matrix(c(mle-zcrit*se, mle+zcrit*se),nrow=length(mle))
	
	colnames(out) <- paste(c(alpha/2,1-alpha/2)*100,"%",sep="")
	
	return(out)
}

library(ellipse)
wald.ellipse <- function(mle,hessian,alpha) {
	var.cov <- solve(hessian)
	
#	cor.co <- mean(se[upper.tri(se) | lower.tri(se)]/prod(diag(se)))
	
	ellipse(var.cov,centre=mle,level=1-alpha)
#	ellipse(cor.co,centre=mle,scale=sqrt(diag(se)),level=1-alpha)
	
}