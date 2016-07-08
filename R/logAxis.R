logAxis <- function(side, ...) {
	if(side %in% c(1,3)) usr <- par("usr")[1:2]
	if(side %in% c(2,4)) usr <- par("usr")[3:4]
	
	maj.minX <- ceiling(usr[1])
	maj.maxX <- floor(usr[2])
	
	if(side %in% c(1,3)) axis(side,xaxp=c(10^maj.minX,10^maj.maxX,1), ...)
	if(side %in% c(2,4)) axis(side,yaxp=c(10^maj.minX,10^maj.maxX,1), ...)
	
	if(maj.minX - usr[1] >= log(2,10)) {
		min.incX <- rep(log(2:9,10),length(maj.minX:maj.maxX)+1)
		min.incX <- min.incX + unlist(lapply((maj.minX-1):maj.maxX,rep,times=8))
		min.incX <- min.incX[min.incX >= usr[1] & min.incX <= usr[2]]
	} else {
		min.incX <- rep(log(2:9,10),length(maj.minX:maj.maxX))
		min.incX <- min.incX + unlist(lapply(maj.minX:maj.maxX,rep,times=8))
		min.incX <- min.incX[min.incX >= usr[1] & min.incX <= usr[2]]
	}
	
	axis(side,at=10^min.incX,labels=FALSE,col.ticks=par("fg"),tcl=par('tcl')*0.6)
}