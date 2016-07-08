paleoAxis <- function(side=1,showQ=FALSE,lspace=1.5) {
	## names and times
	epoc.name <- c("Q","N","Pg","K","J","T","P","C","D","S","O","Cm","PreCm")
	epoc.time <- c(0,2.588,23.03,65.5,145.5,199.6,251.0,299.0,359.2,416.0,443.7,488.3,542.0,3800)
	
	usr <- par('usr')
	if(par('xlog')) usr[1:2] <- 10^usr[1:2]
	if(par('ylog')) usr[3:4] <- 10^usr[1:2]
	
	if(!showQ) {
		epoc.name <- epoc.name[-1]
		epoc.time <- epoc.time[-2]
	}
	
	## get time and rectangle limits depending on side for plotting
	if(side == 1 | side == 3) {
		tlim <- usr[1:2]
		scl <- lspace*diff(usr[3:4]) / (par("fin")[2] - sum(par("mai")[c(1,3)]))
		
		if(side == 1) {
			rlim <- c(0, -par("mai")[side]/par("mar")[side])*scl + usr[3]
		} else {
			rlim <- c(0, par("mai")[side]/par("mar")[side])*scl + usr[4]
		}
		
		prect <- function() rect(t0,rlim[1],t1,rlim[2]) 
		strdim <- function(s) strwidth(s)
		
		new.name <- epoc.name[which(usr[2] <= epoc.time[-1] & epoc.time[-length(epoc.time)] <= usr[1])]
	} else {
		tlim <- usr[3:4]
		scl <- lspace*diff(usr[1:2]) / (par("fin")[1] - sum(par("mai")[c(2,4)]))
		
		if(side == 2) {
			rlim <- c(0, -par("mai")[side]/par("mar")[side])*scl + usr[1]
		} else {
			rlim <- c(0, par("mai")[side]/par("mar")[side])*scl + usr[2]
		}
		
		prect <- function() rect(rlim[1],t0,rlim[2],t1)
		strdim <- function(s) strwidth(s) * abs(diff(usr[3:4])/diff(usr[1:2]))
		
		new.name <- epoc.name[which(usr[4] <= epoc.time[-1] & epoc.time[-length(epoc.time)] <= usr[3])]
	}
	
	## rectagle edges
	t0 <- epoc.time[min(tlim) <= epoc.time & epoc.time < max(tlim)]
	t1 <- c(epoc.time[min(tlim) <= epoc.time & epoc.time < max(tlim)], max(tlim))
	if(min(tlim) > 0 & !(min(tlim) %in% t0)) {
		t0 <- c(min(tlim), t0)
	} else {
		t1 <- t1[-1]
	}
	
	## epoch names and name positions (tat)
	new.name[abs(strdim(new.name)) > abs(t1-t0)] <- ""
	tat <- (t0 + t1)/2
	
	## make rectangles, axis and names
	prect()
	axis(side,line=lspace)
	mtext(new.name,side=side,line=0.25,at=tat)
}

par(mar=c(4,4,4,4),mgp=c(3,1,0),xpd=NA)
plot(1,xlim=c(650,0),ylim=c(650,100),axes=FALSE)
paleoAxis(1)
paleoAxis(2)