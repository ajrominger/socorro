paleoAxis <- function(showQ=FALSE,tcol="transparent") {
	tline.space <- 0.08		# proportion of vertical used for time line
	Mwid <- 0.025			# approx proportional width of 'M' character
	
#	names and times
	epoc.name <- c("Q","N","Pg","K","J","T","P","C","D","S","O","Cm","PreCm")
	epoc.time <- c(0,2.588,23.03,65.5,145.5,199.6,251.0,299.0,359.2,416.0,443.7,488.3,542.0,3800)
	if(!showQ) {
		epoc.name <- epoc.name[-(1:2)]
	}
	
#	calculate space for time line
	usr <- par("usr")
	xfact <- usr[1] - usr[2]
	yfact <- tline.space*(usr[4] - usr[3])

#	calculate which epochs can be printed
	new.time <- epoc.time[which(usr[2] <= epoc.time & epoc.time <= usr[1])]
	if(usr[2] >= min(epoc.time)) {
		new.time <- c(usr[2],new.time)
	}
	if(usr[1] <= max(epoc.time)) {
		new.time <- c(new.time,usr[1])
	}
	num.time <- length(new.time)
	
#	calcualte which names can be printed
	new.name <- epoc.name[which(usr[2] <= epoc.time[-1] & epoc.time[-length(epoc.time)] <= usr[1])]
	epoc.mid <- (new.time[-1] + new.time[-num.time])/2
	
#	if there's not enough space, don't print name
	prop.binW <- (new.time[-1] - new.time[-num.time])/xfact
	prop.charW <- nchar(new.name)*Mwid
	new.name[prop.charW >= prop.binW] <- ""
	
	rect(new.time[-num.time],usr[3],new.time[-1],usr[3]-yfact,col=tcol)
	text(epoc.mid,usr[3]-0.5*yfact,labels=new.name)
}
