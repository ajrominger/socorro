kurt <- function(x,na.rm=FALSE) {
	diff <- x - mean(x,na.rm=na.rm)
	m4 <- mean(diff^4,na.rm=na.rm)
	m2 <- mean(diff^2,na.rm=na.rm)
	m4/(m2^2) - 3
}