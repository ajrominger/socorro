skew <- function(x,na.rm=FALSE) {
	diff <- x - mean(x,na.rm=na.rm)
	m3 <- mean(diff^3,na.rm=na.rm)
	m2 <- mean(diff^2,na.rm=na.rm)
	m3/(m2^(3/2))
}