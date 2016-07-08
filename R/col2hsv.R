col2hsv <- function(col,print=FALSE,...) {
	rgb.vec <- col2rgb(col)
	hsv.vec <- rgb2hsv(rgb.vec[1],rgb.vec[2],rgb.vec[3])
	if(print) {
		res <- as.vector(hsv.vec)
		names(res) <- c("h","s","v")
	} else {
		res <- hsv(hsv.vec[1],hsv.vec[2],hsv.vec[3],...)
	}
	res
}