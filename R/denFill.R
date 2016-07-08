den.fill <- function(dist, alpha=0.05, col="black", lwd=2, transp=0.3, add=FALSE, ...) {    
    ## specify the two densities
    den1 <- density(dist)
    quant1 <- quantile(dist,c(alpha/2,1-alpha/2))
    
    ## establish plot window, axes, etc.
    if(!add) plot(den1,col="transparent", ...)
    
    ## compute polygon coords for CI
    temp1 <- cbind(den1$x,den1$y)
    
    temp1.mid <- temp1[which(quant1[1] < temp1[,1] & temp1[,1] < quant1[2]),]
    x1.cord <- c(min(temp1.mid[,1]),temp1.mid[,1],max(temp1.mid[,1]))
    y1.cord <- c(0,temp1.mid[,2],0)
    
    ##	compute color
    col.temp <- col2rgb(col)
    colp <- rgb(col.temp[1,1], col.temp[2,1], col.temp[3,1], alpha=transp*255, maxColorValue=255)
    coll <- col
    
    polygon(x1.cord,y1.cord,col=colp,border=NA)
    segments(min(temp1.mid[,1]),0,min(temp1.mid[,1]),temp1.mid[1,2],col=coll)
    segments(max(temp1.mid[,1]),0,max(temp1.mid[,1]),temp1.mid[length(temp1.mid[,2]),2],col=coll)
    
    ## add lines last for better visual presentation
    lines(temp1,lwd=lwd,col=coll)
}
