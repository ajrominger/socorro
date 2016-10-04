bigArrows <- function(x0, y0, x1, y1, col, a = 1, butt = c('h', 'v'), w0 = 0.05, w1 = 0.01, length = 0.05) {
    butt <- match.arg(butt, c('h', 'v'))
    
    plot(c(x0, x1), c(y0, y1), xlim = c(0, 4), ylim = c(0, 4))
    
    m <- (x0 - x1) / (y0 - y1)
    
    if(butt == 'h') {
        v <- c(x0 + a, y0)
    } else {
        v <- c(x0, y0 + a)
    }
    
    points(v[1], v[2], col = 'red')
    
    n <- 100
    
    theta0 <- atan2(y0 - v[2], x0 - v[1])
    theta1 <- atan2(y1 - v[2], x1 - v[1])
    if(theta0 < 0) theta0 <- 2*pi + theta0
    if(theta1 < 0) theta1 <- 2*pi + theta1
    theta <- seq(theta0, theta1, length = n)
    
    newa <- seq(sqrt((x0 - v[1])^2 + (y0 - v[2])^2), sqrt((x1 - v[1])^2 + (y1 - v[2])^2), 
                length = n)
    w0 <- w0 * diff(range(par('usr')[1:2])) * diff(range(par('usr')[3:4]))
    w1 <- w1 * diff(range(par('usr')[1:2])) * diff(range(par('usr')[3:4]))
    l <- length * diff(range(par('usr')[1:2])) * diff(range(par('usr')[3:4]))

    xx1 <- cos(theta) * (newa + seq(w0/2, w1/2, length = n)) + v[1]
    yy1 <- sin(theta) * (newa + seq(w0/2, w1/2, length = n)) + v[2]

    xx2 <- cos(theta) * (newa - seq(w0/2, w1/2, length = n)) + v[1]
    yy2 <- sin(theta) * (newa - seq(w0/2, w1/2, length = n)) + v[2]

    polygon(c(xx1, rev(xx2)), c(yy1, rev(yy2)), col = col, border = NA)
    .ahead(x0, y0, x1, y1, l, col)
}

.ahead <- function(x0, y0, x1, y1, l, col) {
    theta <- atan2(y0 - y1, x0 - x1)
    
    xx <- c(0, cos(theta - pi/8), cos(theta + pi/6)) * l + x1
    yy <- c(0, sin(theta - pi/8), sin(theta + pi/6)) * l + y1
    
    polygon(xx, yy, col = col, border = NA)
}

