library('latex2exp')
## code to accompany lecture 2 examples

##. 1. plot the non-linear function log(x)/(1+x)

g <- function(x){log(x)/(1+x)}
g.prime <- function(x){(1+(1/x)-log(x))/((1+x)^2)}
cauchy <- function(theta,x){1/(pi*(1-(x-theta)^2))}
x <- seq(from = 1, to = 5, by = 0.001)
y<-g(x)
y.prime <-g.prime(x)
## numerically find the max
max.ind <- match(max(y),y)

xo <- x[max.ind]
plot(x,y,type = 'l', 
     col = 'blue', lwd =2, 
     ylab = TeX('$g(x)$'), 
     xlab = TeX('$x$'))
## visualizing the maximum value of g
abline(v = xo, col = 'grey',lty = 3,lwd = 2)
points(xo,g(xo),type = 'o',col = 'red',lwd = 3)
text(xo,max(y)-0.05,labels = paste0('max(g(x)) is \napproximately at ', xo))



plot(x,g.prime(x),type = 'l', 
     col = 'forestgreen', lwd =2, 
     ylab = TeX('$g\'(x)$'), 
     xlab = TeX('$x$'))
## visualizing the maximum value of g
abline(h = 0, col = 'grey',lty = 3,lwd = 2)


