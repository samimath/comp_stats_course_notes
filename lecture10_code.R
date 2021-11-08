############################################################################
# COMPUTATIONAL STATISTICS
# by Geof Givens and Jennifer Hoeting
# CHAPTER 11 EXAMPLES (last update 10/1/2012)

############################################################################
### EXAMPLE 11.1-2 EASY DATA
############################################################################
# easy = observed data
# x    = observed predictor data
# y    = observed response data
# tma  = truncated neighborhood moving average (for k odd)
############################################################################

## INITIAL VALUES
easy <- read.table('datasets/easysmooth.dat',header=T)
x    = easy$X
y    = easy$Y

## FUNCTIONS

## Smoothing function
TMA = function(k,y){
  # get number of observations
  n = length(y)
  # initiate smoothing matrix
  S = matrix(0,n,n)
  # index for points below x_i
  b = (k-1)/2
  if(k>1){
    # assign coefficient for smoothing matrix
    for(i in 1:b){
      S[i,1:(b+i)] = 1/(k-b+i-1)
      S[n-i+1,(n-b-i+1):n] = 1/(k-b+i-1)
    }
    for(i in (b+1):(n-b)){
      S[i,(i-b):(i+b)] = 1/k
    }}
  if(k==1){S = diag(1,n)}
  out = S%*%y
  return(out)
}



## MAIN
s3  = TMA(5,y)
s13 = TMA(13,y)
s43 = TMA(43,y)

## OUTPUT PLOTS
s = function(x){(x^3) * sin((x+3.4)/2)}
x.plot = seq(min(x),max(x),length.out=1000)
y.plot = s(x.plot)
plot(x,y,xlab="Predictor",ylab="Response", col = 'darkgrey')
lines(x.plot,y.plot,lwd = 2)
lines(x,s3, col = 'blue',lty = 2, lwd = 1.5)
lines(x,s13,col = 'forestgreen',lty =2,lwd = 1.5)
lines(x,s43,col = 'orange',lty = 2, lwd = 1.5)
legend("bottomright",c("True relation","k=4","k=13","k=43"),
       lty=c(1,2,2,2), col = c('black','blue','forestgreen','orange'))

k    = seq(3,51,by=2)
## FUNCTIONS
CVRSS = function(k,y){
  n = length(y)
  S = matrix(0,n,n)
  b = (k-1)/2
  if(k>1){
    for(i in 1:b){
      S[i,1:(b+i)] = 1/(k-b+i-1)
      S[n-i+1,(n-b-i+1):n] = 1/(k-b+i-1)
    }
    for(i in (b+1):(n-b)){
      S[i,(i-b):(i+b)] = 1/k
    }}
  if(k==1){S = diag(1,n)}
  s.hat = S%*%y
  out = sum(((y-s.hat)/(1-diag(S)))^2)
  return(out)
}

## MAIN
cvrss.val = rep(0,length(k))
for(i in 1:25){
  cvrss.val[i] = CVRSS(k[i],y)
}

## OUTPUT
cvrss.val      # CVRSS VALUES FOR k = 3,5,7,...,51

## OUTPUT PLOTS
plot(k,cvrss.val,type="b",lwd = 2, col = 'red')
points(k,cvrss.val,lwd = 2,type ='l', col = 'black')



## FUNCTIONS
# USES HAT MATRIX

## Running line smoother
RLSMOOTH1 = function(k,y,x){
  n = length(y)
  s.hat = rep(0,n)
  b = (k-1)/2
  if(k>1){
    for(i in 1:(b+1)){
      xi = x[1:(b+i)]
      xi = cbind(rep(1,length(xi)),xi)
      hi = xi%*%solve(t(xi)%*%xi)%*%t(xi)
      s.hat[i] = y[1:(b+i)]%*%hi[i,]
      
      xi = x[(n-b-i+1):n]
      xi = cbind(rep(1,length(xi)),xi)
      hi = xi%*%solve(t(xi)%*%xi)%*%t(xi)
      s.hat[n-i+1] = y[(n-b-i+1):n]%*%hi[nrow(hi)-i+1,]
    }
    for(i in (b+2):(n-b-1)){
      xi = x[(i-b):(i+b)]
      xi = cbind(rep(1,length(xi)),xi)
      hi = xi%*%solve(t(xi)%*%xi)%*%t(xi)
      s.hat[i] = y[(i-b):(i+b)]%*%hi[b+1,]
    }}
  if(k==1){s.hat = y}
  return(s.hat)
}



rlsmooth1.val = RLSMOOTH1(23,y,x)
## OUTPUT PLOTS
s = function(x){(x^3) * sin((x+3.4)/2)}
x.plot = seq(min(x),max(x),length.out=1000)
y.plot = s(x.plot)
plot(x,y,xlab="Predictor",ylab="Response", col = 'darkgrey', main = 'running-line smoother (k = 23)')
lines(x.plot,y.plot,lty=1, lwd = 2)
lines(x,rlsmooth1.val,type="l", col = 'blue', lwd =2)


### Kernel smoother
## FUNCTIONS
fx.hat = function(z,h){dnorm((z-x)/h)/h}
KSMOOTH = function(h,y,x){
  n = length(y)
  s.hat = rep(0,n)
  for(i in 1:n){
    a = fx.hat(x[i],h)
    s.hat[i] = sum(y * a/sum(a))
  }
  return(s.hat)
}
h    = 0.16
## MAIN
ksmooth.val = KSMOOTH(h,y,x)

## OUTPUT PLOTS
s = function(x){(x^3) * sin((x+3.4)/2)}
x.plot = seq(min(x),max(x),length.out=1000)
y.plot = s(x.plot)
plot(x,y,xlab="Predictor",ylab="Response",col = 'darkgrey', main = 'kernel smoother (h = 0.16)')
lines(x.plot,y.plot,lty=1, lwd = 2)
lines(x,ksmooth.val,type="l", col = 'blue', lwd =2)
