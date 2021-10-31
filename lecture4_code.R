## EM algorithm example 

## available data
y1 <- 5
## initial value:
p0 <- 2
## number of iteration 
N <- 15

p <-p0
## create empty vector to store value from each iteration
p.vec <-c()
for(i in 1:N){
  p <-2*p/(1+y1*p)
  p.vec[i] <-p
}

plot(1:N,p.vec,type = 'b',lwd = 2)

## check fit:



### FUNCTION

g <-function(x,m,s){
  out <- 1/(2*pi*s)* exp(-x^2/(2*s))
}

g <-function(x,m,s){
  out <- log(1+x)
}

g = Vectorize(g,vectorize.args = "x")

x<-seq(from = 0,to=10,by=.01)

plot(x,g(x),type = 'l')

### Trapezoidal rule:
trapezoidal = function(interval,n,m,s){
  ## define size of each increment
  h   = (interval[2]-interval[1])/n
  
  ## define each interior point x
  x   = interval[1] + (1:(n-1))*h
  ## define the first term
  out = h/2*g(interval[1],m,s)
  ## define the middle terms
  out = c(out, h*g(x,m,s))
  ## define the last term
  out = c(out, h/2*g(interval[2],m,s))
  return(out)
}

interval <-c(0,10)
n<-10
#m <-0
#s <-1

# MAIN
T1 = trapezoidal(interval,n,m,s)

# OUTPUT
sum(T1)      # TRAPEZOIDAL APPROXIMATION


## Monte Carlo approximation example:
runs <- 5000
#runif samples from a uniform distribution
xs <- runif(runs,min=-0.5,max=0.5)
ys <- runif(runs,min=-0.5,max=0.5)
## points that are in a circle with radius 0.5
in.circle <- xs^2 + ys^2 <= 0.5^2
mc.pi <- (sum(in.circle)/runs)*4
plot(xs,ys,col=ifelse(in.circle,"forestgreen","grey"),pch=ifelse(in.circle,"o","x")
     ,xlab='',ylab='',asp=1,
     main=paste("MC Approximation of Pi =",mc.pi,"\n N=", runs),lwd = 3)

#

