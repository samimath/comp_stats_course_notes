## INITIAL VALUES
x = c(-2,-2)
itr = 40
x.values = matrix(0,itr+1,2)
x.values[1,] = x

## OBJECTIVE FUNCTION AND DERIVATIVES


## LOGISTIC POPULATION GROWTH MODEL FOR BEETLES. It is a function of t and x (theta)
N.func <-function(t,x){
  N0 <-2
  out<-(N0*x[1])/(N0+(x[1]-N0)*exp(-x[2]*t))
  return(out)
}
## FIRST DERIVATIVES
N.prime <-function(t,x){
  ## 1 = k, 2 = r
  N0 <-2
  base <-N0*(1-exp(x[2]*t)) + x[1]*exp(x[2]*t)
  out.1<-N0^2*(1-exp(-x[2]*t))/(base^2)
  out.2<-(t*N0*x[1]*(x[1]-N0)*exp(x[2]*t))/(base^2)
  out = matrix(c(out.1,out.2),ncol=1)
  return(out)
}

## SECOND DERIVATIVES:
N.prime2 <-function(t,x){
  N0 <-2
  base <-N0+exp(x[2]*t)*(x[1]-N0)
  out.kk<-2*(N0^2)*exp(-x[2]*t)*(1-exp(-x[2]*t))/(base^3)
  out.rr<-(N0*x[1]*exp(-2*x[2]*t)*t^2*(N0-N0*exp(x[2]*t))*(x[1]-N0))/(base^3)
  out.kr <-((N0^2*exp(-x[2]*t)*t)*(exp(-x[2]*t)*(N0-x[1]) + 2*x[1] -N0))/(base^3)
  out = matrix(c(out.kk,out.kr,out.kr,out.rr),ncol=1)
  return(out)
}


N.func <- Vectorize(N.func,'t')
N.prime <- Vectorize(N.prime,'t')
N.prime2 <- Vectorize(N.prime2,'t')

gamma <- function(x,t,y){
  
  out <- y - N.func(t,x)
  
  return(out)
}

## GAMMA.X is an n-by-1 vector measuring the difference between observed beetle count and the model. It is a function of x (theta) as the data is already embedded
gamma.x <- function(x){
  t = c(0,8,28,41,63,79,97,117,135,154)
  y=c(2,47,192,256,768,896,1120,896,1184,1024)
  out <- gamma(x,t,y)
  return(out)
}

N.prime.x <-function(x){
  t = c(0,8,28,41,63,79,97,117,135,154)
  out<-N.prime(t,x)
  return(out)
}

g <- function(x){
  out <- -1*gamma.x(x) %*% gamma.x(x)
  return(out)
}

g.prime <-function(x){

  g.prime.1 <-2*sum(gamma.x(x)*N.prime.x(x)[1,])
  g.prime.2 <- 2*sum(gamma.x(x)%*%N.prime.x(x)[2,])
  out <- matrix(c(g.prime.1,g.prime.2),ncol=1)
  return(out)
  
}
### test:


t = c(0,8,28,41,63,79,97,117,135,154)
y=c(2,47,192,256,768,896,1120,896,1184,1024)
k0 = 1033.5153256
r0 = 0.117
x0 <-c(k0,r0)
#g.i(x=c(k0,r0),t,y)
print(paste('initial test: g(\theta) = ', g(x0)))

g.prime = function(x){
  g.prime.da = (-1)*((4*x[1]^3)+(4*x[1]*x[2])-(42*x[1])+(2*x[2]^2)-14)
  g.prime.db = (-1)*((2*x[1]^2)-(26*x[2])-22+(4*x[1]*x[2])+(4*x[2]^3))
  out = matrix(c(g.prime.da,g.prime.db),ncol=1)
  return(out)
}
g.2prime=function(x){
  g.2prime.da2 = (-1)*((12*x[1]^2)+(4*x[2])-42)
  g.2prime.db2 = (-1)*((12*x[2]^2)+(4*x[1])-26)
  g.2prime.dadb = (-1)*(4*(x[1]+x[2]))
  out = matrix(c(g.2prime.da2,g.2prime.dadb,
                 g.2prime.dadb,g.2prime.db2),nrow=2, byrow=TRUE)
  return(out)
}

## MAIN
for(i in 1:itr){
  x = x - solve(g.2prime(x))%*%g.prime(x)
  x.values[i+1,] = x
}

## OUTPUT
x		# FINAL ESTIMATE
g(x) 		# OBJECTIVE FUNCTION AT ESTIMATE
g.prime(x) 	# GRADIENT AT ESTIMATE

## PLOT OF CONVERGENCE
z = matrix(0,100,100)
x1.max = max(4.5,ceiling(max(x.values[,1])))
x1.min = min(-2,floor(min(x.values[,1])))
x2.max = max(3,ceiling(max(x.values[,2])))
x2.min = min(-2,floor(min(x.values[,2])))
x1 = seq(x1.min,x1.max,length=100)
x2 = seq(x2.min,x2.max,length=100)
for(i in 1:100){
  for(j in 1:100){
    z[i,j] = g(c(x1[i],x2[j]))
  }
}
contour(x1,x2,z,nlevels=20,drawlabels=FALSE)
for(i in 1:itr){
  segments(x.values[i,1],x.values[i,2],x.values[i+1,1],
           x.values[i+1,2],lty=2)
}
