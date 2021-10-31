### read in some data from US census
library('rio')

## doc https://www.census.gov/content/dam/Census/programs-surveys/mhs/technical-documentation/puf/MHS_PUF_Documentation2019.docx
## census regions:
## 1. = Northeast, 2.  = Midwest,  3.  South, 4. West, 5. National

url <- 'https://www2.census.gov/programs-surveys/mhs/tables/2014/PUF2019.xlsx'
mhs.data<-rio::import(url)
## let's look at midwest, and select records where the price and sqft values are reported rather than imputed
mhs.midwest <- mhs.data[(mhs.data$REGION==2 & mhs.data$JPRICE =='R'& mhs.data$JSQFT =='R'),]


## some data exploration:

par(mfrow = c(1,3))
hist(mhs.midwest$PRICE,breaks=30,border = 'grey',col='gold',main = 'Distribution of Price')
hist(mhs.midwest$SQFT,breaks=30,border = 'grey',col='blue', main = 'Distribution of SQFT')
plot(mhs.midwest$SQFT,mhs.midwest$PRICE,col='forestgreen', main = 'SQFT vs Price')



## first fit a linear model, predict median house price by median sqft 

midwest.lm <-lm(data = mhs.midwest, formula = PRICE~SQFT)

lm.coef<-midwest.lm$coefficients

lm.sig <- sigma(midwest.lm)

midwest.fiitted <- lm.coef[1] + lm.coef[2]*mhs.midwest$SQFT


## plot the result:

plot(x=midwest.fiitted, y=mhs.midwest$PRICE,
     xlab="Model-predicted median house price in Midwest",
     ylab="Actual median house values in Midwest")
abline(a=0,b=1,col="grey")

## Now try solving this with MLE from scratch

# First, put the data into matrices for the MLE procedure

x <- cbind(1,as.matrix(mhs.midwest$SQFT))

y <- as.matrix(mhs.midwest$PRICE)

ones <- x[,1]
X <-as.matrix(x)






# Calculate K and n for later use

K <- ncol(x)

K

K1 <- K + 1

n <- nrow(x)

n



# Define the function to be optimized



log.lik.lm <- function(theta,X,Y) {
  #preset values for X, Y based on x and y defined from the data
  Y <- as.vector(y)
  
  X <- as.matrix(x)
  
  xbeta <- X%*%theta[1:K]
  
  Sig <- theta[K1:K1]
  
  sum(-(1/2)*log(2*pi)-(1/2)*log(Sig^2)-(1/(2*Sig^2))*(y-xbeta)^2)
  
}




## we can do some test:

## evaluate log likelihood value using the coefficients from the lm package
log.lik.lm(theta = c(lm.coef[1],lm.coef[2],sigma(midwest.lm)))

## or we can also take a look at how the likelihood changes if we vary one of the parameter (say beta_1 in this case)

b0_list <- -20000:-1000
b1_list <-30:100
l_test <-lapply(b1_list,
                function(b){log.lik.lm(theta=c(lm.coef[1],b,sigma(midwest.lm)))})


l_test2 <-lapply(b0_list, function(b){log.lik.lm(theta=c(b,lm.coef[2],sigma(midwest.lm)))})

par(mfrow=c(1,2))

plot(b0_list,l_test2, type='l',col = 'purple',main = 'll as a function b0')
plot(b1_list,l_test, type='l',col ='orange',main = 'll as a function b1')


### This can also help us identify a good initial value to feed in the optimization 


midwest.mle <- optim(par=c(-9000,60,20000),fn=log.lik.lm, 
               method = "BFGS", 
               control = list(trace=1,maxit=5000,fnscale = -1),
               hessian = FALSE)



plot(x=mhs.midwest$SQFT, y=mhs.midwest$PRICE,
     xlab="SQFT",
     ylab="PRICE",
     col='darkgrey',
     main = 'House values in midwest as a function of square footage')
abline(a = midwest.lm$coefficients[1], b = midwest.lm$coefficients[2],col='forestgreen',lwd = 2)
abline(a = midwest.mle$par[1], b = midwest.mle$par[2],col='red', lwd = 2)
legend(1800, 100000, legend = c('data','LS estimate', 'MLE'),
       col=c('darkgrey','forestgreen','red'), lty = c(1,1,1),cex=0.8,
       title="Line types", text.font=4,bty = 'n',bg = 'transparent')



### Howcome the estimates of beta from MLE and the lm package are different?

beta = solve(t(X) %*% X) %*% (t(X) %*% y)

