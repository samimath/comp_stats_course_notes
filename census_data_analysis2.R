library('rio')
url <- 'https://www2.census.gov/programs-surveys/mhs/tables/2014/PUF2019.xlsx'
mhs.data <- rio::import(url)
summary(mhs.data)

# let's work with only midwest data that is reported (not imputed)

mhs.midwest <- mhs.data[(mhs.data$REGION==2 & mhs.data$JPRICE == 'R' & mhs.data$JSQFT =='R'),] 

# visualize the distribution of price and sqft
par(mfrow= c(1,2))
hist(mhs.midwest$PRICE,col = 'grey')
hist(mhs.midwest$SQFT,col = 'green')


# build a linear model

midwest.lm <- lm(data = mhs.midwest, formula = PRICE~SQFT)
lm.coef <-coef(midwest.lm)


### Find model parameters using MLE approach


## design matrix:

### predictors:
x <- cbind(1,as.matrix(mhs.midwest$SQFT))
### observations;
y <- as.matrix(mhs.midwest$PRICE)

## colmmn number of the matrix:
K <-ncol(x)

K1 <- K+1

## row number of the matrix:

n<- nrow(x)


## define log likelihood function:

log.lik.lm <-function(theta,X,Y){
  # theta = 3 by 1 vector 
  # takes into input parameters theta, X, Y
  Y <- as.vector(y)
  
  X <- as.matrix(x)
  
  # define the Xb term
  xbeta <- X%*%theta[1:K]
  # define the sigma term
  Sig <- theta[K1:K1]
  
  # define the log likelihood function
  
  sum(-(1/2)*log(2*pi)-(1/2)*log(Sig^2)-(1/(2*Sig^2))*(y-xbeta)^2)
  
  
  
}


## evaluate the log likelihood function (X and Y has default values within the function):

log.lik.lm(theta = c(lm.coef[1],lm.coef[2], sigma(midwest.lm)))


## let's find the MLE for theta using optim 
## 'par' is the initial values
midwest.mle <- optim(par = c(-10000,50,1000), 
                     fn = log.lik.lm, 
                     method = 'L-BFGS-B',
                     control = list(trace=1,maxit=500,fnscale = -1))










