### A sample exercise to compare how to generate confidence interval using parametric vs non-parametric methods.


generate_data <- function(x, p, coef){
  
  y <- 0
  for(i in 0:length(coef)){
    
    y <- y + coef[i]*x^(i)
    
  }
  return(y)
  
}


generate_coef_data_non_parametric <- function(sample.x, sample.y){
  output <- list()
  ## generate resample index
  index <- sample(1:length(sample.x),replace = T)
  df <- data.frame(x = sample.x, y= sample.y)
  ## resample from distribution:
  df.sample <- df[sort(index),]
  sample.model <- lm(y~poly(x,p), data = df.sample)
  sample.coef <- as.vector(sample.model$coefficients)
  y.pred <- sample.model$fitted.values
  print(sample.y)
  print(y.pred)
  
  plot(df.sample$x,df.sample$y)
  lines(df.sample$x, y.pred, col = 'red', lwd = 3)
  return(coef)
}


generate_coef_data_parametric <- function(sample.x, sample.y){
  
  coef <- lm.fit
}
generate_coef_data_non_parametric(x,y)

test <- data.frame(x = x , y=y)

index <- sample(1:length(x), replace = T)

sample.df <- test[sort(index),]

sample.model <- lm(y~poly(x,2), data = sample.df)

plot(sample.df$x, sample.df$y)
lines(x, sample.model$fitted.values, col = 'red')

## generate number of points:

n <- 50

n.fit <- 100

bootstrap.samples <- 500

p <-3

x.plot <- seq(0,10,by = 10/n.fit)

x <- seq(0,10,by = 10/n)

# Define the percentiles for the confidence intervals
lower.data <- as.integer(bootstrap.samples*0.025)
upper.data <- as.integer(bootstrap.samples*0.925)

# Generate 0 centered noirse from Beta distribution

noise <- rnorm(n = length(x), mean = 0, sd = 1)
noise <- noise - mean(noise)
hist(noise)

## Define the data:

y <- sin(x)+0.001*exp(x) + noise

plot(x,y)

### 1. polynomial fit
poly.fit <- lm(y ~ poly(x,3))
### 2. kernel fit

kernel.fit <- ksmooth(x,y,kernel = 'normal')

## plot the results:
plot(x,y,col='deepskyblue3',xlab='x',main='Observed data',lwd = 2)
lines(x,poly.fit$fitted.values,col='firebrick1',lwd=3)
lines(kernel.fit$x,kernel.fit$y,col='darkorchid4',lwd=3)

legend("topleft",c("Observed data","Polynomial fit", "Kernel fit"), 
       col=c("deepskyblue3","firebrick1",'darkorchid4'), lwd=3)

## Confidence interval for model parameters
confint(poly.fit, level=0.95)

