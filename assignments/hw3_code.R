## HW3 exploration
aq <- na.omit(datasets::airquality)



par(mfrow = c(2,3))
i <-1
for(m in unique(aq$Month)){
  
  month <- c('May','June','July','Aug','Sept')
  aq.subset <- subset(aq, Month == m)
  plot(aq.subset$Ozone ~ aq.subset$Temp, lwd = 2, 
       main = paste('Month of ',month[i]),
       xlab = 'Max daily temp', ylab = 'Mean O3 ppm')
  i<-i+1
}
plot(aq$Ozone~aq$Temp, main = paste('May - Sept'))


## a) Use simple regression model to predict mean ozone in ppb using max daily temperature, report its beta1
## b) compare standard error of $\beta_1$ from the model, vs methods of bootstrapping

## 95% CI using the standard error can be calculated as : #mean(beta) +/- 1.96*se(beta)#
z <-aq[,c('Temp','Ozone')]
itr       = 10000
theta     = NULL
thetas    = rep(0,itr)
set.seed(0)

model = lm(z[,2]~z[,1])
theta = as.numeric(model$coefficients[2])
for(i in 1:itr){
  z.new = z[sample(1:length(z[,1]),replace=T),]
  model = lm(z.new[,2]~z.new[,1])
  thetas[i] = model$coefficients[2]
}
bias = mean(thetas)-theta

## OUTPUT
print(paste('OBSERVED DATA ESTIMATE: ' ,theta))
print(paste('ESTIMATED BIAS: ' ,bias))
print(paste('BIAS-CORRECTED ESTIMATE: ' ,theta-bias  ))


### KDE:

par(mfrow= c(2,2))
hist(aq$Ozone, breaks = 30,freq = FALSE)
plot(density(aq$Ozone,kernel = 'gaussian',bw = 6.42))
plot(density(aq$Ozone,kernel = 'rectangular',bw = 6.42))
plot(density(aq$Ozone,kernel = 'triangular',bw = 6.42))



  
  



