# Created by Fabio M. Bayer (bayer@ufsm.br)
# July 2018
#
# It is an example of the KARMA model (BAYER, BAYER AND PUMI, 2017) application. 
# 
# In this application we consider the Relative Humidity data in Santa Maria, Rs, Brazil. 
# It is the same data used in Bayer, Cintra and Cribari-Neto (2018). 
#
# References:
#
# BAYER, F.M.; BAYER, D.M. ; PUMI, G. 
# Kumaraswamy autoregressive moving average models for double bounded environmental data. 
# Journal of Hydrology
# 2017
# DOI: 10.1016/j.jhydrol.2017.10.006
#
# BAYER, F.M., CINTRA, R.J., CRIBARI-NETO, F. 
# Beta seasonal autoregressive moving average models, 
# Journal of Statistical Computation and Simulation
# 2018
# DOI: 10.1080/00949655.2018.1491974



# read the data
ur<-scan(file="ur-sm-mensal.txt")
y<-ur/100 # percentage
Y<-ts(y,start=c(2003,1),frequency=12) # time series

# sample size
n<-length(Y)

# Descriptive analysis
summary(Y,na.rm=T) 
var(Y)

# number of forecast steps
h1 <- 10

# Taking off the last 12 observations
n<-n-h1

y<-ts(Y[1:n], start=c(2003,1), frequency = 12)

# some graphics
plot(y)
monthplot(y)
acf(y)
pacf(y)

## covariates 

# # tendency
t <- 1:n # in sample
t_hat <- (n+1):(n+h1) # out of sample

# deterministic seazonality
C<-cos(2*pi*t/12) # in sample 
C_hat<-cos(2*pi*t_hat/12) # out of sample

# S<-sin(2*pi*t/12) # in sample
# S_hat<-sin(2*pi*t_hat/12) # out of sample

# More than on covariate
# mX<-cbind(S,C) # in sample
# mX_hat<-cbind(S_hat,C_hat) # out of sample

# read the karma function
source("karma.r")

# Example 1: fit the model KARMA(1,1) with covariates
fit1 <- karma(y,ar=c(1),ma=c(1),h=h1,diag=1,X = C,X_hat=C_hat)
residq<-fit1$resid3
Box.test(residq, lag = 20, type =  "Ljung-Box", fitdf = 0)

# Example 2: fit the model KARMA(2,2) without covariates
fit2 <- karma(y,ar=c(1,2),ma=c(1,2),h=h1,diag=1)
residq<-fit2$resid3
Box.test(residq, lag = 20, type =  "Ljung-Box", fitdf = 0)
