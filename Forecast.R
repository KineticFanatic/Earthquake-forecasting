#importing data
data <- data_ASM
x <- log(data_ASM$MAG)
y <- data_ASM$`Intervent time`/365

#Histogram Plot
matplot(data_ASM, type = "l")
abline(reg = lm(data ~ time(data$DATE)))
cycle(data)
hist(x, breaks = 50,probability = T ,main = "Histogram of MAG Variable")
lines(density(x), col="red", lwd=2)

#Declustering of Data
spaceM <- exp(-1.024 + 0.804*6) + 15
spaceN <- exp(-1.024 + 0.804*x) - 15

R = exp(K)
f = max(R)
f = f+15
e = min(R) - 15
min(R)
timeM <- exp(-2.870 + 1.235*6) + 60
timeN <- exp(-2.870 + 1.235*x) - 60
g = max(t) + 60
h = min(t) - 60

#Fitting Distribution using MLE & MME taking Intervent time as i.i.d.

library(fitdistrplus)

norMLE<-fitdist(y[-1], "norm", method= "mle")
norMLE
summary(norMLE)
plot(norMLE)

weiMLE<-fitdist(y[-1], "weibull", method= "mle", lower = c(0, 0), start = list(scale = 1, shape = 1))
weiMLE
plot(weiMLE)
summary(weiMLE)

expMLE<-fitdist(y[-1], "exp", method= "mle")
expMLE
plot(expMLE)
summary(expMLE)

expMLE<-fitdist(y, "exp", method= "mle")
expMLE
plot(expMLE)
summary(expMLE)

gammaMLE<-fitdist(y[-1], "gamma")
gammaMLE
plot(binomialMLE)
summary(gammaMLE)

lnormMLE<-fitdist(y[-1], "lnorm")
summary(lnormMLE)

norMME<-fitdist(y[-1], "norm", method= "mme")
norMME
summary(norMME)
plot(norMME)
weiMME<-fitdist()
weiMME<-fitdist(y[-1], "weibull", method= "mme", order= 1) 
weiMME
plot(weiMME)
summary(weiMME)

expMME<-fitdist(y[-1], "exp", method= "mme")
expMME
plot(expMME)
summary(expMME)

gammaMME<-fitdist(y[-1], "gamma" , method= "mme")
gammaMLE
plot(binomialMLE)
summary(gammaMME)

lnormMME<-fitdist(y[-1], "lnorm", method= "mme")
summary(lnormMME)

#Goodness of Fit
gfMLE<-gofstat(list(norMLE, weiMLE,expMLE,lnormMLE, gammaMLE))
gfMME <-gofstat(list(norMME,expMME, lnormMME, gammaMME))

#CDF plot of K-S test
library(EnvStats)
cdfPlot(distribution = "gamma", param.list = list( shape=  0.9851117 , rate  = 0.7758417), xlab = "Time Interval in years", ylab = "Cumulative Probability", xlim = c(0, 25) )
