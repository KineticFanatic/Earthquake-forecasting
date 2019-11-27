data <- data_ASM
matplot(data_ASM, type = "l")
plot(data_ASM, type='p', par=log() ).log
y <- data_ASM[1]
x <- data_ASM[2]

abline(reg = lm(data ~ time(data$DATE)))

cycle(data)


x <- log(data_ASM$MAG)
y <- data_ASM$DATE
y <- data_ASM$`Intervent time`/365
x
hist(x, breaks = 50,probability = T ,main = "Histogram of MAG Variable")
lines(density(x), col="red", lwd=2)


nll <- function(theta0,theta1) {
  x <- data_ASM$`Intervent time`
  y <- data_ASM$MAG
  mu = exp(theta0 + x*theta1)
  -sum(y*(log(mu)) - mu)
}

est <- stats4::mle(minuslog=nll, start=list(theta0=2,theta1=0))
summary(est)
library(bbmle)
mle(minuslogl = nll, start = list(theta0 = 2, theta1 = 0))

pred.ts <- (exp(coef(est)['theta0'] +data_ASM$`Intervent time`*coef(est)['theta1'] ))
rmse(pred.ts, data_ASM$MAG)

glm(MAG ~ `Intervent time`, family = "poisson", data = data_ASM)


######################################################################

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

conservative scrutiny-based

time

library(fitdistrplus)

x <-DECLUST_DATA$MAG
y <- DECLUST_DATA$`Intervent time`/365
summary(y)

hist(y, breaks = 50,probability = T ,main = "Histogram of Intervent time Variable")
lines(density(y), col="red", lwd=2)

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

gfMLE<-gofstat(list(norMLE, weiMLE,expMLE,lnormMLE, gammaMLE))

gfMME <-gofstat(list(norMME,expMME, lnormMME, gammaMME))
gfMME

rln= rlnorm(42,-0.3482,1.144)
grid = seq(2010,2100,10)
plot(grid, ptlnorm(grid,-0.3482,1.144),type="l",xlab="time in years",ylab="f(x)")
lines(density(x),col="red")
library(EnvStats)
cdfPlot(distribution = "gamma", param.list = list( shape=  0.9851117 , rate  = 0.7758417), xlab = "Time Interval in years", ylab = "Cumulative Probability", xlim = c(0, 25) )
