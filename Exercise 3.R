##setwd("~/Desktop/Master 2 Semester/GLM Übung")
## Exercise 1
### Throwing a coin

## Likelihood for the coin result
## a)
Li.coin<-function(probH,h=60,t=40){
  n<-h+t # no of trials
  probT<-1-probH # probability of obtaining tails
  Li.coin<-choose(n,h)*probH^h*probT^t
  return(Li.coin)
}

## b)
Li.coin(0)
Li.coin(0.2)
Li.coin(0.5)
Li.coin(0.8)
Li.coin(1)


## probH=0.5 is the most likely out of these five proposed parameters.

## c) 
Lis<-rep(NA,100)
Lis
probs<-seq(0,1,by=0.01)
probs
for(i in 1:101){
  Lis[i]<-Li.coin(probs[i],60,40)
}

plot(probs,Lis,type="l",xlab="Probability of getting Heads",ylab="Likelihood", main="Parameter values and Likelihoods")

plot(probs,Lis,type="l",xlab="Probability of getting Heads",ylab="Likelihood", main="Parameter values and Likelihoods")


## d)
maxLi.coin <- optimize(f=Li.coin, h=60, t=40,
                       interval=c(0,1),maximum=TRUE)
maxLi.coin

## Excercise 2


dat <- read.csv("cyclones.csv", sep=";")
dat

#########
## (a) ##
#########
mean(dat$cyclones)
#########
## (b) ##
#########
L.i <- function(yi, lambda){
  res <- dpois(yi, lambda)
  return(res)
}

l.i <- function(yi, lambda){
  return(log(L.i(yi, lambda)))
}

loglikelihood <- function(lambda, y){
  n<-length(y)
  all.li<-rep(NA,n)
  for(i in 1:n){
    all.li[i]<-l.i(y[i],lambda)
  }
  # better:
  # all.li <- apply(as.matrix(y),MAR=1,FUN=l.i,lambda=lambda)
  loglik <- sum(all.li)
  return(loglik)
}


loglikelihood2 <- function(lambda, y){
  n<-length(y)
  all.li<-l.i(y,lambda)
  # better:
  # all.li <- apply(as.matrix(y),MAR=1,FUN=l.i,lambda=lambda)
  loglik <- sum(all.li)
  return(loglik)
}






##
lambda.grid <- seq(range(dat$cyclones)[1], range(dat$cyclones)[2],
                   length=100)
lambda.grid
loglik.on.grid <- apply(as.matrix(lambda.grid), MAR=1, FUN=loglikelihood,
                        y=dat$cyclones)

par(mfrow=c(1,2))
plot(lambda.grid, loglik.on.grid, type="l", lwd=2,ylim=c(-60,-28))

## numerical optimisation
numerical.solution <- optimize(f=loglikelihood, y=dat$cyclones,
                               interval=range(dat$cyclones),maximum=TRUE)
numerical.solution

abline(v=numerical.solution$maximum, col=2, lwd=2)
abline(h=numerical.solution$objective, col=2, lwd=2)

## using the negative of the log-likelihood
nloglikelihood<-function(lambda,y){
  nloglik<-loglikelihood(lambda,y)*-1
  return(nloglik)
}

nloglik.on.grid <- apply(as.matrix(lambda.grid), MAR=1, FUN=nloglikelihood,
                         y=dat$cyclones)

plot(lambda.grid, nloglik.on.grid, type="l", lwd=2,ylim=c(28,60))


numerical.solution2 <- optimize(f=nloglikelihood, y=dat$cyclones,
                                interval=range(dat$cyclones))
numerical.solution2
numerical.solution3 <- nlm(f=nloglikelihood,p=2,y=dat$cyclones)
numerical.solution3

abline(v=numerical.solution3$estimae, col=2, lwd=2)
abline(h=numerical.solution3$minimum, col=2, lwd=2)
