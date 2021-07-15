#experiment varying priors
prior.rate <- 0.1
prior.sd <- 10

trueA <- 0
trueB <- 1

n <- 10^4

Y <- rnorm( n, 0, 1 )

prior <- function(param){
  a=param[1]
  b=param[2]
  Aprior <- dnorm(a,mean = 0,sd = prior.sd, log = T)
  Bprior <- dgamma(b,1,rate=prior.rate,log = T)
  return(Aprior+Bprior)
}
likelihood <- function(param){
  a=param[1]
  b=param[2]
  singlelikelihoods = dnorm(Y,mean=a,sd=sqrt(1/b),log=T)
  sumall=sum(singlelikelihoods)
  return(sumall)
}

posterior <- function(param){
  return(prior(param)+likelihood(param))
}

m <- 30000

sima <- simb <- numeric(m)
logposterior <- numeric(m)

sima[1] = rnorm(1,mean=0,sd= prior.sd)
simb[1] = rgamma(1,shape=1,rate=prior.rate)
logposterior[1] <- posterior( c(sima[1],simb[1]) )


#Gibbs
for(i in 2:m)
{
  sh <- .5 * n + 1
  rt <- sum( ( (Y - sima[i-1])^2 ) / 2 ) + prior.rate 
  
  simb[i] = rgamma(1, shape=sh, rate = rt )
  
  sima[i] <- rnorm( 1, mean=simb[i]*sum(Y)/(n*simb[i]+ 1/(prior.sd)^2 ), sd=1/sqrt(n*simb[i] + 1/(prior.sd)^2))
  
  logposterior[i] <- posterior( c(sima[i],simb[i]) )

}

par(mfrow = c(1,2))
hist(sima, nclass=30, main="Posterior of a", xlab="True value = red line" )
abline(v = mean(sima), col="blue")
abline(v = trueA, col="red" )
hist(simb, nclass=30, main="Posterior of b", xlab="True value = red line" )
abline(v = mean(simb), col="blue")
abline(v = trueB, col="red" )
