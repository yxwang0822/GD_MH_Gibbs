X <- read.csv("guangdong.csv", sep = ",", header = T, encoding = "UTF-8")

rows.with.NA.1 <- which( is.na( X[ , 1] ) )
rows.with.NA.2 <- which( is.na( X[ , 2] ) )
rows.with.NA.3 <- which( is.na( X[ , 3] ) )
rows.with.NA.4 <- which( is.na( X[ , 4] ) )
rows.with.NA.5 <- which( is.na( X[ , 5] ) )
rows.with.NA.6 <- which( is.na( X[ , 6] ) )
rows.with.NA.7 <- which( is.na( X[ , 7] ) )
rows.with.NA.8 <- which( is.na( X[ , 8] ) )
rows.with.NA.9 <- which( is.na( X[ , 9] ) )
rows.with.NA.10 <- which( is.na( X[ , 10] ) )
rows.with.NA.11 <- which( is.na( X[ , 11] ) )
rows.with.NA.12 <- which( is.na( X[ , 12] ) )
rows.with.NA.13 <- which( is.na( X[ , 13] ) )
rows.with.NA.14 <- which( is.na( X[ , 14] ) )
rows.with.NA.15 <- which( is.na( X[ , 15] ) )
rows.with.NA.16 <- which( is.na( X[ , 16] ) )
rows.with.NA.17 <- which( is.na( X[ , 17] ) )

Y <- X[ -rows.with.NA.4, ]

P <- Y[ , 11]
CK <- Y[ , 6]
FK <- Y[ , 7]
K <- Y[ , 8]
L <- Y[ , 14]
I <- Y[ , 17]

rows.KLPzero <- which((CK<=0)|(FK==0)|(L==0)|(P==0)|(I==0))
Y <- Y[-rows.KLPzero, ]

P <- Y[ , 11]
CK <- Y[ , 6]
FK <- Y[ , 7]
K <- Y[ , 8]
L <- Y[ , 14]
I <- Y[ , 17]

log.P <- log(P)
log.K <- log(K)
log.CK <- log(CK)
log.FK <- log(FK)
log.L <- log(L)
log.I <- log(I)

OLS <- lm(log.P ~ log.CK + log.FK)
summary(OLS)

alpha <- unname(coefficients(OLS))
TFP <- log.P - alpha[2]*log.CK - alpha[3]*log.FK
#TFP <- exp(log.A)
Y <- cbind(Y, TFP)

attach(Y)
TFP.c <- (TFP > quantile(TFP, 0.05)) & (TFP < quantile(TFP, 0.95))
detach()
Y <- Y[TFP.c, ]
TFP <- Y[ , 18]

P <- Y[ , 11]
CK <- Y[ , 6]
FK <- Y[ , 7]
K <- Y[ , 8]
L <- Y[ , 14]

P <- Y[ , 11]
CK <- Y[ , 6]
FK <- Y[ , 7]
K <- Y[ , 8]
L <- Y[ , 14]
I <- Y[ , 17]

log.P <- log(P)
log.K <- log(K)
log.CK <- log(CK)
log.FK <- log(FK)
log.L <- log(L)
log.I <- log(I)

Scale <- Y[ , 4]

Scale.L <- function(m){
  Scl.L <- numeric(m)
  for (i in 1 : m){
    if (Scale[i] == 1)
    {
      Scl.L[i] = 1
    }
    else
    {
      Scl.L[i] = 0
    }
  }
  return(Scl.L)
}

Scale.M <- function(m){
  Scl.M <- numeric(m)
  for (i in 1 : m){
    if (Scale[i] == 2)
    {
      Scl.M[i] = 1
    }
    else
    {
      Scl.M[i] = 0
    }
  }
  return(Scl.M)
}

Scale.S <- function(m){
  Scl.S <- numeric(m)
  for (i in 1 : m){
    if ((Scale[i] == 3) | (Scale[i] == 4))
    {
      Scl.S[i] = 1
    }
    else
    {
      Scl.S[i] = 0
    }
  }
  return(Scl.S)
}

Scl.L <- Scale.L(length(Scale))
Scl.M <- Scale.M(length(Scale))
Scl.S <- Scale.S(length(Scale))

Ownership <- Y[ , 1]

State <- function(m){
  State <- numeric(m)
  for (i in 1 : m){
    if ((Ownership[i] == 100) | (Ownership[i] == 110))
    {
      State[i] = 1
    }
    else
    {
      State[i] = 0
    }
  }
  return(State)
}

Collect <- function(m){
  Collect <- numeric(m)
  for (i in 1 : m){
    if ((Ownership[i] == 120) | (Ownership[i] == 130))
    {
      Collect[i] = 1
    }
    else
    {
      Collect[i] = 0
    }
  }
  return(Collect)
}

Private <- function(m){
  Private <- numeric(m)
  for (i in 1 : m){
    if ((170 <= Ownership[i]) & (Ownership[i] <= 174))
    {
      Private[i] = 1
    }
    else
    {
      Private[i] = 0
    }
  }
  return(Private)
}

Partner <- function(m){
  Partner <- numeric(m)
  for (i in 1 : m){
    if ((140 <= Ownership[i]) & (Ownership[i] <= 149))
    {
      Partner[i] = 1
    }
    else
    {
      Partner[i] = 0
    }
  }
  return(Partner)
}

Foreign <- function(m){
  Foreign <- numeric(m)
  for (i in 1 : m){
    if ((200 <= Ownership[i]) & (Ownership[i] <= 340))
    {
      Foreign[i] = 1
    }
    else
    {
      Foreign[i] = 0
    }
  }
  return(Foreign)
}

State <- State(length(Ownership))
Collect <- Collect(length(Ownership))
Private <- Private(length(Ownership))
Partner <- Partner(length(Ownership))
Foreign <- Foreign(length(Ownership))

Y <- cbind(Y, Scl.L, Scl.M, Scl.S, State, Collect, Private, Partner, Foreign)

ExpBinom <- Y[ , 16]

Age <- Y[ , 15]
Agesqrd <- Age^2
Liability <- Y[ , 9]
s <- Liability/K
Y <- cbind(Y, Agesqrd, s)

Probit <- glm(ExpBinom ~ scale(TFP) + scale(log.CK) + scale(log.FK) + scale(log.I) + scale(s) + scale(Age) + Scl.L + Scl.M + State + Collect + Private + Partner + Foreign, family=binomial(link="probit"))
summary(Probit)

beta <- unname(coefficients(Probit))
prior.mean <- 0
prior.sd <- 10   
trueB0 <- beta[1]
trueB1 <- beta[2]
trueB2 <- beta[3]
trueB3 <- beta[4]
trueB4 <- beta[5]
trueB5 <- beta[6]
trueB6 <- beta[7]
trueB7 <- beta[8]
#trueB8 <- beta[9]
trueB8 <- beta[9]
trueB9 <- beta[10]
trueB10 <- beta[11]
trueB11 <- beta[12]
trueB12 <- beta[13]

prior <- function(param){
  b0=param[1]
  b1=param[2]
  b2=param[3]
  b3=param[4]
  b4=param[5]
  b5=param[6]
  b6=param[7]
  b7=param[8]
  b8=param[9]
  b9=param[10]
  b10=param[11]
  b11=param[12]
  B0prior <- dnorm(b0, mean = prior.mean, sd = prior.sd, log = T)
  B1prior <- dnorm(b1, mean = prior.mean, sd = prior.sd, log = T)
  B2prior <- dnorm(b2, mean = prior.mean, sd = prior.sd, log = T)
  B3prior <- dnorm(b3, mean = prior.mean, sd = prior.sd, log = T)
  B4prior <- dnorm(b4, mean = prior.mean, sd = prior.sd, log = T)
  B5prior <- dnorm(b5, mean = prior.mean, sd = prior.sd, log = T)
  B6prior <- dnorm(b6, mean = prior.mean, sd = prior.sd, log = T)
  B7prior <- dnorm(b7, mean = prior.mean, sd = prior.sd, log = T)
  B8prior <- dnorm(b8, mean = prior.mean, sd = prior.sd, log = T)
  B9prior <- dnorm(b9, mean = prior.mean, sd = prior.sd, log = T)
  B10prior <- dnorm(b10, mean = prior.mean, sd = prior.sd, log = T)
  B11prior <- dnorm(b11, mean = prior.mean, sd = prior.sd, log = T)
  return(B0prior + B1prior + B2prior + B3prior + B4prior + B5prior + B6prior + B7prior + B8prior + B9prior + B10prior + B11prior)
}

X.S <- cbind(rep(1, length(TFP)), scale(TFP), scale(log.CK), scale(log.FK), scale(log.I), scale(s), scale(Age), Scl.L, Scl.M, Scl.S, State, Foreign )

likelihood <- function(param){
  b0=param[1]
  b1=param[2]
  b2=param[3]
  b3=param[4]
  b4=param[5]
  b5=param[6]
  b6=param[7]
  b7=param[8]
  b8=param[9]
  b9=param[10]
  b10=param[11]
  b11=param[12]
  eta.l <- X.S %*% c(b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11)
  P.l <- pnorm(eta.l)
  singlelikelihoods = dbinom(ExpBinom, 1, P.l, log = T)
  sumall=sum(singlelikelihoods)
  return(sumall)
}

posterior <- function(param){
  return(prior(param)+likelihood(param))
}

proposal.sd <- 0.01

proposalfunction <- function(param){
  return(rmvnorm(1, mean = param, sigma = diag(proposal.sd^2,12,12)))
}

q <- function(m, n){
  return( dmvnorm( m, mean = n, sigma = diag(proposal.sd^2,12,12), log = T) )
}

run_metropolis_MCMC <- function(initialvalue, iterations){
  chain = array(dim = c(iterations+1, 12))
  chain[1,] = initialvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i, ])
    alpha.MH = exp( posterior(proposal) - posterior(chain[i, ]) + q(proposal, chain[i, ])- q(chain[i, ], proposal) )
    if (runif(1) < min(1, alpha.MH)) 
    {
      chain[i+1,] = proposal
    }
    else
    {
      chain[i+1,] = chain[i,]
    }
  }  
  return(chain)
}

initialvalue <- rep(0.01, 12)
iterations <- 10^4
MH = run_metropolis_MCMC(initialvalue, iterations)

#BurnIn <- 10000
#MH <- MH[-(1:BurnIn), ]

par(mfrow = c(4,4))
hist(MH[ , 1],nclass=30, main="Posterior of B0", xlab="True value = red line" )
abline(v = mean(MH[ , 1]), col="blue")
#abline(v = trueB0, col="red" )
hist(MH[ , 2],nclass=30, main="Posterior of B1", xlab="True value = red line" )
abline(v = mean(MH[ , 2]), col="blue")
#abline(v = trueB1, col="red" )
hist(MH[ , 3],nclass=30, main="Posterior of B2", xlab="True value = red line" )
abline(v = mean(MH[ , 3]), col="blue")
#abline(v = trueB2, col="red" )
hist(MH[ , 4],nclass=30, main="Posterior of B3", xlab="True value = red line" )
abline(v = mean(MH[ , 4]), col="blue")
#abline(v = trueB3, col="red" )
hist(MH[ , 5],nclass=30, main="Posterior of B4", xlab="True value = red line" )
abline(v = mean(MH[ , 5]), col="blue")
#abline(v = trueB4, col="red" )
hist(MH[ , 6],nclass=30, main="Posterior of B5", xlab="True value = red line" )
abline(v = mean(MH[ , 6]), col="blue")
#abline(v = trueB5, col="red" )
hist(MH[ , 7],nclass=30, main="Posterior of B6", xlab="True value = red line" )
abline(v = mean(MH[ , 7]), col="blue")
#abline(v = trueB6, col="red" )
hist(MH[ , 8],nclass=30, main="Posterior of B7", xlab="True value = red line" )
abline(v = mean(MH[ , 8]), col="blue")
#abline(v = trueB7, col="red" )
hist(MH[ , 9],nclass=30, main="Posterior of B8", xlab="True value = red line" )
abline(v = mean(MH[ , 9]), col="blue")
#abline(v = trueB8, col="red" )
hist(MH[ , 10],nclass=30, main="Posterior of B9", xlab="True value = red line" )
abline(v = mean(MH[ , 10]), col="blue")
#abline(v = trueB9, col="red" )
hist(MH[ , 11],nclass=30, main="Posterior of B10", xlab="True value = red line" )
abline(v = mean(MH[ , 11]), col="blue")
#abline(v = trueB10, col="red" )
hist(MH[ , 12],nclass=30, main="Posterior of B11", xlab="True value = red line" )
abline(v = mean(MH[ , 12]), col="blue")
#abline(v = trueB11, col="red" )


acceptance = 1-mean(duplicated(MH))

trunc <- function(param){
  b0=param[1]
  b1=param[2]
  b2=param[3]
  b3=param[4]
  b4=param[5]
  b5=param[6]
  b6=param[7]
  b7=param[8]
  b8=param[9]
  b9=param[10]
  b10=param[11]
  b11=param[12]
  etastar <- X.S %*% c(b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11)
  simz <- numeric( length(TFP) )
  for (i in 1 : length(TFP)){
    if (ExpBinom[i] == 1)
    {
      simz[i] = rtrunc(1, spec = "norm", a = 0, b = Inf, mean = etastar[i], sd = 1 )
    }
    else
    {
      simz[i] = rtrunc(1, spec = "norm", a = -Inf, b = 0, mean = etastar[i], sd = 1 )
    }
  }
  return(simz)
}

simb <- matrix(0, iterations, 12)
simz <- matrix(0, iterations, length(TFP))
simb[1, ]=rep(0.1, 12)
b <- diag(prior.mean,12,12)
v <- diag(prior.sd,12,12)

#gibbs
for(i in 2:iterations)
{
  simz[i,] = trunc( simb[i-1, ] )
  
  V = solve(solve(v) + t(X.S) %*% X.S)
  
  B <- matrix(0, iterations, 12)
  
  B[i, ] = V %*% ( t(X.S) %*% simz[i,] )
  
  simb[i, ] = rmvnorm(1, mean=B[i, ], sigma=V)
  
 # cat("\n done to iteration ", i)
}

#chol -- cholesky decomp 
#chol2inv -- convert cholesky decomp to inverse

par(mfrow = c(4,4))
hist(simb[ , 1],nclass=30, main="Posterior of B0", xlab="True value = red line" )
abline(v = mean(simb[ , 1]), col="blue")
#abline(v = trueB0, col="red" )
hist(simb[ , 2],nclass=30, main="Posterior of B1", xlab="True value = red line" )
abline(v = mean(simb[ , 2]), col="blue")
#abline(v = trueB1, col="red" )
hist(simb[ , 3],nclass=30, main="Posterior of B2", xlab="True value = red line" )
abline(v = mean(simb[ , 3]), col="blue")
#abline(v = trueB2, col="red" )
hist(simb[ , 4],nclass=30, main="Posterior of B3", xlab="True value = red line" )
abline(v = mean(simb[ , 4]), col="blue")
#abline(v = trueB3, col="red" )
hist(simb[ , 5],nclass=30, main="Posterior of B4", xlab="True value = red line" )
abline(v = mean(simb[ , 5]), col="blue")
#abline(v = trueB4, col="red" )
hist(simb[ , 6],nclass=30, main="Posterior of B5", xlab="True value = red line" )
abline(v = mean(simb[ , 6]), col="blue")
#abline(v = trueB5, col="red" )
hist(simb[ , 7],nclass=30, main="Posterior of B6", xlab="True value = red line" )
abline(v = mean(simb[ , 7]), col="blue")
#abline(v = trueB6, col="red" )
hist(simb[ , 8],nclass=30, main="Posterior of B7", xlab="True value = red line" )
abline(v = mean(simb[ , 8]), col="blue")
#abline(v = trueB7, col="red" )
hist(simb[ , 9],nclass=30, main="Posterior of B8", xlab="True value = red line" )
abline(v = mean(simb[ , 9]), col="blue")
#abline(v = trueB8, col="red" )
hist(simb[ , 10],nclass=30, main="Posterior of B9", xlab="True value = red line" )
abline(v = mean(simb[ , 10]), col="blue")
#abline(v = trueB9, col="red" )
hist(simb[ , 11],nclass=30, main="Posterior of B10", xlab="True value = red line" )
abline(v = mean(simb[ , 11]), col="blue")
#abline(v = trueB10, col="red" )
hist(simb[ , 12],nclass=30, main="Posterior of B11", xlab="True value = red line" )
abline(v = mean(simb[ , 12]), col="blue")
#abline(v = trueB11, col="red" )
