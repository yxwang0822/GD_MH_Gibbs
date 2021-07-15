N = 26000 # Total number of iterations
rho = 0.5

simx = simy = vector()
simx[1] = simy[1] = 0 # Initialise vectors

# Gibbs sampler
for(i in 2:N){
  simx[i] = rnorm(1,rho*simy[i-1],sqrt(1-rho^2))
  simy[i] = rnorm(1,rho*simx[i],sqrt(1-rho^2))
}

# burnin and thinning
burnin = 1000
thinning = 25

# Gibbs sample
sim = cbind(simx[seq(from=burnin,to=N,by=thinning)],simy[seq(from=burnin,to=N,by=thinning)])

# A diagnosis tool to assess the convergence of the sampler
plot(ts(sim[,1]))
plot(ts(sim[,2]))

# Plots of the marginals and the joint distribution

hist(sim[,1])
hist(sim[,2])

plot(sim,col=1:1000)
plot(sim,type="l")