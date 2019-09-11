# 1_jags_models_binomial.R
# binomial models for weekend/after hours regression
# prepares model, data and initial values
# version that uses JAGS on HPC
# called from 1_prepare_data_for_jags.R
# July 2019

current.wd = getwd()
setwd('Z:/weekend')

## create external text file with bugs model
# a1) intercept only, common time slope - country
model.file = 'binomial.bugs.intercept.txt'
bugs = file(model.file , 'w')
cat('model{
for (k in 1:N){
    dep[k] ~ dbin(p[k], denom[k])
    logit(p[k]) <- beta[1] + beta[2]*window[k] + beta.c[country[k]]
}
    for(j in 1:2){beta[j] ~ dnorm(0,0.001)}
    for (c in 1:M){
    beta.country[c] ~ dnorm(0, tau)
    beta.c[c] <- beta.country[c] - mean.country # centre, slower but better chains
    }
    mean.country <- mean(beta.country[1:M])
    tau ~ dgamma(1,1)
}', file=bugs)
close(bugs)
## as above, but with sinusoidal season
# a2) intercept only, common time slope - country
model.file = 'binomial.bugs.intercept.season.txt'
bugs = file(model.file , 'w')
cat('model{
for (k in 1:N){
    dep[k] ~ dbin(p[k], denom[k])
    logit(p[k]) <- beta[1] + beta[2]*window[k] + beta.cos[country[k]]*cos[k] + beta.sin[country[k]]*sin[k] + beta.c[country[k]]
}
    for(j in 1:4){beta[j] ~ dnorm(0,0.001)}
    for (c in 1:M){
      beta.country[c] ~ dnorm(0, tau[1])
      beta.c[c] <- beta.country[c] - mean.country 
      beta.cos[c] ~ dnorm(beta[3], tau[2])
      beta.sin[c] ~ dnorm(beta[4], tau[3])
    }
    mean.country <- mean(beta.country[1:M])
    for(j in 1:3){tau[j] ~ dgamma(1,1)}
}', file=bugs)
close(bugs)
# b) intercept and slope vary by country
model.file = 'binomial.bugs.intercept.slope.txt'
bugs = file(model.file , 'w')
cat('model{
for (k in 1:N){
    dep[k] ~ dbin(p[k], denom[k])
    logit(p[k]) <- beta[1] + slope[country[k]]*window[k] + beta.c[country[k]]
}
    for (c in 1:M){
      beta.country[c] ~ dnorm(0, tau[1])
      beta.c[c] <- beta.country[c] - mean.country # centre, slower but better chains
      slope[c] ~ dnorm(beta[2], tau[2]) # centred on beta[2]
    }
    for(j in 1:2){
     beta[j] ~ dnorm(0, 0.001)
     tau[j] ~ dgamma(1, 1)
    }
    mean.country <- mean(beta.country[1:M])
}', file=bugs)
close(bugs)
# b2) as above with season
model.file = 'binomial.bugs.intercept.slope.season.txt'
bugs = file(model.file , 'w')
cat('model{
for (k in 1:N){
    dep[k] ~ dbin(p[k], denom[k])
    logit(p[k]) <- beta[1] + slope[country[k]]*window[k] + beta.c[country[k]] + beta.cos[country[k]]*cos[k] + beta.sin[country[k]]*sin[k]
}
    for (c in 1:M){
      beta.country[c] ~ dnorm(0, tau[1])
      beta.c[c] <- beta.country[c] - mean.country # centre, slower but better chains
      slope[c] ~ dnorm(beta[2], tau[2]) # centred on beta[2]
      beta.cos[c] ~ dnorm(beta[3], tau[3])
      beta.sin[c] ~ dnorm(beta[4], tau[4])
    }
  mean.country <- mean(beta.country[1:M])
    for(j in 1:4){
      beta[j] ~ dnorm(0, 0.001)
      tau[j] ~ dgamma(1, 1)
    }
}', file=bugs)
close(bugs)

# prepare the data for bugs
N = nrow(for.model)
M = length(unique(for.model$country))
country.numbers = as.data.frame(table(for.model$country)); names(country.numbers) = c('Country','Count') # table of country numbers, used later to exclude small countries
bdata = list(N = N, M = M, 
             window = (for.model$window - median(for.model$window))/52, # centred and scaled to 52 weeks
             dep = for.model$dep, # Weekend, late night or holiday
             denom = for.model$denom, 
             country = as.numeric(as.factor(for.model$country))) # alphabetical order
if(season == TRUE){
  for.model$yrfrac = yrfraction(for.model$date)
  bdata$cos = cos(2*pi*for.model$yrfrac)
  bdata$sin = sin(2*pi*for.model$yrfrac)
}
# initial values
inits = list(tau=1, beta=c(0,0), beta.country=rep(0, M))
if(type == 'slope'){ # extra/alternative initial values for random slope model
  inits$slope = rep(0, M) 
  inits$tau = c(1,1)
}
if(season == TRUE){
  inits$beta = c(0,0,0,0)
  inits$tau = c(1,1,1,1) 
  inits$beta.cos = rep(0, M) 
  inits$beta.sin = rep(0, M) 
}
inits = rep(list(inits), num.chains)

# get ready for JAGS run on HPC
parms = c('beta', 'tau','beta.c')
model.file = 'binomial.bugs.intercept.txt'
if(season == TRUE){
  model.file = 'binomial.bugs.intercept.season.txt'
  parms = c(parms, 'beta.cos', 'beta.sin')
}
if(type == 'slope'){ # change file and add slope to monitored parameters
  model.file = 'binomial.bugs.intercept.slope.txt'
  if(season == TRUE){model.file = 'binomial.bugs.intercept.slope.season.txt'}
  parms = c(parms, 'slope')
}

# save data ready to run (had to use earlier version of data)
save(season.text, which.outcome, which.data.text, which.journal, type,
  MCMC, thin, num.chains, model.file, parms, inits, bdata, country.numbers,
  jags.filename.chains, jags.filename.results, file=jags.filename, version=2)
setwd(current.wd) # move back to directory
