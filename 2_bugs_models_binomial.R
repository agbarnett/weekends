# 2_bugs_models_binomial.R
# bugs binomial models for weekend/after hours regression
# called from 2_analysis.Rmd

# MCMC basics
#MCMC = 100; thin = 1; num.chains = 2
MCMC = 1000; thin = 3; num.chains = 2

## create external text file with bugs model
# a1) intercept only - country
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
# a2) intercept only - country
model.file = 'binomial.bugs.intercept.season.txt'
bugs = file(model.file , 'w')
cat('model{
for (k in 1:N){
    dep[k] ~ dbin(p[k], denom[k])
    logit(p[k]) <- beta[1] + beta[2]*window[k] + beta.cos.c[country[k]]*cos[k] + beta.sin.c[country[k]]*sin[k] + beta.c[country[k]]
}
    for(j in 1:2){beta[j] ~ dnorm(0,0.001)}
    for (c in 1:M){
      beta.cos[c] ~ dnorm(0, tau[1])
      beta.sin[c] ~ dnorm(0, tau[2])
      beta.country[c] ~ dnorm(0, tau[3])
      beta.cos.c[c] <- beta.cos[c] - mean.cos # centre, slower but better chains
      beta.sin.c[c] <- beta.sin[c] - mean.sin 
      beta.c[c] <- beta.country[c] - mean.country 
    }
    mean.cos <- mean(beta.cos[1:M])
    mean.sin <- mean(beta.sin[1:M])
    mean.country <- mean(beta.country[1:M])
    for(j in 1:3){tau[j] ~ dgamma(1,1)}
}', file=bugs)
close(bugs)
# b) intercept and slope - country
model.file = 'binomial.bugs.intercept.slope.txt'
bugs = file(model.file , 'w')
cat('model{
for (k in 1:N){
    dep[k] ~ dbin(p[k], denom[k])
    logit(p[k]) <- beta[1] + slope[country[k]]*window[k] + beta.c[country[k]]
}
    for(j in 1:2){beta[j] ~ dnorm(0,0.001)}
    for (c in 1:M){
    slope[c] ~ dnorm(beta[2], tau[1]) # centred on beta[2]
    beta.country[c] ~ dnorm(0, tau[2])
    beta.c[c] <- beta.country[c] - mean.country # centre, slower but better chains
    }
    mean.country <- mean(beta.country[1:M])
    for(k in 1:2){tau[k] ~ dgamma(1,1)}
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
      slope[c] ~ dnorm(beta[2], tau[2]) # centred on beta[2]
      beta.cos[c] ~ dnorm(beta[3], tau[3])
      beta.sin[c] ~ dnorm(beta[4], tau[4])
      beta.c[c] <- beta.country[c] - mean.country # centre, slower but better chains
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
             dep = for.model$dep, # Weekend or holiday
             denom = for.model$denom, 
             country = as.numeric(as.factor(for.model$country)))
if(season == TRUE){
  for.model$yrfrac = yrfraction(for.model$date)
  bdata$cos = cos(for.model$yrfrac)
  bdata$sin = sin(for.model$yrfrac)
}
# initial values
inits = list(tau=1, beta=c(0,0), beta.country=rep(0, M))
if(type == 'slope'){ # extra/alternative initial values for random slope model
  inits$slope = rep(0, M) 
  inits$tau = c(1,1)
}
if(season == TRUE){
  inits$tau=c(1,1,1); inits$beta.cos = rep(0, M); inits$beta.sin = rep(0, M) 
  if(type=='slope'){inits$tau = c(1,1,1,1); inits$beta=c(0,0,0,0)} # four
}
inits = rep(list(inits), num.chains)

# run BUGS
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
bugs.results =  bugs(data=bdata, inits=inits, parameters=parms, model.file=model.file,
                     n.chains = num.chains, n.iter=MCMC*thin*2, debug=T, DIC=TRUE, # *2 for burn-in
                     bugs.directory="c:/Program Files/WinBUGS14")

# estimate additional variables from chains
intercept = as.vector(bugs.results$sims.array[,1:2,1]) # two chains
slope = as.vector(bugs.results$sims.array[,1:2,2])
r1 = data.frame(var='p', num=1, res= 1 / (1 + exp(-intercept)), stringsAsFactors = FALSE) # inverse logit
r2 = data.frame(var='p', num=2, res= 1 / (1 + exp(-intercept-slope)), stringsAsFactors = FALSE)
r3 = data.frame(var='diff', num=NA, res=r2$res - r1$res, stringsAsFactors = FALSE)
r4 = data.frame(var='ratio', num=NA, res=r2$res / r1$res, stringsAsFactors = FALSE)
calcs = bind_rows(r1, r2, r3, r4) %>%
  group_by(var, num) %>%
  summarise(mean=mean(res), lower=quantile(res, 0.025), upper=quantile(res, 0.975)) %>%
  ungroup()
# re-scale intercepts to probabilities
index = which(str_detect(string=names(bugs.results$sims.array[1,1,]), pattern='beta.c\\['))
ints = NULL
for (k in index){
  country = as.vector(bugs.results$sims.array[,1:2,k]) # two chains
  f = data.frame(var='intercept', num=k +1 -min(index), res= 1 / (1 + exp(-intercept-country)), stringsAsFactors = FALSE) # inverse logit
  ints = rbind(ints, f)
}
calcs2 = group_by(ints, var, num) %>%
  summarise(mean=mean(res), lower=quantile(res, 0.025), upper=quantile(res, 0.975)) %>%
  ungroup()

# calculate summary stats
ests = data.frame(bugs.results$summary[,c(1,3,7)]) %>%
  mutate(var = row.names(bugs.results$summary),
         num = as.numeric(stringr::str_remove_all(string=var, pattern = '[a-z]|\\[|\\.|\\]')),
         var = stringr::str_remove_all(string=var, pattern = '[0-9]|\\[|\\]')) %>%
  filter(var !='deviance') %>%
  rename('lower'='X2.5.',
         'upper'='X97.5.') %>%
  bind_rows(calcs, calcs2) # add calculated estimates
dic.frame = data.frame(data = which.data.text, journal = which.journal, model = type, pD=bugs.results$pD, DIC=bugs.results$DIC, stringsAsFactors = FALSE)

## make chain checks for appendix, just overall intercept and slope
index = which(names(bugs.results$sims.array[1,1,]) %in% c('beta[1]','beta[2]')) # locate intercept and slope
chain1 = bugs.results$sims.array[,1, index] # 
chain1 = tidyr::gather(as.data.frame(chain1)) %>%
  mutate(chain = 1, iter=1:n())
chain2 = bugs.results$sims.array[,2, index] # 
chain2 = tidyr::gather(as.data.frame(chain2)) %>%
  mutate(chain = 2, iter=1:n())
to.plot = bind_rows(chain1, chain2) %>%
  group_by(key) %>%
  mutate(mini = min(iter)) %>%
  mutate(iter = iter - mini + 1)
cplot = ggplot(data=to.plot, aes(x=iter, y=value, col=factor(chain)))+
  geom_line(lty=2)+
  theme_bw()+
  xlab('Iteration')+
  ylab('Estimate')+
  scale_color_manual('Chain', values=cbPalette[2:3])+
  facet_wrap(~key, scales = 'free_y')
# save plots
outfile = paste('figures/MCMC.', which.outcome, '.', which.data.text, '.', which.journal, '.', type, '.jpg', sep='')
jpeg(outfile, width=5, height=4, units='in', res=300, quality = 100)
print(cplot)
invisible(dev.off())

# save results, DIC and plot
outfile = paste('data/bugs.', which.outcome, '.', which.data.text, '.', which.journal, '.', type, '.RData', sep='') #filename includes data source (converted from object to string), model type and journal
save(cplot, ests, dic.frame, country.numbers, file=outfile)
remove(bugs.results, intercept, slope, calcs, calcs2, r1, r2, r3, r4) # tidy up
