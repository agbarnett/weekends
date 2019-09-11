# 1_jags_models_binomial_holidays.R
# binomial models for holiday regression
# prepares model, data and initial values
# version that uses JAGS on HPC
# called from 1_prepare_data_for_jags.R
# August 2019

# get holiday data
load('data/holidays.RData') # from 0_country_holidays.R

# move to HPC
current.wd = getwd()
setwd('Z:/weekend')

## create external text file with bugs model
# random country effect for holiday variable, still need a random intercept? that would surely be confounded with holiday, so no
model.file = 'binomial.bugs.intercept.holiday.txt'
bugs = file(model.file , 'w')
cat('model{
for (k in 1:N){
    dep[k] ~ dbin(p[k], denom[k])
    logit(p[k]) <- beta[1] + beta[2]*window[k] + beta.c[country[k]]*holiday[k]
}
    for(j in 1:2){beta[j] ~ dnorm(0,0.001)}
    for (c in 1:M){
      beta.c[c] ~ dnorm(0, tau) # do not centre
      inter[c] <- beta[1] + beta.c[c] # probability per country
    }
    overall <- beta[1] + mean(beta.c[1:M]) # average probability
    tau ~ dgamma(1,1)
}', file=bugs)
close(bugs)


## Prepare data for analysis to examine holidays
# a) get holiday data
holidays = filter(holidays, is.na(counties)==TRUE) %>% # just national holidays, so nothing in counties
  dplyr::select(country, date) %>%
  mutate(holiday = 'Yes') %>%
  unique() # remove duplicates
countries.with.holidays = unique(holidays$country)
# get compete range of windows and dates
dates.and.windows = dplyr::select(both, local.date, window) %>%
  unique() # now repeat over all countries 
dates.and.windows.plus = NULL
for (country in countries.with.holidays){
  dates.and.windows.plus = bind_rows(dates.and.windows.plus, 
                                     mutate(dates.and.windows, country=country))
}
# add window information to holiday data (get all days within window)
holidays.plus.windows = full_join(holidays, dates.and.windows.plus, by=c('country', 'date'='local.date')) %>%
  filter(!is.na(window)) %>%  # remove holidays outside windows
  mutate(holiday = tidyr::replace_na(holiday, 'No')) %>%
  group_by(country, window) %>%
  mutate(any.hols = any(holiday == 'Yes')) %>% # Only windows with at least one holiday (take all seven days)
  filter(any.hols == TRUE) %>%
  dplyr::select(-any.hols) %>%
  ungroup()

# b) count submissions by date
count.data = filter(both, journal==which.journal, type==which.data.text) %>%
  group_by(country, window, local.date) %>%
  summarise(dep = n()) %>% # counts per day (dependent variable)
  ungroup()

# b) merge counts with holiday information; keep only weeks with at least one holiday
for.model = left_join(holidays.plus.windows, count.data, by=c('country', 'window', 'date'='local.date')) %>%
  mutate(dep = tidyr::replace_na(dep, 0)) %>%
  group_by(country, window) %>%
  mutate(denom = sum(dep)) %>% # counts per week (denominator)
  ungroup() %>%
  filter(denom > 0 ) %>% # only weeks with at least one paper
  arrange(country, date)

# prepare the data for bugs
N = nrow(for.model)
M = length(unique(for.model$country))
country.numbers = as.data.frame(table(for.model$country)); names(country.numbers) = c('Country','Count') # table of country numbers, used later to exclude small countries
bdata = list(N = N, M = M, 
             holiday = as.numeric(for.model$holiday=='Yes'),
             window = (for.model$window - median(for.model$window))/52, # centred and scaled to 52 weeks
             dep = for.model$dep, # Weekend, late night or holiday
             denom = for.model$denom, 
             country = as.numeric(as.factor(for.model$country))) # alphabetical order
# initial values
inits = list(tau=1, beta=c(0,0), beta.country=rep(0, M))
if(type == 'slope'){ # extra/alternative initial values for random slope model
  inits$slope = rep(0, M) 
  inits$tau = c(1,1)
}
inits = rep(list(inits), num.chains)

# get ready for JAGS run on HPC
parms = c('beta', 'tau','beta.c','inter','overall')

# save data ready to run (had to use earlier version of data)
save(which.outcome, which.data.text, which.journal, type,
  MCMC, thin, num.chains, model.file, parms, inits, bdata, country.numbers,
  jags.filename.chains, jags.filename.results, file=jags.filename, version=2)
setwd(current.wd) # move back to directory
