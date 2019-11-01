# 2_country_difference_from_average.R
# look at between country differences from average times
# analysis of late nights/early mornings versus in hours
# separate analysis for reviews and submissions
# October 2019
library(R2WinBUGS)
library(dplyr)
library(tidyr)

## load the author and reviewer data
load('data/BMJAnalysisReadyAuthors.RData') # authors from 1_bmj_submission_data.R
load('data/BMJAnalysisReadyReviewers.RData') # reviewers from 1_bmj_reviewer_data.R
reviewer = dplyr::select(reviewer, -lon, -lat, -timezoneId, -city, -address) %>% # drop variables not used here
  mutate(type='Reviews')
submission = dplyr::select(submission, -lon, -lat, -timezoneId, -city, -address) %>% # drop variables not used here
  mutate(type='Submission')
both = bind_rows(reviewer, submission) # concatenate

## get number of submissions/reviews per country per journal per hour
for.model = mutate(both, 
                   journal = as.character(journal),
                  hour = floor(local.hour),
                  type = ifelse(type=='Submission', 'Submissions', type)) %>% # tiny change to label
  group_by(type, journal, country, hour) %>%
  summarise(count = n()) %>%
  ungroup()
# fill in hours with no results; tidyr::expand not working!
fill = NULL
for (t in c('Reviews','Submissions')){
  for (j in c('BMJ','BMJ Open')){
    for (c in unique(for.model$country)){
      for (h in 0:23){
        frame = data.frame(type = t, journal = j, country = c, hour = h, stringsAsFactors = FALSE)
        fill = bind_rows(fill, frame)
      }
    }
  }
}
# merge filled and original data and replace missing with zero
for.model = left_join(fill, for.model, by=c('type','journal','country','hour')) %>%
  mutate(count = ifelse(is.na(count), 0, count))
# now get totals per journal/type/country combination
for.model = group_by(for.model, type, journal, country) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(cos.hour = cos(2*pi*(hour-1)/24), # sinusoid
         sin.hour = sin(2*pi*(hour-1)/24))
# only model countries with a minimum of 100 for all model types
stats.per.country = select(for.model, country, type, journal, total) %>%
  unique() %>%
  group_by(country) %>%
  summarise(n=n(), min = min(total)) %>%
  ungroup() %>%
  filter(n==4, min>=100) # all four combinations and at least 100 per combination
#
for.model = filter(for.model, country %in% stats.per.country$country) %>%
  mutate(country.num = as.numeric(as.factor(country))) 

## model pattern
# standard hour (non-parametric) plus smooth differences using sinusoid for country, journal and type (review or submission)
model.file = 'country.difference.bugs.txt'
bugs = file(model.file , 'w')
cat('model{
for (k in 1:N){
    count[k] ~ dpois(mu[k])
    log(mu[k]) <- offset[k] + intercept + beta.c[hour[k]] + gamma[country[k], hour[k]] + alpha[hour[k]]*bmj[k]
}
    for (j in 1:24){
      beta[j] ~ dnorm(0, tau.beta)
      beta.c[j] <- beta[j] - mean.beta # centre on zero
    }
    mean.beta <- mean(beta[1:24])
    # smooth country-specific difference
    for (j in 1:C){ # loop through countries
      cosine[j] ~ dnorm(0, 0.0001)
      sine[j] ~ dnorm(0, 0.0001)
      cosine.c[j] <- cosine[j] - mean.country.cosine # centre on zero
      sine.c[j] <- sine[j] - mean.country.sine
      for (k in 1:24){ # create smooth sinusoid shape
        gamma[j,k] <- (cosine.c[j]*cosf[k]) + (sine.c[j]*sinf[k])
      }
    }
    mean.country.cosine <- mean(cosine[1:C])
    mean.country.sine <- mean(sine[1:C])
    # smooth sinusoid for journal difference
    cosine.j ~ dnorm(0, 0.0001)
    sine.j ~ dnorm(0, 0.0001)
    for (k in 1:24){
      alpha[k] <- (cosine.j*cosf[k]) + (sine.j*sinf[k])
    }
    #
    intercept ~ dnorm(0, 0.0001)
    tau.beta ~ dgamma(1, 1)
}', file=bugs)
close(bugs)

## prepare the data for bugs
# reviews or submissions
for (loop in c('Reviews','Submissions')){
  for.this.model = filter(for.model, type==loop)
  N = nrow(for.this.model)
  C = max(for.this.model$country.num)
  bdata = list(N = N, C = C, 
             cosf = cos(2*pi*0:23/24), # for sinusoids (reduce computation)
             sinf = sin(2*pi*0:23/24), # for sinusoids
             bmj = as.numeric(for.this.model$journal=='BMJ'),
             country = for.this.model$country.num,
             count = for.this.model$count,
             offset = log(for.this.model$total), # denominator as offset
             hour = for.this.model$hour + 1) # add one as it is an index

  # initial values
  num.chains = 2; MCMC=5000; thin=3
  inits = list(tau.beta=1, intercept=0, beta=rep(0,24), cosine=rep(0,C), sine=rep(0,C), cosine.j=0, sine.j=0)
  inits = rep(list(inits), num.chains)
  
  # run BUGS
  parms = c('mu','beta', 'tau.beta', 'intercept','cosine.c','sine.c','cosine.j','sine.j')
  bugs.results =  bugs(data=bdata, inits=inits, parameters=parms, model.file=model.file, bugs.seed=879497,
                       n.chains = num.chains, n.thin=thin, n.iter=MCMC*thin*2, debug=TRUE, DIC=FALSE, # *2 for burn-in
                       bugs.directory="c:/Program Files/WinBUGS14")
  # save including chains
  country.list = stats.per.country$country
  outfile = paste('data/bugs.results.country.difference.', loop,'.RData', sep='')
  save(bdata, country.list, MCMC, num.chains, thin, bugs.results, file=outfile)
} # end of loop

# next: 2_country_difference_from_average_plot.R
