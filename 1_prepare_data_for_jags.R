# 1_prepare_data_for_jags.R
# loop through the options and prepare the data for JAGS
# data from 1_bmj_reviewer_data.R and 1_bmj_submission_data.R
# August 2019
library(dplyr)
library(season)

# MCMC basics
#MCMC = 100; thin = 1; num.chains = 2
MCMC = 1000; thin = 3; num.chains = 2

## a) weekends and late nights
# loop through options
for (which.outcome in c('weekend','latenight')){
  for (which.data.text in c('submissions','reviews')){ # use data of weekly counts
    infile = ifelse(which.data.text=='submissions', 'data/BMJAnalysisReadyAuthors.RData', 'data/BMJAnalysisReadyReviewers.RData')
    load(infile)
    which.data = get(paste(which.data.text, '.', which.outcome, sep='')) # data with windows; dependent variable set as 'dep'
    for (which.journal in c('BMJ','BMJ Open')){ 
      for.model = filter(which.data, journal == which.journal) 
      for (type in c('intercept','slope')){ 
        for (season in c(TRUE, FALSE)){ 
          source('99_jags_filename.R') # create filenames for data, chains and results
          # prepare the data
          source('1_jags_models_binomial.R')
        }
      }
    }
  }
}

## b) holidays
which.outcome = 'holiday'
# loop through options, fewer because used a simpler model
for (which.data.text in c('Submission','Reviews')){
  infile = ifelse(which.data.text=='Submission', 'data/BMJAnalysisReadyAuthors.RData', 'data/BMJAnalysisReadyReviewers.RData')
  load(infile)
  for (which.journal in c('BMJ','BMJ Open')){ 
    type = 'intercept' # do not use slope model
    season = FALSE # always false for holidays
    source('99_jags_filename.R') # create filenames for data, chains and results
    # prepare the data:
    source('1_jags_models_binomial_holidays.R')
  }
}


# ... now run makemany.R or makemany.holidays.R on HPC