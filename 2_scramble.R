# 2_scramble.R
# scramble times for initial analysis
# July 2019
set.seed(1234)

## scramble data using probability distributions
# a) reviewers
reviewer = mutate(reviewer,
                  local.date = as.Date('2012-01-02') + round(runif(nrow(reviewer), min=0, max=365.25*7)), # scramble dates to avoid trends
                  holiday = rbinom(n= nrow(reviewer), size=1, prob=0.02), # rarer
                  holiday = factor(holiday, levels=0:1, labels=c('No','Yes')),
                  late.night = rbinom(n= nrow(reviewer), size=1, prob=0.4),
                  late.night = factor(late.night, levels=0:1, labels=c('No','Yes')),
   weekend = rbinom(n= nrow(reviewer), size=1, prob=1/7),
  weekend = factor(weekend, levels=0:1, labels=c('Weekday', 'Weekend')),
  local.hour = runif(n = nrow(reviewer), min=0, max=23.99)) %>%
  select(-window) # remove window
## re-calculate windows
windows = data.frame(local.date = seq(min(reviewer$local.date), max(reviewer$local.date), 1)) %>% # range of observed dates
  mutate(monday = weekdays(local.date) == 'Monday', # new windows start on Monday
         window = cumsum(monday)) %>%
  filter(window > 0, window < max(window)) %>% # remove first and last windows that do not contain full weeks
  select(-monday) # no longer needed
reviewer = left_join(windows, reviewer, by='local.date') %>%
  filter(!is.na(journal)) # remove one day with no reviewer submissions

# b) authors
submission = mutate(submission,
                    local.date = as.Date('2012-01-02') + round(runif(nrow(submission), min=0, max=365.25*7)), # scramble dates to avoid trends
                    holiday = rbinom(n= nrow(submission), size=1, prob=0.02), # rarer
                  holiday = factor(holiday, levels=0:1, labels=c('No','Yes')),
                  late.night = rbinom(n= nrow(submission), size=1, prob=0.4),
                  late.night = factor(late.night, levels=0:1, labels=c('No','Yes')),
                  weekend = rbinom(n= nrow(submission), size=1, prob=1/7),
                  weekend = factor(weekend, levels=0:1, labels=c('Weekday', 'Weekend')),
                  local.hour = runif(n = nrow(submission), min=0, max=23.99)) %>%
  select(-window) # remove window
## re-calculate windows
windows = data.frame(local.date = seq(min(submission$local.date), max(submission$local.date), 1)) %>% # range of observed dates
  mutate(monday = weekdays(local.date) == 'Monday', # new windows start on Monday
         window = cumsum(monday)) %>%
  filter(window > 0, window < max(window)) %>% # remove first and last windows that do not contain full weeks
  select(-monday) # no longer needed
submission = left_join(windows, submission, by='local.date') %>%
  filter(!is.na(journal)) # remove one day with no reviewer submissions


