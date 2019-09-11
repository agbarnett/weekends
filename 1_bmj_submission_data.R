# 1_bmj_submission_data.R
# read the BMJ & BMJ Open submission data from Sara and prepare it for analysis
# "the data for submissions between 01/01/2012 and today" (28 March 2019)
# June 2019
library(readxl)
library(dplyr)
library(forcats)
library(timechange) # for timezones
library(lubridate) # for force_tz and with_tz
source('0_address_edits.R') # for common edits of address data

# bmj open
# Sara: "I have attached the dataset for BMJ Open with transmission dates from 01/01/2012.
# I see that some of the papers don't have a manuscript type.
# I think it is fair to include all article types for BMJ Open as it only publishes research so you can just ignore this variable."

## get the data
# dates are imported as UTC time zone
BMJ = read_excel('data/BMJ Submissions for Adrian Barnett with transmission date & city.xlsx')
BMJ = mutate(BMJ, journal = 'BMJ') %>%
  filter(is.na(`Author Country/Region`) == FALSE) # exclude 7 with missing country
bmj.open = read_excel('data/BMJ Open submissions for Adrian Barnett with transmission date & city.xlsx')
bmj.open = mutate(bmj.open, journal = 'BMJ Open') %>%
  filter(is.na(`Author Country/Region`) == FALSE) # exclude 3 with missing country
# concatenate two journals, rename variables
bmj = bind_rows(BMJ, bmj.open) %>%
  rename(
    'city' = 'Author City',
    'state' = 'Author State/Province',
    'country' = 'Author Country/Region',
    'type' = "Manuscript Type",
    'datetime' = "Transmission Date"
  ) %>% # Sara: "transmission date which is the actual date /time the author submits the paper"
  mutate(year = as.numeric(format(datetime, '%Y')),
         datetime = force_tz(datetime, tzone = 'EST')) %>%# use force_tz because we only want to change timezone, not the hours and minutes
#  filter(year < 2019) %>% # remove papers in 2019 (reduce any seasonal confounding by using whole years) # removed this restriction in July 2019
  select(-year, -type, -`Submission Date - Original`) # unlikely to use these variables
# common clean up of address data:
bmj = address.edits(bmj)
# numbers for CONSORT flow chart
n.submitted.bmj = sum(bmj$journal == 'BMJ')
n.submitted.bmjopen = sum(bmj$journal != 'BMJ')

## Exclude countries with under 100 submissions
over.100 = group_by(bmj, country) %>%
  summarise(count = n()) %>%
  filter(count >= 100) %>%
  select(-count) %>%
  ungroup()
bmj = merge(over.100, bmj, by = 'country')
n.post.exclude.100 = nrow(bmj) # keep numbers lost

## merge with geolocation data
# load address data
load('data/placesboth.RData') # from 0_address_data.R
# merge geocoded places back with bmj data, exclude papers that could not be geocoded
bmj = left_join(bmj, places, by = c('country', 'state', 'city')) %>% ##### maybe by address because of NA? ####
filter(is.na(lat) == FALSE) %>% # remove those that could not be geocoded
  mutate(country = ifelse(country=='United kingdom', 'United Kingdom', country), # fix few missing capital letters
         country = ifelse(country=='United states', 'United States', country),
         country = ifelse(country=='Republic of korea', 'South Korea', country),
         country = ifelse(country=='Saudi arabia', 'Saudi Arabia', country),
         country = ifelse(country=='South africa', 'South Africa', country),
         country = ifelse(country=='New zealand', 'New Zealand', country),
         country = ifelse(country=='Hong kong', 'Hong Kong', country))
n.after.geomatch = nrow(bmj)

# check differences between country in BMJ system and Google country
check = filter(bmj, country !=countryName,
               str_detect(address, pattern='Hong kong') == FALSE,
               str_detect(countryName, pattern='Hong Kong') == FALSE)  # ignore differences in China/Hong Kong
table(check$country) # almost all discrepencies are for UK, and google data is more believable

# Given above check, assume any wrong address with UK is incorrect in journal system and correct in Google
wrong = filter(bmj, country != countryName,
               country == 'United Kingdom') %>% # only UK
  mutate(country = countryName) %>% # replace country with Google country
  select(-countryName)
not.wrong = filter(bmj, country == countryName) %>%
  select(-countryName)
bmj = bind_rows(wrong, not.wrong)
n.post.exclude.differing.country = nrow(bmj) # lose more because country is not equal to country name, but it's not UK

## Repeat exclusion of countries with under 100 submissions using
over.100 = group_by(bmj, country) %>%
  summarise(count = n()) %>%
  filter(count >= 100) %>%
  select(-count) %>%
  ungroup()
bmj = merge(over.100, bmj, by = 'country')
n.post.exclude.100.second = nrow(bmj) # keep numbers lost (second stage)

## change date/time to local timezone
# had to do in loops per 82 time zones
empty = NULL
for (tz in unique(bmj$timezoneId)) {
  one.tz = filter(bmj, timezoneId == tz) %>%
    mutate(
      local.datetime = with_tz(datetime, tzone = timezoneId[1]),  # change the time zone ...
      local.date = as.Date(local.datetime),  # ... now calculate local dates, hours, etc (can't do below because time zone is changed when data are pasted together)
      local.hour = as.numeric(format(local.datetime, '%H')) + as.numeric(format(local.datetime, '%M')) / 60 ,  # local hours and minutes (not bothered with seconds)
      weekend = format(local.date, '%u') %in% c(6, 7),
      weekend = factor(
        as.numeric(weekend),
        levels = 0:1,
        labels = c('Weekday', 'Weekend')
      )
    ) %>% #
    select(-local.datetime, -datetime) # drop local data time because it gets changed when data are concatenated with other time zones
  empty = bind_rows(empty, one.tz) # concatenate
}

## add country-specific weekend, see here: https://en.wikipedia.org/wiki/Workweek_and_weekend
# Malaysia varies by region, kept with Sat/Sun weekend
bmj = mutate(empty, 
             weekday = format(local.date, '%u'),
             weekend = weekday %in% c(6,7), # Sat/Sun
             weekend = ifelse(country %in% c('Afghanistan','Algeria','Bahrain','Bangladesh','Egypt','Iraq','Israel','Jordon','Kuwait','Libya','Maldives','Oman','Qatar','Saudi Arabia','Sudan','Syria','UAE','Yemen'), weekday %in% c(5,6), weekend), # Friday to Saturday
             weekend = ifelse(country %in% c('Iran','Somalia'), weekday==5, weekend), # Fri only
             weekend = ifelse(country %in% c('India','Hong Kong','Mexico','Uganda'), weekday==7, weekend), # Sun only
             weekend = factor(as.numeric(weekend), levels = 0:1, labels = c('Weekday', 'Weekend'))) %>%
  dplyr::select(-weekday)
## add after hours (late nights) - same across countries
bmj = mutate(bmj,
    journal = factor(journal),
    late.night = ifelse(local.hour < 7 | local.hour >= 18, 1, 0), # 6:00pm to 6:59am (before 7:00am)
    late.night = factor(late.night,
        levels = 0:1,
        labels = c('No', 'Yes')
      ))

## add public holidays
load('data/holidays.RData') # from 0_country_holidays.R
holidays = filter(holidays, is.na(counties)==TRUE) %>% # just national holidays, so nothing in counties
  select(country, date) %>%
  mutate(holiday=1) %>%
  unique() # remove duplicates
bmj = left_join(bmj, holidays, by=c('country'='country', 'local.date'='date')) %>% # could also merge by state, but need to reformat all states 
  mutate(holiday = ifelse(is.na(holiday)==TRUE, 0, holiday),
         holiday = factor(holiday, levels = 0:1, labels = c('No', 'Yes'))) %>%
  select(-state, -gmtOffset) # don't need these variables

## add windows for weeks
windows = data.frame(local.date = seq(min(bmj$local.date), max(bmj$local.date), 1)) %>% # range of observed dates
  mutate(monday = weekdays(local.date) == 'Monday', # new windows start on Monday
         window = cumsum(monday)) %>%
  filter(window>0, window< max(window)) %>% # remove first and last windows that do not contain full weeks
  select(-monday) # no longer needed
submission = left_join(windows, bmj, by='local.date')
n.after.windows = nrow(submission)

## set up the data in counts per week for the regression models
# first create dates from windows for seasonal analysis (used below)
for.merge = data.frame(window = seq(1,400,1)) %>%
  mutate(date = as.Date('2012-01-02')+(window*7)) # start on Jan 2nd
# a) now set up weekly counts for weekends
submissions.weekend = arrange(submission, local.date) %>%
  group_by(journal, window, country, weekend) %>%
  summarise(count = n()) %>%
  tidyr::spread(key=weekend, value=count) %>% # put weekends and weekdays on same row
  mutate(Weekday = ifelse(is.na(Weekday), 0, Weekday), # replace missing with zero
         Weekend = ifelse(is.na(Weekend), 0, Weekend),
         denom = Weekday + Weekend) %>%
  rename('dep' = 'Weekend') # identify dependent variable for winbugs
submissions.weekend = left_join(submissions.weekend, for.merge, by='window')
# b) now set up weekly counts for late nights
submissions.latenight = arrange(submission, window) %>%
  group_by(journal, window, country, late.night) %>%
  summarise(count = n()) %>%
  tidyr::spread(key=late.night, value=count) %>% # put weekends and weekdays on same row
  mutate(No = ifelse(is.na(No), 0, No), # replace missing with zero
         Yes = ifelse(is.na(Yes), 0, Yes),
         denom = No + Yes) %>%
  rename('dep' = 'Yes') # identify dependent variable for winbugs
submissions.latenight = left_join(submissions.latenight, for.merge, by='window')

## save the analysis-ready data
# consolidate numbers for flow diagram into a data frame
submission.numbers = data.frame(
  n.bmjopen = n.submitted.bmjopen,
  n.bmj = n.submitted.bmj,
  n.post.exclude.100 = n.post.exclude.100,
  n.post.exclude.differing.country = n.post.exclude.differing.country,
  n.post.exclude.100.second = n.post.exclude.100.second,
  n.after.geomatch = n.after.geomatch,
  n.after.windows = n.after.windows)
save(submission, submissions.weekend, submissions.latenight, submission.numbers, file = 'data/BMJAnalysisReadyAuthors.RData')
