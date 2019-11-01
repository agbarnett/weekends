# 1_bmj_reviewer_data.R
# read the BMJ & BMJ Open peer reviewer data from Sara and prepare it for analysis
# update after new data on 30 October 2019
# October 2019
library(readxl)
library(dplyr)
library(lubridate) # for force_tz and with_tz
source('0_address_edits.R') # for common edits of address data

## get the data from Excel, covers the dates 2012-01-01 to 2018-12-31
# BMJ Open 68018 rows
bmj.open = read_excel("data/oct2019/BMJ Open Reviews completed 2012 to 2019 for Adrian Barnett's study 4 (final).xlsx", col_names=TRUE, sheet='page') %>%
  mutate(journal = 'BMJ Open') %>%
  rename('Exclude' = `Exclude?`) %>%
  select(-"Manuscript ID - Original",-"Manuscript ID", -"# Revisions")
# BMJ 18868 rows
BMJ = read_excel("data/oct2019/BMJ Completed reviewer data 2012 to 2019 for Adrian Barnett study 4 (final) indicating P&P reviewers.xls", col_names=TRUE) %>%
  mutate(journal = 'BMJ') %>%
  select(-"ManuscriptIDOriginal",-"ManuscriptID", -"@#Revisions") %>%
  rename(
    "Reviewer Person ID" = "ReviewerPersonID",
    "Manuscript Type" = "ManuscriptType" , # add spaces
    "Date Score Sheet Completed" = "DateScoreSheetCompleted",
    'Reviewer Country/Region' = "ReviewerCountryRegion",
    'Reviewer State/Province' = "ReviewerStateProvince",
    "Reviewer City" = "ReviewerCity")
bmj = bind_rows(bmj.open, BMJ) %>%
  rename(
    'city' = 'Reviewer City',
    'state' = 'Reviewer State/Province',
    'country' = 'Reviewer Country/Region',
    'type' = "Manuscript Type",
    'datetime' = "Date Score Sheet Completed") %>% # 
  mutate(year = as.numeric(format(datetime, '%Y')),
         datetime = force_tz(datetime, tzone = 'EST')) %>% # change to NA # use force_tz because we only want to change timezone, not the hours and minutes
  select(-year, -type) # unlikely to use these variables
# numbers for CONSORT flow chart
n.submitted.bmj = sum(bmj$journal == 'BMJ')
n.submitted.bmjopen = sum(bmj$journal != 'BMJ')

# exclude missing address
bmj = filter(bmj, !is.na(country))
n.missing.address = nrow(bmj)

# common clean up of address data:
bmj = address.edits(bmj)

## Exclude statistical reviews
bmj = filter(bmj, Exclude != 'Yes, statistician' | is.na(Exclude))
n.excluded.stats = nrow(bmj)

## Exclude Patient and public reviews 
bmj = filter(bmj, Exclude != 'Possibly, patient and public reviewer' | is.na(Exclude))
n.excluded.patients = nrow(bmj)

## Exclude transfers from other journals
bmj = filter(bmj, is.na(Exclude)) %>% # only transfer left in 
  select(-Exclude, -starts_with("Transfer"), -starts_with('Receiving'))
n.excluded.transfer = nrow(bmj)

## Exclude countries with under 100 submissions (currently counted across the two journals)
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
         country = ifelse(country=='South africa', 'South Africa', country),
         country = ifelse(country=='New zealand', 'New Zealand', country),
         country = ifelse(country=='Hong kong', 'Hong Kong', country))
n.after.geomatch = nrow(bmj)

# check differences between country in BMJ system and Google country
check = filter(bmj, country !=countryName,
               str_detect(address, pattern='Hong kong') == FALSE,
               str_detect(countryName, pattern='Hong Kong') == FALSE)  # ignore differences in China/Hong Kong
table(check$country) # almost all discrepencies are for UK, and google data is more believable
# Given above check, assume any wrong address with UK is incorrect in system and correct in Google
# lose ~300 more because country is not equal to country name, but it's not UK
wrong = filter(bmj, country != countryName,
               country == 'United Kingdom') %>% # only UK
  mutate(country = countryName) %>% # replace country with Google country
  select(-countryName)
not.wrong = filter(bmj, country == countryName) %>%
  select(-countryName)
bmj = bind_rows(wrong, not.wrong)
n.post.exclude.differing.country = nrow(bmj) # 

## Repeat exclusion of countries with under 100 submissions using
over.100 = group_by(bmj, country) %>%
  summarise(count = n()) %>%
  filter(count >= 100) %>%
  select(-count) %>%
  ungroup()
bmj = merge(over.100, bmj, by = 'country')
n.post.exclude.100.second = nrow(bmj) # keep numbers lost (second stage)

## change date/time to local timezone
# had to do in loops per time zones (around 100)
empty = NULL
for (tz in unique(bmj$timezoneId)) {
  one.tz = filter(bmj, timezoneId == tz) %>%
    mutate(
      local.datetime = with_tz(datetime, tzone = timezoneId[1]), # change the time zone, which also changes the time ...
      local.date = as.Date(local.datetime),  # ... now calculate local dates, hours, etc (can't do below because time zone is changed when data are pasted together)
      local.hour = as.numeric(format(local.datetime, '%H')) + as.numeric(format(local.datetime, '%M')) / 60   # local hours and minutes (not bothered with seconds)
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

## add windows for weeks for regression analysis
windows = data.frame(local.date = seq(min(bmj$local.date), max(bmj$local.date), 1)) %>% # range of observed dates
  mutate(monday = weekdays(local.date) == 'Monday', # new windows start on Monday
         window = cumsum(monday)) %>%
  filter(window>0, window< max(window)) %>% # remove first and last windows that do not contain full weeks
  select(-monday) # no longer needed
reviewer = left_join(windows, bmj, by='local.date') %>%
  filter(!is.na(journal)) # remove one day with no reviewer submissions
n.after.windows = nrow(reviewer)

### set up the data in counts per week for the regression models ###
# first create dates from windows for seasonal analysis (used below)
for.merge = data.frame(window = seq(1,400,1)) %>%
  mutate(date = as.Date('2012-01-02')+(window*7)) # start on Jan 2nd
# a) weekly counts for weekends
reviews.weekend = arrange(reviewer, local.date) %>%
  group_by(journal, window, country, weekend) %>%
  summarise(count = n()) %>%
  tidyr::spread(key=weekend, value=count) %>% # put weekends and weekdays on same row
  mutate(Weekday = ifelse(is.na(Weekday), 0, Weekday), # replace missing with zero
         Weekend = ifelse(is.na(Weekend), 0, Weekend),
         denom = Weekday + Weekend) %>%
  rename('dep' = 'Weekend') # identify dependent variable for winbugs
reviews.weekend = left_join(reviews.weekend, for.merge, by='window')

## Save
reviewer.numbers = data.frame(n.bmj = n.submitted.bmj,
                     n.bmjopen = n.submitted.bmjopen,
                     n.missing.address = n.missing.address,
                     n.excluded.stats = n.excluded.stats,
                     n.excluded.patients = n.excluded.patients,
                     n.excluded.transfer = n.excluded.transfer,
                     n.post.exclude.100 = n.post.exclude.100,
                     n.post.exclude.differing.country = n.post.exclude.differing.country,
                     n.post.exclude.100.second = n.post.exclude.100.second,
                     n.after.geomatch = n.after.geomatch,
                     n.after.windows = n.after.windows)
save(reviewer, reviews.weekend, reviewer.numbers, file='data/BMJAnalysisReadyReviewers.RData')

# version for sharing on github
reviewer = dplyr::select(reviewer, local.date, local.hour, window, country, journal, timezoneId, weekend, holiday)
save(reviewer, file='data/BMJReviewers.RData')


