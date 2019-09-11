# 0_address_data.R
# get all the paper and reviewer data and assign a timezone based on the geolocated address
# May 2019
library(readxl)
library(dplyr)
library(forcats)
library(ggmap) # for geocoding cities
library(geonames) # for turning lat and long into timezones
source('98_google_API.R') # register API with google (NOT SHARED ON GITHUB)
source('98_paste_without_NA.R')
source('0_address_edits.R') # for common edits of address data

## a) get the submission data
BMJ = read_excel('data/BMJ Submissions for Adrian Barnett with transmission date & city.xlsx') %>%
  filter(is.na(`Author Country/Region`) == FALSE) # exclude 7 with missing country
bmj.open = read_excel('data/BMJ Open submissions for Adrian Barnett with transmission date & city.xlsx') %>%
  filter(is.na(`Author Country/Region`) == FALSE) # exclude 3 with missing country
submissions = bind_rows(BMJ, bmj.open) %>%
    rename(
      'city' = 'Author City',
      'state' = 'Author State/Province',
      'country' = 'Author Country/Region') %>%
#  select(city, state, country) %>%
  mutate(datasource = 'submissions') 
## b) get the reviewer data
bmj.open = read_excel("data/BMJ Open_Completed reviewer data 2012 to 2018 for Adrian Barnett study.xlsx", col_names=TRUE) 
BMJ = read_excel("data/BMJ_Completed reviewer data 2012 to 2018 for Adrian Barnett study.xlsx", col_names=TRUE) 
reviewers = bind_rows(BMJ, bmj.open) %>%
  filter(is.na(`Reviewer Country/Region`) == FALSE) %>% # exclude 1198 with missing country
  rename(
    'city' = 'Reviewer City',
    'state' = 'Reviewer State/Province',
    'country' = 'Reviewer Country/Region') %>%
#  select(city, state, country) %>%
  mutate(datasource = 'reviewers') 

## concatenate the two data sources
data = rbind(submissions, reviewers)
# common clean up of address data:
data = address.edits(data)

## Exclude countries with under 100 submissions
over.100 = group_by(data, datasource, country) %>%
  summarise(count = n()) %>%
  filter(count >= 100) %>%
  ungroup() %>%
  select(-count, -datasource) %>%
  unique() # either 100 in reviewers or submissions
data = merge(over.100, data, by = 'country')

## geolocate all places - takes a while
# create geo data
places = select(data, city, state, country) %>%
  distinct() %>% # just unique places
  mutate(address = paste3(city, ', ', state, ', ', country, sep=''),
         address = stringr::str_replace(address, pattern='^, ', replacement = ''), # replace starting comma twice
         address = stringr::str_replace(address, pattern='^, ', replacement = ''),
         address = stringr::str_replace(address, pattern=', , ', replacement = ', '))
codes = mutate(geocode(places$address)) # takes a while
places = bind_cols(places, codes) # add lat and long back to places
save(places, file = 'data/places.RData') # saved because repeated database use will cost money
# now find time zone for every place based on lat and long
# exclude places with missing lon/lat
missing.lat.long = sum(is.na(places$lat))
places = filter(places, is.na(lat) == FALSE)
places$timezoneId = places$countryName = ''
places$gmtOffset = NA
for (i in 1:nrow(places)) { # loop, takes a long while
  # loop through each place
  res = GNtimezone(lat = places$lat[i],
                   lng = places$lon[i],
                   radius = 0) %>%
    select(gmtOffset, timezoneId, countryName)
  places$gmtOffset[i] = res$gmtOffset
  places$timezoneId[i] = as.character(res$timezoneId)
  places$countryName[i] = as.character(res$countryName) # used as a check
  Sys.sleep(9) # wait for this many secs
  if(i%%500 == 0){cat('Up to ',i,'\n', append=TRUE)}
}

# Save for use by 1_bmj_[xxx]_data.R
save(over.100, places, file = 'data/placesboth.RData') # 
