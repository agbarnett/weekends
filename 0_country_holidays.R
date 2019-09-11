# 0_country_holidays.R
# get the data on country holidays for 2012 to 2019
# July 2019
library("rjson")
library(dplyr)

## get holiday data
# from https://date.nager.at/Home/Countries; "100 countries are supported"
country.codes = read.table(file = 'data/country.codes.holidays.txt', header=T, stringsAsFactors = FALSE, sep='\t') %>%
  mutate(country = ifelse(country == 'United States', 'United states', country), # change wording to match BMJ data
         country = ifelse(country == 'United Kingdom', 'United kingdom', country), 
         country = ifelse(country == 'South Africa', 'South africa', country),
         country = ifelse(country == 'New Zealand', 'New zealand', country)
  )

# Only progress with countries that are in reviewer/author data
load('data/placesboth.RData')
country.codes.100 = filter(country.codes, country%in%over.100$country)
country.codes.not100 = filter(country.codes, country%in%over.100$country ==FALSE)
missing.from.holidays = over.100$country[over.100$country %in% country.codes$country == FALSE] # countries in BMJ data without holiday data

# Download the holiday data
holidays = NULL
for (c in 1:nrow(country.codes.100)){ # loop through countries
  for (y in 2012:2019){ # loop through years
    url = paste("https://date.nager.at/api/v2/PublicHolidays/", y, "/", country.codes.100$code[c], sep='')
    download.file(url, destfile = 'temp.txt') # download file from net
    json_data = fromJSON(file='temp.txt') # read JSON data
    for (k in 1:length(json_data)){ # loop through holidays
      this = data.frame(t(json_data[[k]])) 
      frame = data.frame(country = country.codes.100$country[c],
                         date = this$date[[1]],
                         localName = this$localName[[1]],
                         name = this$name[[1]],
                         fixed = this$fixed[[1]],
                         global = this$global[[1]],
                         counties = NA,
                         type = this$type[[1]], stringsAsFactors = FALSE)
      if(is.null(this$counties[[1]]) == FALSE){frame$counties = paste(this$counties[[1]], collapse=',')}
      holidays = bind_rows(holidays, frame)
    }
  }
}

# final formats
holidays = mutate(holidays,
  date = as.Date(date),
  country = ifelse(country == 'United states', 'United States', country), # change wording back !
  country = ifelse(country == 'United kingdom', 'United Kingdom', country), 
  country = ifelse(country == 'South africa', 'South Africa', country),
  country = ifelse(country == 'New zealand', 'New Zealand', country)
)

# save
save(holidays, missing.from.holidays, file='data/holidays.RData')
