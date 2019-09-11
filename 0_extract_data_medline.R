# 0_extract_data_medline.R
# extra data from medline dump from pubmed
# just get date field, don't need anything else
# date received comes from PLOS ONE system (e.g., our audit paper is 2017-06-22)
# no idea if it accounts for time zone
# April 2019
library(dplyr)
library(stringr)
library(tidyr)

# weekday labels
wdays = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

years = 2007:2018 # ignore first year because too few results and add a lot of noise
earliest.date = as.Date('07/08/2006', '%d/%m/%Y') # this is a Monday, so window starts on Monday
earliest.date = as.Date('12/08/2006', '%d/%m/%Y') # move to Saturday, does this make a difference to the results by moving the window?
all = NULL
for (year in years){
  infile = paste('data/pubmed_medline', year, '.txt', sep='')

# could also use ,'AD  ' for address, but has line breaks
  xmas.dates = as.Date(paste(rep(2000:2018,3), c('-12-24','-12-25','-12-26'), sep='')) # all possible Xmas dates from Xmas eve to boxing day
  raw = read.table(infile, sep='-', header=FALSE, fill = TRUE, quote='', stringsAsFactors = FALSE) %>%
    filter(V1 %in% c('PHST')) %>% # just dates
    filter(str_detect(pattern='received', string=V2)) %>% # just  received date
    rename('date' = 'V2') %>%
    mutate(date= str_replace_all(pattern = ' 00:00 \\[received\\]', replacement='', string=date), # date received (submitted)
           date = as.Date(date, '%Y/%m/%d'),
           xmas = date %in% xmas.dates,
           weekday = weekdays(date),
           weekday = factor(weekday, levels=wdays)) %>%
    select(-starts_with('V'))
  all = rbind(all, raw)
} # end of year loop
all = mutate(all, year=as.numeric(format(date, '%Y'))) %>% # add year based on date
  filter(year > 1969) # remove one clear error
 
## now calculate stats on grouped data (cannot do above because years overlap)
# annual stats
results.xmas = group_by(all, year, xmas) %>% # count numbers
    summarise(count=n()) 
results = group_by(all, year, weekday) %>% # count numbers
    summarise(count=n()) 
# weekly stats
weekly = mutate(all, time = floor(as.numeric(date - earliest.date)/7) ) %>% # time in seven week windows, better than using %U. time is weeks since earliest date
    group_by(time, weekday) %>% # count numbers
    summarise(count=n()) 

## calculate annual percentages
#
results.xmas = group_by(results.xmas, year) %>%
    mutate(N = sum(count),
           percent = 100*(count/N)) %>%
  ungroup()
#
results = group_by(results, year) %>%
  mutate(N = sum(count),
         percent = 100*(count/N)) %>%
  ungroup()

## calculate weekly percentages of weekend (yes/no) for time-trend analysis
for.trend = mutate(weekly, weekend = weekday %in% c('Saturday','Sunday'),
                   weekend = factor(as.numeric(weekend), levels=0:1, labels=c('No','Yes'))) %>%
  group_by(time, weekend) %>%
  summarise(N = sum(count)) %>% # count weekdays and weekends
  ungroup() %>%
  spread(key=weekend, value=N) %>% # long to wide
  mutate(No = ifelse(is.na(No), 0, No),
         Yes = ifelse(is.na(Yes), 0, Yes)) # replace NA with zero

# save
save(weekly, results, results.xmas, years, for.trend, earliest.date, file = 'data/PLOSAnalysisReady.RData')
