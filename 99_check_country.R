# 99_check_country.R
# check the results over time for one country
# July 2019

country.to.check = 'South Korea'
journal.to.check = 'BMJ'

## Trend
# plot weekends by week-to-week
for.plot = arrange(submission, local.date) %>%
  filter(country == country.to.check, journal==journal.to.check) %>%
  mutate(weekend = as.character(weekend)) %>%
  group_by(window, weekend) %>%
  summarise(count = n()) %>%
  tidyr::spread(key=weekend, value=count) %>% # put weekends and weekdays on same row
  mutate(Weekday = ifelse(is.na(Weekday), 0, Weekday), # replace missing with zero
         Weekend = ifelse(is.na(Weekend), 0, Weekend),
         denom = Weekday + Weekend,
         percent = 100*Weekend/denom)
tplot = ggplot(data=for.plot, aes(x=window, y=percent))+
  geom_line()+
  geom_smooth(col='skyblue', se = FALSE, method='loess', span=0.75)+
  g.theme+
  scale_x_continuous(breaks=axis.labels$window, labels=axis.labels$year)+
  ylab('Weekly percent')+
  xlab('Time')
tplot

## Season
# add dates from windows for seasonal analysis
for.merge = data.frame(window = seq(1,400,1)) %>%
  mutate(date = as.Date('2012-01-02')+(window*7)) # start on Jan 2nd
for.plot = left_join(for.plot, for.merge, by='window') %>%
  mutate(month = as.numeric(format(date, '%m')))
seasplot = ggplot(data=for.plot, aes(x=factor(month), y=percent))+
  geom_boxplot()+
  g.theme+
  ylab('Weekly percent')+
  xlab('Month')
seasplot
