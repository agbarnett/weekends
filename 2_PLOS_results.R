# 1_PLOS_results.R
# plot and model the PLOS weekend/xmas results
# April 2019
library(ggplot2)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
library(dplyr)
load('data/PLOSAnalysisReady.RData') # from 0_extract_data_medline.R

# observed data by days of the week
wplot = ggplot(data=results, aes(x=year, y=percent, col=weekday))+
  geom_line(lwd=1.1)+
  theme_bw()
wplot

# observed xmas over time
results.xmas = filter(results.xmas, xmas==TRUE) # just yes
xplot = ggplot(data=results.xmas, aes(x=year, y=percent))+
  geom_line(lwd=1.1)+
  theme_bw()
xplot

# combine into weekends vs weekdays
weekend = group_by(results, year) %>%
  mutate(weekend = ifelse(weekday %in% c('Saturday','Sunday'), 1, 0)) %>%
  group_by(year, weekend, N) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  filter(weekend==1) # just weekend for binomial model

# Model results (weekend vs weekdays)
m = glm(count ~ I(year-2007), offset=log(N), data=weekend, family=poisson)
#
pred = data.frame(year=2007:2030, N=1, type='link')
pred$fit = exp(predict(m, newdata = pred)) / 2 # divide by two to get one day
pred$week.fit = (1-(2*pred$fit))/5
ggplot(data=pred, aes(x=year, y=fit))+
  geom_line()+
  geom_hline(yintercept = 1/7, lty=2, col='red')+
  ylab('Probability')

## combine predictions and plot
wplot = ggplot(data=results, aes(x=year, y=percent, col=weekday))+
  geom_line(data=pred, aes(x=year, y=fit*100), col='black', lty=2)+
  geom_line(data=pred, aes(x=year, y=week.fit*100), col='black', lty=2)+
  geom_line(lwd=1.1)+
  scale_color_manual(NULL, values=cbbPalette[1:7])+
  theme_bw()+
  ylab('Percent')+
  xlab('Year')+
  theme(legend.position = 'right', legend.direction = 'vertical')
wplot
jpeg('figures/PLOSobservedPlusPredicted.jpg', width=5, height=4, units='in', res=400, quality=100)
print(wplot)
dev.off()

## sample plot as above but without extrapolation
no.extrapolation = filter(pred, year<2019)
wplot = ggplot(data=results, aes(x=year, y=percent, col=weekday))+
  geom_line(data=no.extrapolation, aes(x=year, y=fit*100), col='black', lty=2)+
  geom_line(data=no.extrapolation, aes(x=year, y=week.fit*100), col='black', lty=2)+
  geom_line(lwd=1.1)+
  scale_color_manual(NULL, values=cbbPalette[1:7])+
  theme_bw()+
  ylab('Percent')+
  xlab('Year')+
  theme(legend.position = 'right', legend.direction = 'vertical')
wplot
jpeg('figures/PLOSobservedPlusPredictedNoExtrapolation.jpg', width=5, height=4, units='in', res=400, quality=100)
print(wplot)
dev.off()

# model with identity link?
# predict time at when weekends will hit weekdays

### Trend by weeks ###
# Binomial model of weeks, using log-link to give RR
m = glm(cbind(Yes, No) ~ I(time/52.17857), data=for.trend, family=binomial(link='log'))
summary(m)
exp(m$coefficients[2]) # relative risk per year (assuming 52.17857 week year = 365.25 / 7)

# plot weekends by week-to-week
for.plot = mutate(for.trend, percent = 100*Yes / (Yes+No),
                  date = earliest.date + time*7) # reconstruct date from weeks
tplot = ggplot(data=for.plot, aes(x=date, y=percent))+
  geom_line()+
  geom_smooth(col='skyblue', se = FALSE)+
  theme_bw()+
  scale_x_date(date_breaks='2 years')+
  ylab('Percent')+
  xlab('Time')
tplot
jpeg('figures/PLOSbyweeks.jpg', width=5, height=4, units='in', res=400, quality=100)
print(tplot)
dev.off()

# Linear model for power calculation (very similar to binomial model)
# using data that excludes outlying weeks
m = glm(percent ~ I(time/52.17857), data=for.plot, family=gaussian())
summary(m)
plot(for.plot$percent, predict(m)) # quick scatter plot
cor(for.plot$percent, predict(m))^2 # quick scatter plot

## Case-crossover model to look at relative change?
