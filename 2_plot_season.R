# 2_plot_season.R
# function to plot sinusoidal seasonal estimate from winbugs model (results from 2_bugs.model.R)
# July 2019
library(ggrepel) # for geom_text_repel

# only plot countries with this number of results, vary by journal because there's more crowding with BMJ Open
n.limit = ifelse(which.journal=='BMJ', 100, 200)

# breaks for months (labels) on axis
dates = as.Date(paste('2000-', 1:12, '-01', sep=''))
breaks = yrfraction(dates) *2*pi # breaks in radians
months = substr(month.abb, 1,1)

## a) line plot of average season
cos.est = filter(ests, var=='beta', num==3)$mean
sin.est = filter(ests, var=='beta', num==4)$mean
frame = data.frame(time = seq(0, 2*pi, length.out=40)) %>%
  mutate(y = cos.est*cos(time) + sin.est*sin(time) )
# plot
mplot = ggplot(data=frame, aes(x=time, y=exp(y))) + # scale to odds ratio
  geom_line(size=1)+
  xlab('Month')+
  ylab('Seasonal change in odds ratio')+
  scale_x_continuous(limits=c(0,2*pi), breaks=breaks, labels=months)+
  geom_hline(yintercept = 1, lty=2)+ # Add reference line at 1
  g.theme +
  theme(text = element_text(size=14), 
    panel.grid.major.y = element_blank())

# b) plot countries
country.numbers = mutate(country.numbers, # create frame of country numbers; is in alphabetical order
                         country = as.character(Country),
                         num = 1:n())
to.plot = filter(ests, var %in% c("beta.cos","beta.sin")) %>%
  left_join(country.numbers, by=c('num')) %>% # add country counts
  filter(Count >= n.limit) %>% # must have 100+ or 200+ results
  arrange(mean)
# spread to wide for scatter (cos and sin on same row)
wide = dplyr::select(to.plot, -lower, -upper) %>%
  tidyr::spread(var, mean) %>%
  mutate(amp = sqrt(beta.cos^2 + beta.sin^2),
         phase = NA, 
         colour=1)

# add overall
to.plot.mean = data.frame(colour=2, num=max(wide$num)+1, country='Overall', beta.cos=cos.est, beta.sin=sin.est, stringsAsFactors = FALSE) %>%
  mutate(amp = sqrt(beta.cos^2 + beta.sin^2),
         phase = NA)
wide = bind_rows(wide, to.plot.mean)

# loop to calculate phase
for (k in 1:nrow(wide)){
  wide$phase[k] = phasecalc(wide$beta.cos[k], wide$beta.sin[k])
}
# plot
seasonplot = ggplot(data=wide, aes(x=phase, y=amp, label=country, col=factor(colour))) + # logit scale
  geom_point()+
  scale_color_manual(NULL, values=c('black','blue'))+
  xlab('Month')+
  ylab('Amplitude, logit scale')+
  scale_x_continuous(limits=c(0,2*pi), breaks=breaks, labels=months)+
  geom_text_repel()+
  g.theme +
  theme(legend.position = 'none',
        text = element_text(size=14),
        panel.grid.major.y = element_blank())+
  coord_polar()

