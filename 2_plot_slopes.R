# 2_plot_slopes.R
# function to plot slopes (changes over time) from winbugs/jags model
# July 2019

# reference line for mean slope and 95% CI
xref = filter(ests, num==2, var=='beta') # do not back transform

# plot country-specific slopes
country.numbers = mutate(country.numbers, # create frame of country numbers; is in alphabetical order
                         country = as.character(Country),
                         num = 1:n())
to.plot = filter(ests, var=='slope') %>%
  left_join(country.numbers, by=c('num')) %>% # add country counts
  filter(Count >= 100) %>% # must have 100+ results
  arrange(mean) %>%
  mutate(xaxis = 1:n(), colour=1)
n.countries = length(unique(to.plot$country)) + 1 # plus one for mean

# add overall mean and CI
to.plot.mean = mutate(xref, country='Overall', colour=2, Count=0, xaxis=max(to.plot$xaxis)+1)
to.plot = bind_rows(to.plot, to.plot.mean)

# shading to help identify countries
shading = data.frame(xmin=seq(1, n.countries-1, 2)) %>%  # ignore mean so minus 1
  mutate(xmax = xmin+0.1,
         lower = min(to.plot$lower),
         upper = max(to.plot$upper) )

# plot
splot = ggplot(data=to.plot, aes(x=xaxis, y=mean, ymin=lower, ymax=upper, col=factor(colour))) +
  annotate('rect', xmin=shading$xmin, xmax=shading$xmax, ymin=shading$lower, ymax=shading$upper, col=grey(0.9), size=3) + # 
  geom_point()+
  scale_color_manual(NULL, values=c('black','blue'))+
  geom_errorbar(width=0)+
  scale_x_continuous(breaks=1:n.countries, labels=to.plot$country, limits = c(0, n.countries))+
  geom_hline(yintercept = xref$mean, lty=2)+ # Add reference line at overall mean
  xlab('')+
  ylab('Country-specific change over time')+
  theme_bw()+
  theme(legend.position = 'none',
         panel.grid.minor = element_blank(), axis.text.x = element_text(size=8),
        panel.grid.major.y = element_blank())+
  annotate('text', y=xref$mean, x=0, label='Steeper than average', hjust=-0.05, size=3)+
  annotate('text', y=xref$mean, x=0, label='Shallower than average', hjust=1.05, size=3)+
  coord_flip()

