# 1_plot_BMJ.R
# plot the BMJ data
# March 2019
load('data/BMJAnalysisReady.RData')
library(ggplot2)

# check hour of day
hour.histo = ggplot(data=filter(bmj, country=='Australia'), aes(x=hour)) +
  geom_histogram()+
  theme_bw()
hour.histo
