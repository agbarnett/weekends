# 99_plot_for_paper.R
# plot of intercepts for paper from BMJ and BMJ Open
# Sep 2019
library(dplyr)
library(ggplot2)
library(gridExtra)
library(boot)
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())

# BMJ
jags.filename.results = 'JAGS.weekend.submissions.BMJ.intercept.Season.results.RData'
load(paste('Z:/weekend/', jags.filename.results, sep='')) # from process.jags.R on HPC
source('2_plot_intercepts.R')
plot1 = iplot
plot1 = plot1 + ggtitle("BMJ") + theme(axis.text.x = element_text(size=9)) # bigger text for BMJ

# BMJ Open
jags.filename.results = 'JAGS.weekend.submissions.BMJ Open.intercept.Season.results.RData'
load(paste('Z:/weekend/', jags.filename.results, sep='')) # from process.jags.R on HPC
source('2_plot_intercepts.R')
plot2 = iplot
plot2 = plot2 + ggtitle("BMJ Open") + theme(axis.text.x = element_text(size=9))

#jpeg('figures/Intercepts.jpg', width=8.2, height=4, units='in', res=400, quality = 100)
# dimensions for BMJ
jpeg('figures/BarnettFig2.jpg', width=10, height=5, units='in', res=400, quality = 100)
gridExtra::grid.arrange(plot1, plot2, ncol=2)
dev.off()

