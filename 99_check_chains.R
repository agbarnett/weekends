# 99_check_chains.R
# check chains from JAGS and WinBUGS for best models
# Oct 2019
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())

## Section 1: JAGS models ##

# a) Get the DIC to find best model
to.load = dir('Z:/weekend', pattern='results')
DIC = NULL
for (f in to.load){
  load(paste('Z:/weekend/', f, sep='')  )
  DIC = bind_rows(DIC, dic.frame)
}
# 
best = group_by(DIC, journal, outcome, data) %>%
  arrange(journal, outcome, data, DIC) %>%
  slice(1) %>%
  ungroup()

# b) now get the chains and make the nice plot
keep.plots = list()
for (fnum in 1:nrow(best)){
  # extract details from best model
  which.journal = best$journal[fnum]
  which.outcome = best$outcome[fnum]
  which.data.text = best$data[fnum]
  which.season = best$season[fnum]
  which.season = ifelse(is.na(which.season), FALSE, which.season) # replace missing
  season.text = ifelse(which.season==TRUE, 'Season', 'NoSeason')
  type = best$model[fnum]
  jags.filename.chains = paste('Z:/weekend/JAGS.', which.outcome, '.', which.data.text, '.', which.journal, '.',
                                  type, '.', season.text, '.chains.RData', sep='')
  load(jags.filename.chains)
  
  # extract chains
  n.samples = dim(codaSamples[[1]])[1]
  n.vars = dim(codaSamples[[1]])[2]
  chain1 = as.data.frame(as.matrix(codaSamples[[1]][,1:n.vars])) %>%
    gather() %>%
    mutate(iter = rep(1:(n.samples), n.vars), chain=1)
  chain2 = as.data.frame(as.matrix(codaSamples[[2]][,1:n.vars])) %>%
    gather() %>%
    mutate(iter = rep(1:(n.samples), n.vars), chain=2)
  chains = bind_rows(chain1, chain2) %>% 
    mutate(num = as.numeric(gsub(x=key, pattern = '[a-z]|\\[|\\.|\\]', replacement = '')),
           var = gsub(x=key, pattern = '[0-9]|\\[|\\]', replacement = ''))
  
  ## make chain checks for appendix, just overall intercept and slope
  to.plot = filter(chains, var=='beta')
  cplot = ggplot(data=to.plot, aes(x=iter, y=value, col=factor(chain)))+
    geom_line(lty=2)+
    theme_bw()+
    xlab('Iteration')+
    ylab('Estimate')+
    scale_color_manual('Chain', values=c('dark red','skyblue'))+
    facet_wrap(~key, scales = 'free_y')

  ## store plot in a list
  this.title = paste(which.journal, ', ', which.outcome, ', ', which.data.text, sep='')
  keep.plots[[fnum]] = cplot + theme(legend.position = 'none') + # remove legend for combined plot
    ggtitle(this.title) # add title
}
 
# 12 to export, so two plots of 2x3; A4 size
jpeg('figures/CheckChains1.jpg', width=8.27, height=11.69, units='in', res=400, quality = 100)
gridExtra::grid.arrange(keep.plots[[1]], keep.plots[[2]], keep.plots[[3]],
                        keep.plots[[4]], keep.plots[[5]], keep.plots[[6]], ncol=2)
dev.off()
jpeg('figures/CheckChains2.jpg', width=8.27, height=11.69, units='in', res=400, quality = 100)
gridExtra::grid.arrange(keep.plots[[7]], keep.plots[[8]], keep.plots[[9]],
                        keep.plots[[10]], keep.plots[[11]], keep.plots[[12]], ncol=2)
dev.off()

## Section 2: Winbugs models ##

# a) submissions
load('data/bugs.results.country.difference.Submissions.RData') # from 2_country_difference_from_average.R
# get the chain estimates for the intercept and random effect inverse-variance
index = grep('intercept|tau.beta', rownames(bugs.results$summary))
cnames = rownames(bugs.results$summary)[index]
to.use1 = as.matrix(bugs.results$sims.array[,1,index]) # first chain
to.use2 = as.matrix(bugs.results$sims.array[,2,index]) # second chain
colnames(to.use1) = cnames
colnames(to.use2) = cnames
# switch from wide to long
long1 = data.frame(to.use1) %>%
  mutate(iter = 1:n(), chain=1) %>%
  tidyr::gather(key='var', value='value', -iter, -chain)
long2 = data.frame(to.use2) %>%
  mutate(iter = 1:n(), chain=2) %>%
  tidyr::gather(key='var', value='value', -iter, -chain)
to.plot = bind_rows(long1, long2) %>%
  mutate(var = str_remove_all(string=var, pattern='[^a-zA-Z]'))
cplot.submissions = ggplot(data=to.plot, aes(x=iter, y=value, col=factor(chain)))+
  geom_line(lty=2)+
  theme_bw()+
  xlab('Iteration')+
  ylab('Estimate')+
  scale_color_manual('Chain', values=c('dark red','skyblue'))+
  ggtitle('Submissions')+
  facet_wrap(~var, scales = 'free_y')

# a) submissions
load('data/bugs.results.country.difference.Reviews.RData') # from 2_country_difference_from_average.R
# get the chain estimates for the intercept and random effect inverse-variance
index = grep('intercept|tau.beta', rownames(bugs.results$summary))
cnames = rownames(bugs.results$summary)[index]
to.use1 = as.matrix(bugs.results$sims.array[,1,index]) # first chain
to.use2 = as.matrix(bugs.results$sims.array[,2,index]) # second chain
colnames(to.use1) = cnames
colnames(to.use2) = cnames
# switch from wide to long
long1 = data.frame(to.use1) %>%
  mutate(iter = 1:n(), chain=1) %>%
  tidyr::gather(key='var', value='value', -iter, -chain)
long2 = data.frame(to.use2) %>%
  mutate(iter = 1:n(), chain=2) %>%
  tidyr::gather(key='var', value='value', -iter, -chain)
to.plot = bind_rows(long1, long2) %>%
  mutate(var = str_remove_all(string=var, pattern='[^a-zA-Z]'))
cplot.reviews = ggplot(data=to.plot, aes(x=iter, y=value, col=factor(chain)))+
  geom_line(lty=2)+
  theme_bw()+
  xlab('Iteration')+
  ylab('Estimate')+
  scale_color_manual('Chain', values=c('dark red','skyblue'))+
  ggtitle('Peer reviews')+
  facet_wrap(~var, scales = 'free_y')

# export
jpeg('figures/CheckChains3.jpg', width=8, height=8, units='in', res=400, quality = 100)
gridExtra::grid.arrange(cplot.submissions, cplot.reviews, nrow=2)
dev.off()
