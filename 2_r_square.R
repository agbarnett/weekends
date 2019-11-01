# 2_r_square.R 
# estimate the R-squared for the hour of day models
# October 2019

# get the chain estimates
index = grep('^mu\\[', rownames(bugs.results$summary))
cnames = rownames(bugs.results$summary)[index]
to.use1 = as.matrix(bugs.results$sims.array[,1,index]) # first chain
to.use2 = as.matrix(bugs.results$sims.array[,2,index]) # second chain
colnames(to.use1) = cnames
colnames(to.use2) = cnames
# switch from wide to long
long1 = data.frame(to.use1) %>%
  mutate(i = 1:n(), chain=1) %>%
  tidyr::gather(key='var', value='value', -i, -chain)
long2 = data.frame(to.use2) %>%
  mutate(i = 1:n(), chain=2) %>%
  tidyr::gather(key='var', value='value', -i, -chain)
# get fit statistics
fit.stats = bind_rows(long1, long2) %>%
  mutate(num = as.numeric(str_remove_all(string=var, pattern='[^0-9]'))) %>%
  group_by(num) %>%
  summarise(mean = mean(value), # summary stats
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975)) %>%
  ungroup() 

# R-squared
observed = bdata$count / exp(bdata$offset) # transform from count to probability
predicted = fit.stats$mean / exp(bdata$offset)
#plot(observed, predicted)
rsq = 100*cor(observed, predicted)^2
