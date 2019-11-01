# 2_amplitude_CIs.R
# make credible intervals for amplitude for hour of day analysis
# October 2019

# get the chain estimates
index = grep('^cosine.c|^sine.c', rownames(bugs.results$summary))
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
binded = bind_rows(long1, long2) %>%
  mutate(country.num = as.numeric(str_remove_all(string=var, pattern='[^0-9]')), # remove all but numbers
         var = str_remove_all(string=var, pattern='[^a-zA-Z]')) %>%
  group_by(chain, i, country.num) %>%
  spread(var, value) %>% # cosine and sine on same row
  ungroup() %>%
  mutate(amp = sqrt(cosinec^2 + sinec^2)) # amplitude of sinusoid
# now get stats as probability ratios per country
amp.stats = group_by(binded, country.num) %>%
  summarise(meana = exp(mean(amp)),
            lowera = exp(quantile(amp, 0.025)),
            uppera = exp(quantile(amp, 0.975))) %>%
  ungroup() 
