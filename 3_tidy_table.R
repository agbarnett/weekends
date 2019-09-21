# 3_tidy_table.R
# function to tidy table for results from winbugs/jags, called by 2_analysis.Rmd
# July 2019

tidy.table = function(indata){
  to.table = filter(indata, var %in% c('beta','p','diff','ratio')) %>%
    mutate(mean = round(mean*1000)/1000,
           CI = paste(round(lower*1000)/1000, ', ', round(upper*1000)/1000, sep=''),
           variable = var,
           variable = ifelse(var=='beta' & num==1, "Intercept, logit scale", variable),
           variable = ifelse(var=='beta' & num==2, "Change, logit scale", variable),
           variable = ifelse(var=='beta' & num==3, "Cos season, logit scale", variable),
           variable = ifelse(var=='beta' & num==4, "Sin season, logit scale", variable),
           variable = ifelse(var=='p' & num==1, "Probability, 2012", variable),
           variable = ifelse(var=='p' & num==2, "Probability, 2013", variable),
           variable = ifelse(var=='diff' , "Difference", variable),
           variable = ifelse(var=='ratio', "Ratio", variable),
           row.order = 1,
           row.order = ifelse(var=='beta' & num==1, 1, row.order),
           row.order = ifelse(var=='beta' & num==2, 2, row.order),
           row.order = ifelse(var=='beta' & num==3, 3, row.order),
           row.order = ifelse(var=='beta' & num==4, 4, row.order),
           row.order = ifelse(var=='p' & num==1, 5, row.order),
           row.order = ifelse(var=='p' & num==2, 6, row.order),
           row.order = ifelse(var=='diff' , 7, row.order),
           row.order = ifelse(var=='ratio', 8, row.order)
    ) %>%
    arrange(row.order) %>%
    dplyr::select(variable, mean, CI) %>%
    rename('Mean'='mean',
           '95% CI' = 'CI')
  return(to.table)
}