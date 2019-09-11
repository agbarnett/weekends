# 0_address_edits.R
# function to make common address edits
# May 2019
library(Hmisc) # for capitalize
library(stringr)
library(forcats)

address.edits = function(indata){
  # strings to cut
  strings.to.remove = tolower(c('...','-----','---','--','\\\\','Other / none','Other ->','\\(state\\)','\\?\\?','^--$',
                                '-- select state --','--select state--','-- select a state --','-- select country --','-- select county --',
                                '-- select a state/province --','-- select city --','-- please select --','- none -','Select county',
                                '-- none --','Please choose','Please choose a state','N/A = not applicable','--- Select One ---',
                                'State/province (if applicable)','None selected','Not applicable (outside of us)','Not applicable','Select','N/A','None','Patient reviewer',
                                'Not Listed or Not Applicable','-please select-','please select','[select a state]','Please select (only u.s. / can / aus)','Non-us/non-canadian',
                                'Outside us/canada/australia','Outside u.s./canada','-- select a state/province --','Non-us/canada',
                                'Please select state','select one','select...','select one...','Select state/province','Select a state/province',
                                'select state','select a state'))
  strings.to.remove = unique(strings.to.remove) # just in case of doubles
  strings.to.remove = strings.to.remove[order(1/nchar(strings.to.remove))] # from longest to shortest
  # 
  outdata = mutate(indata,
  # change country names to match Google
  country = fct_recode(  
    country,
    'United Kingdom' = 'United Kingdom of Great Britain and Northern Ireland',
    'China' = 'Hong Kong',
    'Iran' = 'Iran (the Islamic Republic of)',
    'Iran' = 'Iran, Islamic Republic of',
    'Republic of Korea' = 'Korea, Republic of',
    'Republic of Korea' = 'Korea (the Republic of)'),
  country = as.character(country),
  # clear up some addresses
  state = ifelse(nchar(state)==1, NA, state), # remove few odd single letters/characters
  city = ifelse(nchar(city)==1, NA, city), # 
  country = ifelse(nchar(country)==1, NA, country), # 
  state = ifelse(tolower(state) %in% strings.to.remove, NA, state),                   
  city = ifelse(tolower(city) %in% strings.to.remove, NA, city),                   
  country = ifelse(tolower(country) %in% strings.to.remove, NA, country), 
  # remove double spaces
  city = str_replace_all(string=city, pattern='  ', replacement=' '),
  state = str_replace_all(string=state, pattern='  ', replacement=' '),
  country = str_replace_all(string=country, pattern='  ', replacement=' '),
  # capitalise first letter
  city = capitalize(tolower(city)),
  state = capitalize(tolower(state)),
  country = capitalize(tolower(country))
  ) # end of mutate
  return(outdata)
}

