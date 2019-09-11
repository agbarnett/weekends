# 99_country_holidays.R
# Make file of country-specific holidays
# abandoned in favour of https://github.com/tinohager/Nager.Date
# June 2019
library(timeDate) # for holidays

# a) Easter and Xmas, common to all
easter1 = as.Date(holiday(year = 2012:2018, Holiday = "GoodFriday"))
easter2 = as.Date(holiday(year = 2012:2018, Holiday = "Easter"))
easter3 = as.Date(holiday(year = 2012:2018, Holiday = "EasterSunday"))
easter4 = as.Date(holiday(year = 2012:2018, Holiday = "EasterMonday"))
xmas1 = as.Date(holiday(year = 2012:2018, Holiday = "ChristmasEve"))
xmas2 = as.Date(holiday(year = 2012:2018, Holiday = "ChristmasDay"))
xmas3 = as.Date(holiday(year = 2012:2018, Holiday = "BoxingDay"))
xmas4 = as.Date(holiday(year = 2012:2019, Holiday = "NewYearsDay")) # kept to 2019 because a small number of papers drift over into 2019 by one day because of time zone difference

# b) country-specific holidays
holiday.frame = read.table(header = TRUE, sep = ',', stringsAsFactors = FALSE, text = '
country,holiday
Canada,CAVictoriaDay
Canada,CACanadaDay
Canada,CACivicProvincialHoliday
Canada,CALabourDay
Canada,CAThanksgivingDay
Canada,CaRemembranceDay. 
USA,USNewYearsDay
USA,USInaugurationDay
USA,USMLKingsBirthday
USA,USLincolnsBirthday
USA,USWashingtonsBirthday
USA,USMemorialDay
USA,USIndependenceDay
USA,USLaborDay
USA,USColumbusDay
USA,USElectionDay
USA,USVeteransDay
USA,USThanksgivingDay
USA,USChristmasDay
USA,USCPulaskisBirthday
USA,USGoodFriday
UK,GBMayDay
UK,GBBankHoliday
UK,GBSummerBankHoliday
Germany,DEAscension
Germany,DECorpusChristi
Germany,DEGermanUnity
Germany,DEChristmasEve
Germany,DENewYearsEve
France,FRFetDeLaVictoire1945
France,FRAscension
France,FRBastilleDay
France,FRAssumptionVirginMary
France,FRAllSaints
France,FRArmisticeDay
Italy,ITEpiphany
Italy,ITLiberationDay
Italy,ITAssumptionOfVirginMary
Italy,ITAllSaints
Italy,ITStAmrose
Italy,ITImmaculateConception
Japan,JPNewYearsDay
Japan,JPGantan
Japan,JPBankHolidayJan2
Japan,JPBankHolidayJan3
Japan,JPComingOfAgeDay
Japan,JPSeijinNoHi
Japan,JPNatFoundationDay
Japan,JPKenkokuKinenNoHi
Japan,JPGreeneryDay
Japan,JPMidoriNoHi
Japan,JPConstitutionDay
Japan,JPKenpouKinenBi
Japan,JPNationHoliday
Japan,JPKokuminNoKyujitu
Japan,JPChildrensDay
Japan,JPKodomoNoHi
Japan,JPMarineDay
Japan,JPUmiNoHi
Japan,JPRespectForTheAgedDay
Japan,JPAutumnalEquinox
Japan,JPHealthandSportsDay
Japan,JPTaiikuNoHi
Japan,JPNationalCultureDay
Japan,JPBunkaNoHi
Japan,JPThanksgivingDay
Japan,JPEmperorsBirthday
Switzerland,CHBerchtoldsDay
Switzerland,CHSechselaeuten
Switzerland,CHAscension
Switzerland,CHConfederationDay
Switzerland,CHKnabenschiessen'
)

# save
save(holidays, file='data/holidays.RData')