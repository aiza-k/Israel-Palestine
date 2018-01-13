#load data
library(readr)
GED <- read_csv("~/data/Israel_Palestine_deaths_aid_analysis/datasets/ged171.csv")

#create dataframe for only entries on Israeli-Palestinian conflict
IsPaliConflict <- GED[ GED$conflict_name %in% 'Israel:Palestine',]

#sum of total deaths (all years)
sum(IsPaliConflict$best)
sum(IsPaliConflict$deaths_civilians)
sum(IsPaliConflict$deaths_a)
sum(IsPaliConflict$deaths_b)

#Create new dataframe with sum of total deaths per year (aggregate rows)
deaths<-aggregate(IsPaliConflict[36:40], by=list(year=IsPaliConflict$year), FUN=sum)
#Check these sums are the same as those for IsPaliConflict dataset
sum(deaths$best)
sum(deaths$deaths_civilians)
sum(deaths$deaths_a)
sum(deaths$deaths_b)

#-----------------------------------------------------
#best estimates of death by year
#mean number of deaths = 225.82, max is 1679 in 2014. min in 1997.
summary(deaths$best)
#plot is moderately strong, positive-ish. 
#Spike in 2014 (Operation Protective Edge launched by Israel), 2008-2009 
#(Gaza War), and 2000-2005(second intifada)
plot(best ~ Year, deaths, col="red", main = "Best Estimate of All Deaths in the 
     Israeli-Palestinian Conflict by Year", ylab= "Deaths")
bets <- ts(deaths$best, start = 1989, end = 2016, frequency = 1)
plot(bets, col= "red", main = "Best Estimate of All Deaths", ylab = "Deaths")

#linear model of best estimate of deaths by year
#hard to fit linear model, as spike not consistent
#however, The slope is 14.9. This implies that for every 1 year increase in time,
#the predicted best estimate of deaths willincreases by 14.9 deaths.
model <- lm(best ~ year, deaths)
abline(model, col = "blue")
model

#-----------------------------------------------------
#civilian deaths by year
#mean number of deaths = 38.96, max is 307 in 2014. some years with no civilian deaths
summary(deaths$deaths_civilians)
#plot is moderately strong, positive-ish. Spikes as before,
plot(deaths_civilians ~ Year, deaths, col="red", main = "Civilian Deaths by Year", 
     ylab= "Civilian Deaths")
cts <- ts(deaths$deaths_civilians, start = 1989, end = 2016, frequency = 1)
plot(cts, col= "red", main = "Civilian Deaths", ylab = "Civilian Deaths")

#-----------------------------------------------------
#Side A deaths (government of Israel)
table(IsPaliConflict$side_a)
#median number of deaths = 14.32, max is 94 in second intifada. 
summary(deaths$deaths_a)
#plot is moderately strong, positive-ish. also spikes in intifada & 2014, but not 
#in 08 or 09
plot(deaths_a ~ Year, deaths, col="red", main = "Government of Israel Deaths by Year", 
     ylab= "Government of Israel Deaths")
ats <- ts(deaths$deaths_a, start = 1989, end = 2016, frequency = 1)
plot(ats, col= "red", main = "Goverment of Israel Deaths", ylab = "Deaths")

#-----------------------------------------------------
#Side B deaths - include variety of actors however most are Hamas
table(IsPaliConflict$side_b)
#median number of deaths = 95.43, max is 407 in 2008.
summary(deaths$deaths_b)
#plot is weak. spikes in second intifada, but also 2006-2008. only a slight spike in 
#2014.
plot(deaths_b ~ Year, deaths, col="red", main = "Deaths to Sides Opposing Israel by 
     Year", ylab= "Side B Deaths")
bts <- ts(deaths$deaths_b, start = 1989, end = 2016, frequency = 1)
plot(bts, col= "red", main = "Deaths Among Groups Opposing Israel", ylab = "Deaths")

#-----------------------------------------------------
#-----------------------------------------------------
#Deaths data without aggregating by year
plot(best ~ date_start, IsPaliConflict, col="red", main = "Best Estimate of All Deaths in the 
     Israeli-Palestinian Conflict", ylab= "Deaths")

#thanks to F.Begum for help with the code!
