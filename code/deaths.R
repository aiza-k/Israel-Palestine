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

#Create new dataframe with sum of total deaths per year
#Best estimate of all deaths per year (check if this value is same as sum from orginal data set.)
bestdeaths<-aggregate(IsPaliConflict$best, by=list(Category=IsPaliConflict$year), FUN=sum)
colnames(bestdeaths)<-c("Year", "Sum of Best Estimate of Deaths")
bestdeathsfinal <- bestdeaths[order(bestdeaths$Year),] 
sum(bestdeaths$'Sum of Best Estimate of Deaths')
#Civilian deaths per year
civdeaths<-aggregate(IsPaliConflict$deaths_civilians, by=list(Category=IsPaliConflict$year), FUN=sum)
colnames(civdeaths)<-c("Year", "Sum of Civilian Deaths")
civdeathsfinal <- civdeaths[order(civdeaths$Year),] 
sum(civdeaths$'Sum of Civilian Deaths')

#Check specific years of flare ups in conflict, check USmil aid and deaths in and around those years
#2000-2005 second intifada
#check approx 2015?
#thanks to F.Begum for help with the code!