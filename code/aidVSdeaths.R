#combine data sets for military aid and deaths, plot on the same graph
#have data from both form 1989 - 2015

#load data
library(readr)
TEaMA <- read_csv("~/data/Israel_Palestine_deaths_aid_analysis/datasets/TotalEconomicandMilitaryAssistance19462015.csv")
GED <- read_csv("~/data/Israel_Palestine_deaths_aid_analysis/datasets/ged171.csv")

#Create dataframe for only military aid
MA <- TEaMA[ ! TEaMA$`Assistance Category` %in% 'Economic', ]
#create dataframe for only aid given to Israel
Israel <- MA[ MA$Country %in% 'Israel',]
#aggregate aid by Year
Israel_AidByYear<-aggregate(Israel[8:9], by=list('Fiscal Year'=Israel$'Fiscal Year'), FUN=sum)

#create dataframe for only entries on Israeli-Palestinian conflict
IsPaliConflict <- GED[ GED$conflict_name %in% 'Israel:Palestine',]
#Create new dataframe with sum of total deaths per year (aggregate rows)
deaths<-aggregate(IsPaliConflict[36:40], by=list(year=IsPaliConflict$year), FUN=sum)

#standardize column name for ID, combine datasets
colnames(Israel_AidByYear)[which(names(Israel_AidByYear) == "Fiscal Year" )] <- "year"
aidVSdeeaths <- merge(Israel_AidByYear,deaths, by = "year")

#separate plots
plot(deaths_ts, col= "red", main = "Deaths", ylab = "Deaths")
plot(aid_ts, col= "red", main = "Aid", ylab = "Aid")

#scale aid amount to fit useful comparison in one plot
aidVSdeeaths$ConstantDollars2 <- aidVSdeeaths$ConstantDollars/10000000
#timeseries for deaths and aid in one plot
deaths_ts <- ts(aidVSdeeaths$deaths_b, start = 1989, end = 2015, frequency = 1)
aid_ts <- ts(aidVSdeeaths$ConstantDollars2, start = 1989, end = 2015, frequency = 1)
ts.plot(deaths_ts, aid_ts, gpars = list(), col=rainbow(8))
#deaths in red, aid in orange


