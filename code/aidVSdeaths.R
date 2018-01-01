#load data
library(readr)
TEaMA <- read_csv("~/data/Israel_Palestine_deaths_aid_analysis/datasets/TotalEconomicandMilitaryAssistance19462015.csv")
GED <- read_csv("~/data/Israel_Palestine_deaths_aid_analysis/datasets/ged171.csv")

#Create dataframe for only military aid
MA <- TEaMA[ ! TEaMA$`Assistance Category` %in% 'Economic', ]
#create dataframe for only aid given to Israel
Israel <- MA[ MA$Country %in% 'Israel',]
#aggregate aid by Year
Israel_AidByYear<-aggregate(Israel[8:9], by=list('Fiscal Year'=Israel$'Fiscal Year'), 
                            FUN=sum)

#create dataframe for only entries on Israeli-Palestinian conflict
IsPaliConflict <- GED[ GED$conflict_name %in% 'Israel:Palestine',]
#Create new dataframe with sum of total deaths per year (aggregate rows)
deaths<-aggregate(IsPaliConflict[36:40], by=list(year=IsPaliConflict$year), FUN=sum)

#standardize column name for ID, combine datasets
colnames(Israel_AidByYear)[which(names(Israel_AidByYear) == "Fiscal Year" )] <- "year"
aidVSdeaths <- merge(Israel_AidByYear,deaths, by = "year")

#plot on separate Y axis
par(mar = c(5,5,2,5))
plot(best ~ year, aidVSdeaths, type = "l", col="red",
             ylab="Deaths")
par(new = T)
with(aidVSdeaths, plot(year, ConstantDollars, type = "l", axes=F, col = "blue",
                        xlab=NA, ylab=NA))
axis(side = 4)
mtext(side = 4, line = 3, "Aid")
legend("topleft",legend=c("Deaths", "Aid"), col=c("red", "blue"), lty=1:1)
