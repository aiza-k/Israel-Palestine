#load data
library(readr)
TEaMA <- read_csv("~/data/Israel_Palestine_deaths_aid_analysis/datasets/TotalEconomicandMilitaryAssistance19462015.csv")

#Create dataframe for only military aid
MA <- TEaMA[ ! TEaMA$`Assistance Category` %in% 'Economic', ]

#create dataframe for only aid given to Israel
Israel <- MA[ MA$Country %in% 'Israel',]

table(Israel$Region)
table(Israel$Country)
#country and region are obviously the same for all entries
#some differences in Publication Row, Funding Agency and Funding Account Names
table(Israel$`Publication Row`)
table(Israel$`Funding Agency`)
table(Israel$`Funding Account Name`)

#aggregate aid by Year
Israel_AidByYear<-aggregate(Israel[8:9], by=list('Fiscal Year'=Israel$'Fiscal Year'), FUN=sum)

#Miliary aid in Constant Dollars vs Fiscal Year
plot(ConstantDollars ~ `Fiscal Year`, Israel_AidByYear)
model <- lm(ConstantDollars ~ `Fiscal Year`, Israel_AidByYear)
abline(model, col = 'blue')
model

#Miliary aid in Historical Dollars vs Fiscal Year
plot(HistoricalDollars ~ `Fiscal Year`, Israel_AidByYear)
model2 <- lm(HistoricalDollars ~ `Fiscal Year`, Israel_AidByYear)
abline(model2, col = 'blue')
model2


#check USmil aid and deaths in and around 2000-2005, 2008, and 2014
