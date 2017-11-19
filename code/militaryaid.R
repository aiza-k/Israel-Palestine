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

#Min aid in 1961 of 6.227e+03, max in 1979 of 1.099e+10, mean of 2.635e+09.
summary(Israel_AidByYear$ConstantDollars)
which.max(Israel_AidByYear$ConstantDollars)
which.min(Israel_AidByYear$ConstantDollars)
#plot is moderately strong and positive with some outliers
plot(ConstantDollars ~ `Fiscal Year`, Israel_AidByYear)
#The slope is 3.941e+07. 
#This implies that for every 1 year increase in time, the predicted total military aid to Israel willincreases by 3.941e+07 dollars.
model <- lm(ConstantDollars ~ `Fiscal Year`, Israel_AidByYear)
abline(model, col = 'blue')
model
#outliers:
#there are outliers in the years 1974, 1976, and 1979
Outliers_CD <- Israel_AidByYear[Israel_AidByYear$ConstantDollars > 4e+09,] 
View(Outliers_CD)


#Miliary aid in Historical Dollars vs Fiscal Year

#Min aid in 1961 of 1.000e+03, max in 1979 of 4.000e+09, mean of 1.574e+09
summary(Israel_AidByYear$HistoricalDollars)
which.max(Israel_AidByYear$HistoricalDollars)
which.min(Israel_AidByYear$HistoricalDollars)
#plot is strong and positive with some outliers
plot(HistoricalDollars ~ `Fiscal Year`, Israel_AidByYear)
#The slope is 3.941e+07. 
#This implies that for every 1 year increase in time, the predicted total military aid to Israel willincreases by 3.941e+07 dollars.
model2 <- lm(HistoricalDollars ~ `Fiscal Year`, Israel_AidByYear)
abline(model2, col = 'blue')
model2
#outliers:
#there are outliers in the years 1974 and 1979

#Try converting outliers to missing, then find max, min, median and model
#check USmil aid and deaths in and around 2000-2005, 2008, and 2014
#also check what, if anything, happened in outlier years to generate more aid?
