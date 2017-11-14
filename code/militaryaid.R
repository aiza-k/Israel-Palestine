#load data
library(readr)
TEaMA <- read_csv("~/data/Israel_Palestine_deaths_aid_analysis/datasets/TotalEconomicandMilitaryAssistance19462015.csv")

#Create dataframe for only military aid
MA <- TEaMA[ ! TEaMA$`Assistance Category` %in% 'Economic', ]

#create dataframe for only aid given to Israel
Israel <- MA[ MA$Country %in% 'Israel',]

#Miliary aid in Constant Dollars vs Fiscal Year, and linear model
plot(ConstantDollars ~ `Fiscal Year`, Israel)
model <- lm(ConstantDollars ~ `Fiscal Year`, Israel)
abline(model, col = 'blue')
model

#Miliary aid in Historical Dollars vs Fiscal Year, and linear model
plot(HistoricalDollars ~ `Fiscal Year`, Israel)
model2 <- lm(HistoricalDollars ~ `Fiscal Year`, Israel)
abline(model2, col = 'blue')
model2

#Military aid from Funding Account - Foreign Military Financing Program 
Israel_FMFP <- Israel[ Israel$`Funding Account Name` %in% 'Foreign Military Financing Program',]
plot(ConstantDollars ~ `Fiscal Year`, Israel_FMFP)
model3 <- lm(ConstantDollars ~ `Fiscal Year`, Israel_FMFP)
abline(model3, col = 'blue')
model3
#confidence interval
confint(model3, level = 0.95)

#Military aid from Funding Account - Excess Defense Articles 
Israel_EDA <- Israel[ Israel$`Funding Account Name` %in% 'Excess Defense Articles',]
plot(ConstantDollars ~ `Fiscal Year`, Israel_EDA)
model4 <- lm(ConstantDollars ~ `Fiscal Year`, Israel_EDA)
abline(model4, col = 'blue')
model4
#confidence interval
confint(model4, level = 0.95)
