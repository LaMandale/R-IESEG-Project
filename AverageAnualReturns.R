DATA = read.csv("MVEMAG-holdings.csv")

#Download Packages
install.packages("plyr")
install.packages("ggplot2")
library(ggplot2)
library(plyr)
#__________________________________DAILY
#Set the Data Frame
df <- data.frame(TR = DATA$Daily.TR)

#Call the library and Run the function
TRDaily = ddply(df, .(TR), nrow)

#Average of Daily returns
AvgDailyReturns <- mean(TRDaily$TR[1]:TRDaily$TR[3559])
print(AvgDailyReturns)

#__________________________________MONTHLY
#Set the Data Frame
df <- data.frame(TRM = DATA$Month.to.Date.Return)

#Call the library and Run the function
TRMonthly = ddply(df, .(TRM), nrow)

#Average of Daily returns
AvgMonthlyReturns <- mean(TRMonthly$TR[1]:TRMonthly$TR[3559])
print(AvgMonthlyReturns)

#__________________________________YEAR

#Set the Data Frame
df <- data.frame(AOT = DATA$Accrued.Interest.t)
TRYearT = ddply(df, .(AOT), nrow)
df <- data.frame(AOT1 = DATA$AccruedInterest.t.1)
TRYearT1 = ddply(df, .(AOT1), nrow)

YearDifference = (TRYearT$AOT-TRYearT1$AOT1)/TRYearT$AOT
mean(YearDifference[1]:YearDifference[2810])