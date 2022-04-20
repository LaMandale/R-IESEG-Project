DATA = read.csv("MVEMAG-holdings.csv")

#Download the Packages and Call the Libraries
install.packages("plyr")
install.packages("ggplot2")
library(ggplot2) # We will use ggplot2 for Data Visualization
library(plyr) # We will use plyr to split and  combine data
library(basictabler) # We will use basictabler for tables & Data Interpretation

#Dear Professor, we recommend you to run individually each part as the table constructions and calculations results take long to load.
#Each part is divided by a #--[Box]--# 
#Best Regards, 
# Constance Marechal, Younes Bellahcem, Marc Kochmanski-Chmura

#---------[START OF CURRENCY DIVERSIFICATION]----------------#

#Set the Data Frame
dfCUR <- data.frame(CurrencyCount = DATA$Coupon.Currency)
RCurrency = ddply(dfCUR, .(CurrencyCount), nrow)

#Calculation of the Total Number of objects to get the proportions later.
RCurrencyTotal <- sum(RCurrency$V1)

#Proportions Calculations, we want to know how often a currency is used to emit a bond.
RCurrencyProportions <- round((RCurrency$V1/RCurrencyTotal)*100, digits=2)

# Data for the table, we want to keep only the relevant values and categorize all the other currency into an "Other" Category.
  #These are "source values" (coming from the database)
CurrencyDiv <- RCurrency$CurrencyCount
CurrencyNumbers <- RCurrency$V1
CurrencyPro <- RCurrencyProportions
  #Here we create new variables to keep only important information with highest values ( Usd, Eur, Myr ,[...], Other)
fibseq <- c(CurrencyDiv[29], CurrencyDiv[26], CurrencyDiv[8], CurrencyDiv[13], CurrencyDiv[18], "Other")
fibseq2 <- c(CurrencyNumbers[29], CurrencyNumbers[26], CurrencyNumbers[8], CurrencyNumbers[13], CurrencyNumbers[18], RCurrencyTotal - sumfibseg2)
fibseq3 <- c(CurrencyPro[29], CurrencyPro[26], CurrencyPro[8], CurrencyPro[13], CurrencyPro[18], 100 - sumfibseg3)
sumfibseg2 <- sum(c(CurrencyNumbers[29], CurrencyNumbers[26], CurrencyNumbers[8], CurrencyNumbers[13], CurrencyNumbers[18]))
sumfibseg3 <- sum(c(CurrencyPro[29], CurrencyPro[26], CurrencyPro[8], CurrencyPro[13], CurrencyPro[18]))

#Table Construction of freshly created variables after the filtering. 
tbl <- BasicTable$new()
tbl$cells$setCell(1, 1, cellType="root", rawValue="Currency Diversification")
tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="N")
tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="%")
tbl$cells$setColumn(1, cellTypes="rowHeader", rawValues=fibseq)
tbl$cells$setColumn(2, cellTypes="cell", rawValues=fibseq2)
tbl$cells$setColumn(3, cellTypes="cell", rawValues=fibseq3)
formats=list("%.2f"))
tbl$renderTable()

# Pie Chart : Define the variables to select. 
slices <- fibseq2
lbls <- fibseq
pct <- fibseq3
# Pie Chart : Define the labels.
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
# Pie Chart construction 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Currency Diversification")

#---------[END OF CURRENCY DIVERSIFICATION]---------#
#                                                   #
#                                                   #
#                                                   #
#---------[START OF CREDIT QUALITY]-----------------#

#Set the Data Frame
df <- data.frame(RankingsCount = DATA$Ranking)
RC = ddply(df, .(RankingsCount), nrow)

#Calculation of the Total Number of Credits to get the proportions later.
RCTotal <- sum(RC$V1)

#Proportions Calculations, we want to know how many bonds are in the Data Frame depending on their Credit Rating.
RCProportions <- round((RC$V1/RCTotal)*100, digits=2)

# Variables Definition for the table
Rankings <- RC$RankingsCount
Numbers <- RC$V1
Pro <- RCProportions

#Table Construction of freshly created variables after the filtering. 
tbl <- BasicTable$new()
tbl$cells$setCell(1, 1, cellType="root", rawValue="Credit Quality")
tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="N")
tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="%")
tbl$cells$setColumn(1, cellTypes="rowHeader", rawValues=Rankings)
tbl$cells$setColumn(2, cellTypes="cell", rawValues=Numbers)
tbl$cells$setColumn(3, cellTypes="cell", rawValues=Pro)
formats=list("%.2f"))
tbl$renderTable()

# Pie Chart : Define the variables to select. 
slices <- Numbers
lbls <- Rankings
pct <- Pro
# Pie Chart : Define the labels.
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
# Pie Chart construction 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Credit Quality")


#---------[END OF CREDIT QUALITY]-------------------#
#                                                   #
#                                                   #
#                                                   #
#---------[START OF REGIONAL DIVERSIFICATION]-------#

#Set the Data Frame
dfRegional <- data.frame(RegionalDiversification = DATA$Country.of.Risk)
RCountryofRisk = ddply(dfRegional, .(RegionalDiversification), nrow)

#Calculation of the Total Number of Bonds to get the proportions later.
RCountryofRiskTotal <- sum(RCountryofRisk$V1)

#Proportions Calculations,  we want to know how many bonds are emitted by country. 
RCofRiskProportions <- round((RCountryofRisk$V1/RCountryofRiskTotal)*100, digits=2)

# Data for the table
CountryRiskTable <- RCountryofRisk$RegionalDiversification
NumbersPerCountryRisk <- RCountryofRisk$V1
ProCountryRisk <- RCofRiskProportions

#Table Construction of freshly created variables after the filtering. 
tbl <- BasicTable$new()
tbl$cells$setCell(1, 1, cellType="root", rawValue="Country Diversification")
tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="N")
tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="%")
tbl$cells$setColumn(1, cellTypes="rowHeader", rawValues=CountryRiskTable)
tbl$cells$setColumn(2, cellTypes="cell", rawValues=NumbersPerCountryRisk)
tbl$cells$setColumn(3, cellTypes="cell", rawValues=ProCountryRisk)
formats=list("%.2f"))
tbl$renderTable()



#---------[END OF REGIONAL DIVERSIFICATION]---------#
#                                                   #
#                                                   #
#                                                   #
#---------[START OF TOP 5 ISSUERS]------------------#

#Set the Data Frame
df <- data.frame(TOP5ISSUERS = DATA$Security.Name)
df <- transform(df, freq= ave(seq(nrow(df)), DATA$Security.Name, FUN=length))

#Sort the Data Frame by frequencies
df[order(-df$freq), ]
#Results are displayed in the console. We used the “order” function to sort the selected data by frequencies 
#which gave us a ranking of issuers by decreasing number of bonds. 

#---------[END OF TOP 5 ISSUERS]--------------------#
#                                                   #
#                                                   #
#                                                   #
#---------[START OF MATURITY]-----------------------#

#Set the Data Frame
dfMaturity <- data.frame(Maturity = DATA$Maturity)
MaturityX = ddply(dfMaturity, .(Maturity), nrow)
dfMaturityID <- data.frame(testttto = DATA$Security.Name)
MaturityID = ddply(df, .(dfMaturityID$testttto), nrow)

#Format the date to allow a subtraction which gives us the remaining days.
survey <- data.frame(date=c(DATA$Maturity),tx_start=c("1/1/2022"))

survey$date_diff <- as.Date(as.character(survey$date), format="%m/%d/%Y")-
  as.Date(as.character(survey$tx_start), format="%m/%d/%Y")

#Data for the table
MaturityName <- dfMaturityID$testttto
MaturityZ <- survey$date
TimeToMaturity <- as.numeric(survey$date_diff)
  #We divide the results by 365.2 (average number of days in a year) to get the remaining years until maturity.
TimeToMaturityYears <- round(TimeToMaturity /365.2, digits = 2)

Multiplier <- round(TimeToMaturity /365.2, digits = 0)

#First we tried to categorize $Multiplier variable by using the cut function, but it was not giving us the frequencies.
cut(Multiplier, breaks = c(0, 1, 2, 3, 4, 100), 
    labels = c("Between 0 and 1", "Between 1 and 2", "Between 3 and 4", "Between 4 and 5", "Over 5"))

#We constructed a table of the results
tbl <- BasicTable$new()
tbl$cells$setCell(1, 1, cellType="root", rawValue="Asset Name")
tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Maturity Date")
tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Remaining Days")
tbl$cells$setCell(1, 4, cellType="columnHeader", rawValue="Remaining Years")
tbl$cells$setColumn(1, cellTypes="rowHeader", rawValues=MaturityName)
tbl$cells$setColumn(2, cellTypes="cell", rawValues=MaturityZ)
tbl$cells$setColumn(3, cellTypes="cell", rawValues=TimeToMaturity)
tbl$cells$setColumn(4, cellTypes="cell", rawValues=TimeToMaturityYears)
formats=list("%.2f"))
tbl$renderTable()

#We then coded a  for loop to get frequencies classified into intervals.
dfMaturity <- data.frame(Maturity = DATA$Maturity)
yearList <- c()
for (x in dfMaturity) {
  yearList <- (as.numeric(substr(x,nchar(x)-3, nchar(x))) - 2021)
}

Matu <- as.data.frame(table(cut(yearList, seq(0,100,1))))
#This Print command run the frequencies for each intervals. Which allows us to know the Maturity Distribution.
print(Matu)



#---------[END OF MATURITY]--------------------#

#Additionally, we made a Git to make easier our collaboration. You can find an Average Return Calculation attempt and other drafts.
#https://github.com/LaMandale/R-IESEG-Project.git