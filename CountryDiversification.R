DATA = read.csv("MVEMAG-holdings.csv")

#Download Packages
install.packages("plyr")
install.packages("ggplot2")
library(ggplot2)

#Set the Data Frame
df <- data.frame(CountryCount = DATA$Country)

#Call the library and Run the function
library(plyr)
RCountry = ddply(df, .(CountryCount), nrow)

#Total Number of Credits
RCountryTotal <- sum(RCountry$V1)

#Percentages Calculations
RCProportions <- round((RCountry$V1/RCountryTotal)*100, digits=2)

# Data for the table
CountryCountTable <- RCountry$CountryCount
NumbersPerCountry <- RCountry$V1
ProCountry <- RCProportions

#Call the library  and Table Contruction
library(basictabler)
tbl <- BasicTable$new()
tbl$cells$setCell(1, 1, cellType="root", rawValue="Country Diversification")
tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="N")
tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="%")
tbl$cells$setColumn(1, cellTypes="rowHeader", rawValues=CountryCountTable)
tbl$cells$setColumn(2, cellTypes="cell", rawValues=NumbersPerCountry)
tbl$cells$setColumn(3, cellTypes="cell", rawValues=ProCountry)
formats=list("%.2f"))
tbl$renderTable()

