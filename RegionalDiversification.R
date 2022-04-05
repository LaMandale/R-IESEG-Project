DATA = read.csv("MVEMAG-holdings.csv")

#Download Packages
install.packages("plyr")
install.packages("ggplot2")
library(ggplot2)

#Set the Data Frame
df <- data.frame(RegionalDiversification = DATA$Country.of.Risk)

#Call the library and Run the function
library(plyr)
RCountryofRisk = ddply(df, .(RegionalDiversification), nrow)

#Total Number of Credits
RCountryofRiskTotal <- sum(RCountryofRisk$V1)

#Percentages Calculations
RCofRiskProportions <- round((RCountryofRisk$V1/RCountryofRiskTotal)*100, digits=2)

# Data for the table
CountryRiskTable <- RCountryofRisk$RegionalDiversification
NumbersPerCountryRisk <- RCountryofRisk$V1
ProCountryRisk <- RCofRiskProportions

#Call the library  and Table Contruction
library(basictabler)
tbl <- BasicTable$new()
tbl$cells$setCell(1, 1, cellType="root", rawValue="Country Diversification")
tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="N")
tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="%")
tbl$cells$setColumn(1, cellTypes="rowHeader", rawValues=CountryRiskTable)
tbl$cells$setColumn(2, cellTypes="cell", rawValues=NumbersPerCountryRisk)
tbl$cells$setColumn(3, cellTypes="cell", rawValues=ProCountryRisk)
formats=list("%.2f"))
tbl$renderTable()