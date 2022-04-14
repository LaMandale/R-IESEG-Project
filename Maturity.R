DATA = read.csv("MVEMAG-holdings.csv")

#Download Packages
install.packages("plyr")
install.packages("ggplot2")
library(ggplot2)
library(plyr)

#Set the Data Frame
dfMaturity <- data.frame(Maturity = DATA$Maturity)

#Call the library and Run the function
MaturityX = ddply(df, .(Maturity), nrow)

#Set the Data Frame
dfMaturityID <- data.frame(testttto = DATA$Security.Name)

#Call the library and Run the function
MaturityID = ddply(df, .(dfMaturityID$testttto), nrow)

survey <- data.frame(date=c(DATA$Maturity),tx_start=c("1/1/2022"))

survey$date_diff <- as.Date(as.character(survey$date), format="%m/%d/%Y")-
  as.Date(as.character(survey$tx_start), format="%m/%d/%Y")

# Data for the table
MaturityName <- MaturityID$`dfMaturityID$testttto`
MaturityZ <- survey$date
TimeToMaturity <- as.numeric(survey$date_diff)

Multiplier <- TimeToMaturity /365.2

cut(Multiplier, breaks = c(0, 1, 2, 3, 4), 
    labels = c("Between 0 and 1", "Between 1 and 2", "Between 3 and 4", "Over 4"))

#Call the library  and Table Construction
library(basictabler)
tbl <- BasicTable$new()
tbl$cells$setCell(1, 1, cellType="root", rawValue="Asset Name")
tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Maturity Date")
tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Remaining Days")
tbl$cells$setColumn(1, cellTypes="rowHeader", rawValues=MaturityName)
tbl$cells$setColumn(2, cellTypes="cell", rawValues=MaturityZ)
tbl$cells$setColumn(3, cellTypes="cell", rawValues=TimeToMaturity)
formats=list("%.2f"))
tbl$renderTable()