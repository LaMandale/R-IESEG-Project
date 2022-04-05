DATA = read.csv("MVEMAG-holdings.csv")

#Download the Package
install.packages("plyr")

#Set the Data Frame
df <- data.frame(RankingsCount = DATA$Ranking)

#Call the library and Run the function
library(plyr)
RC = ddply(df, .(RankingsCount), nrow)

#Total Number of Credits
RCTotal <- sum(RC$V1)

#Percentages Calculations
RCProportions <- (RC$V1/RCTotal)*100

# Data for the table
Rankings <- RC$RankingsCount
Numbers <- RC$V1
Pro <- RCProportions

#Call the library  and Table Contruction
library(basictabler)
tbl <- BasicTable$new()
tbl$cells$setCell(1, 1, cellType="root", rawValue="Credit Quality")
tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="N")
tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="%")
tbl$cells$setColumn(1, cellTypes="rowHeader", rawValues=Rankings)
tbl$cells$setColumn(2, cellTypes="cell", rawValues=Numbers)
tbl$cells$setColumn(3, cellTypes="cell", rawValues=Pro)
                    formats=list("%.2f"))
tbl$renderTable()