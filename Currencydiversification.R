DATA = read.csv("MVEMAG-holdings.csv")

#Download the Package
install.packages("plyr")
install.packages("ggplot2")
library(ggplot2)

#Set the Data Frame
df <- data.frame(CurrencyCount = DATA$Coupon.Currency)

#Call the library and Run the function
library(plyr)
RCurrency = ddply(df, .(CurrencyCount), nrow)

#Total Number of Credits
RCurrencyTotal <- sum(RCurrency$V1)

#Percentages Calculations
RCurrencyProportions <- round((RCurrency$V1/RCurrencyTotal)*100, digits=2)


# Data for the table
CurrencyDiv <- RCurrency$CurrencyCount
CurrencyNumbers <- RCurrency$V1
CurrencyPro <- RCurrencyProportions
fibseq <- c(CurrencyDiv[29], CurrencyDiv[26], CurrencyDiv[8], CurrencyDiv[13], CurrencyDiv[18], "Other")
fibseq2 <- c(CurrencyNumbers[29], CurrencyNumbers[26], CurrencyNumbers[8], CurrencyNumbers[13], CurrencyNumbers[18], RCurrencyTotal - sumfibseg2)
fibseq3 <- c(CurrencyPro[29], CurrencyPro[26], CurrencyPro[8], CurrencyPro[13], CurrencyPro[18], 100 - sumfibseg3)
sumfibseg2 <- sum(c(CurrencyNumbers[29], CurrencyNumbers[26], CurrencyNumbers[8], CurrencyNumbers[13], CurrencyNumbers[18]))
sumfibseg3 <- sum(c(CurrencyPro[29], CurrencyPro[26], CurrencyPro[8], CurrencyPro[13], CurrencyPro[18]))


#Call the library  and Table Contruction
library(basictabler)
tbl <- BasicTable$new()
tbl$cells$setCell(1, 1, cellType="root", rawValue="Currency Diversification")
tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="N")
tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="%")
tbl$cells$setColumn(1, cellTypes="rowHeader", rawValues=fibseq)
tbl$cells$setColumn(2, cellTypes="cell", rawValues=fibseq2)
tbl$cells$setColumn(3, cellTypes="cell", rawValues=fibseq3)
formats=list("%.2f"))
tbl$renderTable()

# Currency Diversification
slices <- fibseq2
lbls <- fibseq
pct <- fibseq3
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")



