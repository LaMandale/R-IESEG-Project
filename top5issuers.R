DATA = read.csv("MVEMAG-holdings.csv")

#Download Packages
install.packages("plyr")
install.packages("ggplot2")
library(ggplot2)
library(plyr)

#Set the Data Frame
df <- data.frame(CurrencyCount = DATA$Security.Name)
df <- transform(df, freq= ave(seq(nrow(df)), DATA$Security.Name, FUN=length))
df[order(-df$freq), ]

