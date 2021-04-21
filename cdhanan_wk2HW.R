library(tidyverse)

#Hot Dog Plot 1

hotdogs <- read_csv("hot-dog-contest-winners.csv")
View(hotdogs)

barplot(hotdogs$`Dogs eaten`)
barplot(hotdogs$`Dogs eaten`, names.arg=hotdogs$Year)
barplot(hotdogs$`Dogs eaten`, names.arg=hotdogs$Year, 
        col='red', border=NA, xlab = 'Year', ylab = "Hot dogs and buns (HDB) eaten")

fill.colors <- c()
for (i in 1:length(hotdogs$Country)) {
  if (hotdogs$Country[i] == 'United States') {
    fill.colors <- c(fill.colors, '#821122')
  } else {
    fill.colors <- c(fill.colors, '#cccccc')
  }
}

barplot(hotdogs$`Dogs eaten`, names.arg=hotdogs$Year, 
        col=fill.colors, border=NA, xlab = 'Year', ylab = "Hot dogs and buns (HDB) eaten")

fill.colors <- c()
for (i in 1:length(hotdogs$`New record`)) {
  if (hotdogs$`New record`[i] == '1') {
    fill.colors <- c(fill.colors, '#821122')
  } else {
    fill.colors <- c(fill.colors, '#cccccc')
  }
}

barplot(hotdogs$`Dogs eaten`, names.arg=hotdogs$Year, 
        col=fill.colors, border=NA, xlab = 'Year', ylab = "Hot dogs and buns (HDB) eaten")
barplot(hotdogs$`Dogs eaten`, names.arg=hotdogs$Year, space=0.3,
        col=fill.colors, border=NA, xlab = 'Year', ylab = "Hot dogs and buns (HDB) eaten",
        main="Nathan's Hot Dog Eating Contest Results, 1980-2010")

#Hot Dog Plot 2

hot.dog.places <- read_csv('http://datasets.flowingdata.com/hot-dog-places.csv')

hot.dog.matrix <- as.matrix(hot.dog.places)

barplot(hot.dog.matrix, border=NA, space=0.25, ylim=c(0, 200),
        xlab="Year", ylab="Hot dogs and buns (HDBs) eaten",
        main="Hot Dog Eating Contest Results, 1980-2010")

#Scatterplot Sub plot 1

subscribers <- read_csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv")

head(subscribers)

plot(subscribers$Subscribers)
plot(subscribers$Subscribers, type="p", ylim=c(0, 30000))

plot(subscribers$Subscribers, type="h", ylim=c(0, 30000),
     xlab="Day", ylab="Subscribers")
points(subscribers$Subscribers, pch=19, col="black")

#Time Series Chart

population <- read_csv("http://datasets.flowingdata.com/world-population.csv")

head(population)

plot(population$Year, population$Population, type="l",
     ylim=c(0, 7000000000), xlab="Year", ylab="Population")

#Step Chart

postage <- read_csv("http://datasets.flowingdata.com/us-postage.csv")

plot(postage$Year, postage$Price, type="s")
plot(postage$Year, postage$Price, type="s",
     main="US Postage Rates for Letters, First Ounce, 1991-2010",
     xlab="Year", ylab="Postage Rate (Dollars)")

########
#Part 2#
########

art <- read_csv("art.csv")

dev.off()
par(mfrow=c(2,2))

hist(art$total.sale, xlab = "Total Sale ($)", ylab = "Transactions", main = "Total Sale Distribution of Art Supplies",
     col = 'lightblue2', border = 'gray17', breaks = 20)

plot(art$total.sale, xlab = 'Transactions', ylab = 'Total Sale ($)', main = "Total Sales Distribution of Art Supplies",
     col = 'royalblue3', cex = 0.3, pch = 5)

boxplot(art$total.sale[art$paper == 'drawing'], xlab = "Total Sale ($)", ylab = "Transactions",
        main = 'Total Sales Distribution of Water Color Paper', col = 'seagreen3', border = 'gray17', 
        horizontal = T, pch=22, cex = 1)
boxplot(art$total.sale[art$paper == 'watercolor'], xlab = "Total Sale ($)", ylab = "Transactions", 
        main = 'Total Sales Distribution of Drawing Paper', col = 'seagreen3', border = 'gray17', 
        horizontal = T, pch=22, cex = 1)
        