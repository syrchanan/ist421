library(tidyverse)
my.art <- read_csv('art.csv')
View(my.art)

par(mfrow=c(2,2))

# Is there a relationship between the unit price of art goods and their units sold? If so, what kind of relationship is it?
plot(my.art$unit.price, my.art$units.sold,pch=16, col = "lightsteelblue4", 
        ylab = 'Number of Units Sold', xlab= 'Unit Price ($)', main='Unit Price vs Units Sold', type='p')
abline(lm(my.art$units.sold ~ my.art$unit.price),col='mediumvioletred',lwd=3)


# Does the art company sell more units of drawing paper or watercolor paper?
table(my.art$paper)
paper.units <- tapply(my.art$units.sold, list(my.art$paper), sum)
rownames(paper.units) <- c('Drawing','Watercolor')
barplot(paper.units, col=c('royalblue','red3'), xlab='Type of Paper', ylab = 'Units Sold', 
        main='Sales of Drawing Paper vs. Watercolor Paper')
        #legend.text = rownames(paper.units)) <-was going to use this but realized I didn't need the extra dimension


#Does the art company bring in more money (revenue) selling drawing paper or watercolor paper? 
options(scipen = 999)
paper.sales <- tapply(my.art$total.sale, list(my.art$paper), sum)
rownames(paper.sales) <- c('Drawing','Watercolor')
barplot(paper.sales, col=c('royalblue','red3'), xlab='Type of Paper', ylab = 'Total Revenue ($)', 
        main='Sales of Drawing Paper vs. Watercolor Paper')


# Each paper (watercolor and drawing) has different subtypes. It is possible that at some stores, some subtypes sell better than others. 
# For drawing paper only, make a plot that allows the viewer to compare which subtypes of drawing paper sell more (and less) units across the stores.
no.watercolor <- data.frame(
                  c(my.art$paper.type[my.art$paper!='watercolor']),
                  c(my.art$store[my.art$paper!='watercolor']),
                  c(my.art$units.sold[my.art$paper!='watercolor'])
                )
colnames(no.watercolor) = c('paper.type', 'store', 'units.sold')

paper.store.type <- tapply(no.watercolor$units.sold, list(no.watercolor$paper.type,no.watercolor$store), sum)
paper.store.type

barplot(paper.store.type,beside=T,col=c('red3','blue3','seagreen3','gray64'), xlab='Store Location', ylab='Units Sold', main='Drawing Paper Types Sold by Store', ylim=c(0,1000))
legend("topleft", inset=c(.25,.1),legend=rownames(paper.store.type),cex=0.8, lwd=5,lty=1,
       col=c('red3','blue3','seagreen3','gray64'))


#The dataset covers 4 years of data. Compare the revenue gained each year from the sales drawing paper with that of watercolor paper
#Are sales growing over time for both?

year.total <- tapply(my.art$total.sale, list(my.art$year, my.art$paper), sum)
colnames(year.total) <- c('Drawing', 'Watercolor')
plot(year.total[,1],type = 'l',col='red3',lwd=2,
     ylab="Revenue ($)", xlab = 'Years', xaxt='n',
     bty="n", ylim=c(10000,35000),
     main='Sales of Watercolor vs. Drawing Paper Over Time')
lines(year.total[,2],col='blue3',lwd=2)
axis(1, labels = 2012:2015, at=1:4)
legend('bottomright', inset=c(0,.1),legend=colnames(year.total),cex=1.5, lwd=5,lty=1,
       col=c('red3','blue3'))
