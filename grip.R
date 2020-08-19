library(ggplot2)
library(lattice)
library(latticeExtra)
library(dplyr)
library(reshape2)
superstore = read.csv("SampleSuperstore.csv")
names(superstore)<- tolower(names(superstore))
#1
qplot(region, sales, data = superstore, geom = c("line"), size = 3,
      facets = .~category, color = region, main = "Sales in each region by categories")
dev.copy(png, file = "Plot_1.png")
dev.off()

#2
g=ggplot(superstore, aes(x= state, y = sales))
g + geom_col(size = 1, aes(fill = segment) ) +theme(axis.text.x = element_text(angle = 90)) + 
  scale_fill_brewer(palette = "Set1") + ggtitle("Sales vs States divided by segment")
dev.copy(png, file = "Plot_2.png")
dev.off()


super<- superstore %>% select(-c(country, city, postal.code)) %>% arrange(category, sub.category,state)

#3
barplot(table(super$segment, super$sub.category), cex.names = 0.65, col= c("red", "blue", "magenta"), beside = TRUE,
        main = "Count of accessories for each segment", xlab = "Sub Category")
legend("topright", c("Consumer", "Corporate", "Home Office"), 
       fill = c("red", "blue", "magenta"), cex = 0.7)
dev.copy(png, file = "Plot_3.png")
dev.off()


#4
pro<-ggplot(data = super, aes(x = state, y = profit))+ geom_col(aes(fill = state), size = 2 ) + theme(
  axis.text.x = element_text(angle = 90), legend.position = "none") +
  ggtitle("Profit for each state")
print(pro)
dev.copy(png, file = "Plot_4.png")
dev.off()

#5                                                                             
xyplot(sales~profit, data = super, col = rgb(0,0,0, 0.3), pch = 19, xlim = c(-5000,5000), ylim = c(0,15000),
       main = "Overall sales and profits variation")
dev.copy(png, file = "Plot_5.png")
dev.off()

#6
a <- with(super, tapply(quantity, ship.mode, sum))
piepercent<- round(a*100/sum(a), 1)
pie(a, col = c("red", "green", "blue", "magenta"), labels = paste(piepercent, "%"), main = "Shipping Mode division")
legend("topright", c("First Class","Same Day", "Second Class", "Standard Class" ), cex = 0.7,
       fill = c("red", "green", "blue", "magenta"))
dev.copy(png, file = "Plot_6.png")
dev.off()






