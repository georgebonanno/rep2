exFeatures <- read.csv('extracted_with_date_unique.csv',sep = ",",
                       col.names = c("date","location","number",
                                     "price_euro","property_type","area_sqm"),
                       row.names = NULL)

exFeatures <- exFeatures[!is.na(exFeatures$date),]
exFeatures$dates <- as.Date(exFeatures$date)
exFeatures <- exFeatures[!is.na(exFeatures$date),]
exFeatures$price_euro <- as.numeric(exFeatures$price_euro)

orderDates <- (exFeatures$date[order(exFeatures$date)])

orderDates[1]
orderDates[length(orderDates)]

uniqueDates <- uniqueDates[order(uniqueDates)]
length(uniqueDates)

ggplot(exFeatures[1:3000,],
       aes(x=date,color=property_type))+
  geom_bar(stat="count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("number of adverstiments") + 
  ggtitle("advertisment date") 

dateDiff <- c()
largeDiffList <- data.frame(d1=c(),d2=c(),ddiff = c())
for (i in 1:(length(uniqueDates)-1)) {
  dateDiff[i] <- difftime(uniqueDates[i+1],uniqueDates[i])
  if (dateDiff[i] > 1) {

    n <- 1+length(largeDiffList$d1)
    entry <- data.frame(d1=c(paste("",uniqueDates[i])),
                        d2=c(paste("",uniqueDates[i+1])),
                        ddiff = c(dateDiff[i]))
    largeDiffList <- rbind(largeDiffList,entry)

  }
}


orderedDifferences <- largeDiffList[order(-largeDiffList$ddiff),]
orderedDifferences


sundays <- exFeatures[weekdays(exFeatures$dates) == 'Sunday',]
otherDays <- exFeatures[weekdays(exFeatures$dates) != 'Sunday',]

ggplot(sundays,
       aes(x=dates,color=property_type))+
  geom_line(stat="count") +   theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("number of adverstiments")+
  xlab("date")+
  ggtitle("advertisment count for Sundays") 

ggplot(otherDays,
       aes(x=dates))+
  geom_line(stat="count") +theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("number of adverstiments")+
  xlab("date")+
  ggtitle("number of adverstiments for days other than Sundays") 
  




exFeatures <- exFeatures[exFeatures$price_euro < 8e6,]
exFeatures <- exFeatures[exFeatures$price_euro > 1000,]

exFeatures <- na.omit(exFeatures)
meanPriceAdvertisment <- 
  aggregate(x= list(mean_price = exFeatures$price_euro),
            by=list(date=exFeatures$dates),
            FUN=mean)

ggplot(meanPriceAdvertisment,
       aes(x=date,y=mean_price))+
  geom_line() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("mean price (€)")+
  xlab("date")+
  ggtitle("mean price on advertising dates") 


