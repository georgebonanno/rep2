library(ggplot2)

# analysis and plots graphs related to the adverts with respect to the
# publishing dates.

# loads advert information (together with the date when each advert appeared)
exFeatures <- read.csv('extracted_with_date_unique.csv',sep = ",",
                       col.names = c("date","location","number",
                                     "price_euro","property_type","area_sqm"),
                       row.names = NULL)

# perform and filtering/numeric conversion
exFeatures <- exFeatures[!is.na(exFeatures$date),]
exFeatures$dates <- as.Date(exFeatures$date)
exFeatures <- exFeatures[!is.na(exFeatures$date),]
exFeatures$price_euro <- as.numeric(exFeatures$price_euro)

orderDates <- (exFeatures$date[order(exFeatures$date)])

orderDates[1]
orderDates[length(orderDates)]

uniqueDates <- unique(orderDates)
uniqueDates <- uniqueDates[order(uniqueDates)]

# obtains the date range when no advert was found
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


# sort date range by longest range first
orderedDifferences <- largeDiffList[order(-largeDiffList$ddiff),]
orderedDifferences


# obtains advert days that are Sundays
sundays <- exFeatures[weekdays(exFeatures$dates) == 'Sunday',]
# obtains advert days that are not Sundays
otherDays <- exFeatures[weekdays(exFeatures$dates) != 'Sunday',]

# plots the count of adverts per property type every Sunday.
ggplot(sundays,
       aes(x=dates,color=property_type))+
  geom_line(stat="count") +   theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("number of adverstiments")+
  xlab("date")+
  ggtitle("advertisment count for Sundays") 

ggsave('sunday_count_adverts.png')


# plots the count of adverts per property type every Sunday.
ggplot(otherDays,
       aes(x=dates))+
  geom_line(stat="count") +theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("number of adverstiments")+
  xlab("date")+
  ggtitle("number of adverstiments for days other than Sundays") 
  

ggsave('not_sunday_count_adverts.png')


# perform outlier removal (these value were determing after plotting box plot)
exFeatures <- exFeatures[exFeatures$price_euro < 8e6,]
exFeatures <- exFeatures[exFeatures$price_euro > 1000,]

# calculate mean price of advertisment in a given date
exFeatures <- na.omit(exFeatures)
meanPriceAdvertisment <- 
  aggregate(x= list(mean_price = exFeatures$price_euro),
            by=list(date=exFeatures$dates),
            FUN=mean)

# plot mean price advertisment per date
ggplot(meanPriceAdvertisment,
       aes(x=date,y=mean_price))+
  geom_line() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("mean price (€)")+
  xlab("date")+
  ggtitle("mean price on advertising dates") 

ggsave("mean_advertisment_per_date.png")


