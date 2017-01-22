library(ggplot2)

# builds a histogram that acts as an alternative for the pie chart described in 
# the write up.

# length of segment length of slices in pie chart
segmentLength=c(9.97,8.34,10.43,0.33)

#radius of pie chart.
r=5.63


# returns the angle of a slice (as a percentage) given
# the segment length
angle <- function(l){
  round((asin(l/(2*r))/pi)*100,2)
}

# returns the angle of the different slices in the pie chart
# as a percentage
ratios <- sapply(X = segmentLength,FUN = angle)

# returns the percentage that still remains to be computed
# (this difference is due to the lack of precision when 
# measuring the cords)
remainingPercent <- 100-sum(sapply(X = segmentLength,FUN = angle))

# distribute 0.01 percent to every part until
# 100% is achieved.
i<-0
while(remainingPercent > 0) {
  i <- (i+1)%%length(ratios)
  ratios[i] <- ratios[i]+0.01
  remainingPercent<-remainingPercent-0.01
}


# draw histogram
ratios[1] <- ratios[1]+remainingPercent

countries <- data.frame(country=c('Vanuatu','Tonga','Philippines','Malta'),
                        percentage=ratios
                        )

ggplot(data=countries,
       aes(x=reorder(country,
                     country,
                     function(x) {
                       -countries[countries$country==x,]$percentage
                     }),
           y=percentage)) +
  geom_bar(aes(fill=countries$country),stat="identity") +
   scale_y_continuous(limits=c(0,40)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  ggtitle("The 3 countries with the highest natural disaster risk and Malta") +
  labs(x="country",y="risk percentage")  +
  guides(fill=guide_legend(title="countries")) + theme_classic()
