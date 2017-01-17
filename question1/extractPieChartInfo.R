segmentLength=c(9.97,8.34,10.43,0.33)
r=5.63

angle <- function(l){
  round((asin(l/(2*r))/pi)*100,2)
}

ratios <- sapply(X = segmentLength,FUN = angle)

remainingPercent <- 100-sum(sapply(X = segmentLength,FUN = angle))

i<-0
while(remainingPercent > 0) {
  i <- (i+1)%%length(ratios)
  ratios[i] <- ratios[i]+0.01
  remainingPercent<-remainingPercent-0.01
}

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
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  ggtitle("risk percentage of the topmost 3 countries with the \nlargest risk percentage compared to Malta") +
  labs(x="country",y="risk percentage")  + 
  guides(fill=guide_legend(title="countries")) + theme_classic()
