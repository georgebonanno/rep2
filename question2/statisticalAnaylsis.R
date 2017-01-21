villas <- 
  propDetails[propDetails$property_type=='VILLA',]

maisonette <- 
  propDetails[propDetails$property_type=='MAISONETTE',]

plot(density(as.numeric(villas$area_sqm)),
     xlab = "villa area in sqm",ylab = "density",
     main="villa area density histogram")

plot(density(as.numeric(maisonette$area_sqm)),
     xlab = "maisonette area in sqm",
     ylab = "density",
     main="maisonette area in sqm density histogram")


t.test(villas$area_sqm,maisonette$area_sqm,paired = FALSE)

# finding regression
plot(maisonette$price_euro,maisonette$area_sqm,
     xlab = "maisonette price (euro)",
     ylab = "area (sqm)",
     main="A graph of maisonette area (sqm) vs. price (euro)")

reg <- lm(data=maisonette,maisonette$price_euro ~ maisonette$area_sqm)

summary(reg)