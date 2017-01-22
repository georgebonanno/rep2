# performs a statistical analysis on the extracted adverts

# obtains all the adverts for villas
villas <- 
  propDetails[propDetails$property_type=='VILLA',]

# obtains all the adverts for maisonettes
maisonette <- 
  propDetails[propDetails$property_type=='MAISONETTE',]


# plot the density distribution for villa area
plot(density(as.numeric(villas$area_sqm)),
     xlab = "villa area in sqm",ylab = "density",
     main="villa area density histogram")

# plot the density distribution for maisonette area
plot(density(as.numeric(maisonette$area_sqm)),
     xlab = "maisonette area in sqm",
     ylab = "density",
     main="maisonette area in sqm density histogram")


# run the test
t.test(villas$area_sqm,maisonette$area_sqm,paired = FALSE)

# finding regression
plot(maisonette$price_euro,maisonette$area_sqm,
     xlab = "maisonette price (euro)",
     ylab = "area (sqm)",
     main="A graph of maisonette area (sqm) vs. price (euro)")

reg <- lm(data=maisonette,maisonette$price_euro ~ maisonette$area_sqm)

summary(reg)