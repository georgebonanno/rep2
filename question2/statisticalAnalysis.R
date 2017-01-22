source('csvProcessing.R')

villas <- 
  propDetails[propDetails$property_type=='VILLA',]

maisonette <- 
  propDetails[propDetails$property_type=='MAISONETTE',]

plot(density(as.numeric(villas$area_sqm)),
     xlab = "villa area in sqm",ylab = "density",
     main="villa area density")

plot(density(as.numeric(maisonette$area_sqm)),
     xlab = "maisonette area in sqm",
     ylab = "density",
     main="maisonette area in sqm density")

var.test(villas$area_sqm,maisonette$area_sqm)

t.test(villas$area_sqm,maisonette$area_sqm,var.equal=FALSE, paired = FALSE)

