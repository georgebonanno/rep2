# performs a statistical analysis on the extracted adverts
# The analysis checking if there is a statistical difference 
# between the mean area of villas with that of maisonettes.

source('csvProcessing.R')

# obtain all the villas in the data
villas <- 
  propDetails[propDetails$property_type=='VILLA',]

# obtain all the maisonettes in the data
maisonette <- 
  propDetails[propDetails$property_type=='MAISONETTE',]

# plot the area distribution for villas and maisonettes
# to find if the distribution is normal
plot(density(as.numeric(villas$area_sqm)),
     xlab = "villa area in sqm",ylab = "density",
     main="villa area density")

plot(density(as.numeric(maisonette$area_sqm)),
     xlab = "maisonette area in sqm",
     ylab = "density",
     main="maisonette area in sqm density")


# perform F test to see whether the area have equal variances
var.test(villas$area_sqm,maisonette$area_sqm)

# perform t-test with var.equal set to FALSE since variances are not equal.
t.test(villas$area_sqm,maisonette$area_sqm,var.equal=FALSE, paired = FALSE)

