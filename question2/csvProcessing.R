propertyDetails <- read.csv("unique_features.csv",
                            header = TRUE,sep = ",")

propertyDetails$price_euro <- as.numeric(propertyDetails$price_euro)