library(stringr)
library(stringi)
source('propertyNameExtraction.R')

pastePrint <- function(...,sepr=" ") {
  print(paste(...,sep=sepr))
}

validPropertyDescs <- function(line) {
  valid <- !(grepl('Phone 2388 0009 or 7900 8287',line) ||
               grepl("PROPERTY FOR SALE",line) ||
              grepl("PROPERTIES for sale on Malta's best rated property website:",line))
             
  return(valid)
    
}

isNumericFormat <- function(s) {
  return(grepl("^[0-9]+$",s));
}


extractFeatures <- function(line) {
  if (validPropertyDescs(line)) {
    location <- gsub("([^\\.]+)\\..*","\\1",line,perl=TRUE)
    location <- gsub(",","",location,perl=TRUE)
    phone <- gsub(".*([0-9]{4} *[0-9]{4}).*","\\1",line)
    phone <- gsub(" ","",phone)
    pastePrint("phone = ",line)
    if (isNumericFormat(phone)) {
      phone <- as.numeric(phone)
    } else if (str_length(phone) > 0) {
      raise(paste("phone number is not a number in",line))
    }
    price <- gsub(".*€([0-9,]+).*","\\1",line,perl=TRUE)
    price <- gsub(",","",price,perl=TRUE)
    if (!isNumericFormat(price)) {
      price<-""
    } else {
      price <- as.numeric(price)
    }
    description <- extractPropertyDescription(line)
    entireDescription <- paste(location,phone,price,description,sepr=",");
  } else {
    entireDescription <- ""
  }
  return(entireDescription)
}

extractFeatures('GOZO, GĦAJNSIELEM. An ideal residential plot of land for investment or for a good sized terraced house, situated in a three story-height zone. €185,000. Phone 9947 6959.')