library(stringr)
library(stringi)
source('propertyNameExtraction.R')

pastePrint <- function(...,sepr=" ") {
  print(paste(...,sep=sepr))
} 

validPropertyDescs <- function(line) {
  lineContainsStr <- function(x) {
          return(grepl(paste(".*",x,".*",sep=""),line,ignore.case = TRUE))
  }
  
  invalidProps <- c(
    "A RESTAURANT situated in a prominen",
    "PROPERTIES for sale on Malta' *s best rated property website",
    "SELLING YOUR HOUSE",
    "PRICE REDUCTIONS this week on properties",
    "PROPERTIES for sale on www");
   
  
  valid <- !(any(sapply(FUN = lineContainsStr,X = invalidProps)))
             
  return(valid)
    
}

isNumericFormat <- function(s) {
  return(grepl("^[0-9]*(\\.[0-9]+)?$",s));
}

extractArea <- function(propertyDesc) {
  area <- gsub(".*([0-9]+)sqm.*","\\1",propertyDesc)
  if (!isNumericFormat(area)) {
    area <- gsub(".*([0-9]+)msq.*","\\1",propertyDesc)
    if (!isNumericFormat(area)) {
      
      area <- gsub(".*?([0-9]*\\.[0-9]+)(ha).*","\\1",propertyDesc)
      if (isNumericFormat(area)) {
        area <- paste("0",area,sep="")
        #conversion of hectares to sqm
        area <- as.numeric(area)*1000
      } else {
        area <- gsub(".*?([0-9]+\\.[0-9]+) tumoli.*","\\1",propertyDesc)
        if (isNumericFormat(area)) {
          #conversion of tumoli to sqm
          area <- as.numeric(area)*1024;
        }
      }
    }
  }
  if (!(isNumericFormat(area) || is.numeric(area))) {
    area = ""
  }
  return(area)
  
}



extractFeatures <- function(line) {
  if (validPropertyDescs(line)) {
    location <- gsub("([^\\.:]+)[\\.:].*","\\1",line,perl=TRUE)
    if (location == line) {
      location <- "";
    } else {
      location <- gsub(",","",location,perl=TRUE)
    }
    phone <- gsub(".*([0-9]{4} *[0-9]{4}).*","\\1",line)
    phone <- gsub(" ","",phone)
    if (isNumericFormat(phone)) {
      phone <- as.numeric(phone)
    } else if (str_length(phone) > 0) {
      phone <- "";
    }
    price <- gsub(".*€([0-9,]+).*","\\1",line,perl=TRUE)
    price <- gsub(",","",price,perl=TRUE)
    if (!isNumericFormat(price)) {
      price<-""
    } else {
      price <- as.numeric(price)
    }
    area <- extractArea(line)
    
    description <- extractPropertyDescription(line)
    entireDescription <- paste(location,phone,price,description,area,sep=",");
  } else {
    entireDescription <- ""
  }
  return(entireDescription)
}

extractFeatures('GOZO, GĦAJNSIELEM. An ideal residential plot of land for investment or for a good sized terraced house, situated in a three story-height zone. €185,000. Phone 9947 6959.')