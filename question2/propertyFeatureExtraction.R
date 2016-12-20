library(stringr)
library(stringi)
source('locationFiltering.R')
source('propertyNameExtraction.R')

pastePrint <- function(...,sepr=" ") {
  print(paste(...,sep=sepr))
} 

validPropertyDescs <- function(line) {
  lineContainsStr <- function(x) {
          return(grepl(paste(".*",x,".*",sep=""),line,ignore.case = TRUE))
  }
  
  invalidProps <- c(
    "APARTMENTS, maisonettes, etc",
    "A RESTAURANT situated in a prominen",
    "ANNE PULLICINO \\(sensara\\)",
    "PROPERTIES for sale on Malta' *s best rated property website",
    "PROPERTIES for sale on",
    "PRICE REDUCTIONS this week on properties for sale at ",
    "A RESTAURANT situated in a prominent / central area, with good clientele. Phone ",
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
      locations <- list();
    } else {
      #location <- gsub(",","",location,perl=TRUE)
      locations <- resolveLocation(location)
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
    makeDescriptions <- function(l) {
      if (any(l==c('THE VILLAGE'))) {
        pastePrint("found",line)
      }
      paste(l,phone,price,description,area,sep=",");
    }
    entireDescriptions <- lapply(FUN = makeDescriptions,X = locations)
  } else {
    entireDescriptions <- list()
  }
  return(entireDescriptions)
}

extractFeatures('THE VILLAGE. Ground floor maisonette, ready to move into, two bedrooms with modern fitted kitchen. €205,00. Phone 7943 1977.')