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
    "100% FOCUSED on quality properties",
    "ANNE PULLICINO \\(sensara\\)",
    "PROPERTIES for sale on Malta' *s best rated property website",
    "PROPERTIES for sale on",
    "PRICE REDUCTIONS this week on properties for sale at ",
    "A RESTAURANT situated in a prominent / central area, with good clientele. Phone ",
    "SELLING YOUR HOUSE",
    "PRICE REDUCTIONS this week on properties",
    "CLAYTON CAMILLERI APARTMENTS",
    "COMMISSION ONLY APARTMENTS",
    "A BRAND NEW block with three apartments",
    
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

extractPrices <- function(line) {
  matches <- gregexpr("€([0-9 ,]+)",line,perl = TRUE)
  currencyValues <- regmatches(line,matches)
  currencyValues <- gsub("[€, ]","",currencyValues[[1]])
  currencyValues <- lapply(currencyValues,as.numeric)
  return(currencyValues)
}

firstPriceFound <- function(extractedPrices) {
  if (length(extractedPrices) > 0) {
    firstPrice <- extractedPrices[[1]][1]
  } else {
    firstPrice <- ""
  }
  
  return(firstPrice)
}

secondPriceFound <- function(extractedPrices) {
  extractedPrices <- unlist(extractedPrices,recursive = TRUE)
  if (length(extractedPrices) > 1) {
    secondPrices <- paste(extractedPrices[2:length(extractedPrices)],
                          sep="|",
                          collapse = "|")
  } else {
    secondPrices <- ""
  }
  
  return(secondPrices)
}

validLocation <- function(loc) {
  return (!grepl("rent",loc,ignore.case = TRUE));
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
    prices <- extractPrices(line)
    area <- extractArea(line)
    
    description <- extractPropertyDescription(line)
    makeDescriptions <- function(loc) {
      loc <- str_to_upper(loc)
      price <- firstPriceFound(prices)
      secondPrices <- secondPriceFound(prices)
      if (secondPrices == "" && validLocation(loc)) {
        extractedFeatures <- paste(loc,phone,price,description,area,secondPrices,sep=",");
      } else {
        extractedFeatures <- ""
      }
      return(extractedFeatures)
    }
    entireDescriptions <- lapply(FUN = makeDescriptions,X = locations)
  } else {
    entireDescriptions <- list()
  }
  return(entireDescriptions)
}

extractFeatures("&euro;170,000 (Lm73,000). Phone 9986 1713.")