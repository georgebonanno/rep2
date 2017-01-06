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
    "BEAUTIFULLY CONVERTED house of character with a gorgeous layout and lots of outdoor space",
    "ARE YOU BUYING A PROPERTY? CHOICE OF APARTMENTS",
    "PROPERTIES for sale on",
    "PRICE REDUCTIONS this week on properties for sale at ",
    "A RESTAURANT situated in a prominent / central area, with good clientele. Phone ",
    "SELLING YOUR HOUSE",
    "PRICE REDUCTIONS this week on properties",
    "A SMALL NEWTERRACED HOUSE WITH THREE BEDROOMS AND A TWO-CAR",
    "CLAYTON CAMILLERI APARTMENTS",
    "COMMISSION ONLY APARTMENTS",
    "A BRAND NEW block with three apartments",
    "PROPERTIES for sale on www");
   
  
  valid <- !(any(sapply(FUN = lineContainsStr,X = invalidProps)))
             
  return(valid)
    
}

foreignCountries <- c(
  "BAHAMAS",
  "FRANCE",
  "RAGUSA",
  "SICILY",
  "TUSCANY"
)

isForeignCountry <- function(location) {
  return (any(location == foreignCountries))
}

isNumericFormat <- function(s) {
  return(grepl("^[0-9]+(\\.[0-9]+)?$",s));
}

extractArea <- function(propertyDesc) {
  area <- gsub(".*?([,0-9\\.]+) *sqm.*","\\1",propertyDesc,ignore.case = TRUE)
  area <- gsub("[,]",replacement = "",area)
  if (!isNumericFormat(area)) {
    area <- gsub(".*?([,0-9]+)msq.*","\\1",propertyDesc,ignore.case = TRUE)
    area <- str_replace_all(area,",","")
    if (!isNumericFormat(area)) {
      
      area <- gsub(".*?([,0-9\\.]+)(ha).*","\\1",propertyDesc)
      area <- gsub("[,]",replacement = "",area)
      if (isNumericFormat(area)) {
        area <- paste("0",area,sep="")
        #conversion of hectares to sqm
        area <- as.numeric(area)*10000
      } else {
        area <- gsub(".*?([0-9,\\.]+) *tumoli.*","\\1",propertyDesc)
        area <- gsub("[,]",replacement = "",area)
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
  matches <- gregexpr("€(och)?([0-9 ,]+)",line,perl = TRUE)
  currencyValues <- regmatches(line,matches)
  currencyValues <- gsub("[€,och ]","",currencyValues[[1]])
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
  return (!isForeignCountry(loc) && !grepl("rent",loc,ignore.case = TRUE));
}

extractFeatures <- function(line) {
  if (validPropertyDescs(line)) {
    location <- gsub("([^\\.:]+)[\\.:].*","\\1",line)
    if (location == line) {
      locations <- list();
    } else {
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
      if (secondPrices == "" && (length(loc) > 0) && validLocation(loc)) {
        extractedFeatures <- paste(loc,phone,price,description,area,sep=",");
      } else {
        extractedFeatures <- ""
      }
      return(extractedFeatures)
    }
    tryCatch({
      entireDescriptions <- lapply(FUN = makeDescriptions,X = locations)  
    },error=function(e) {
      pastePrint("error while processing text",line)
      stop(e)
    })
    
  } else {
    entireDescriptions <- list()
  }
  return(entireDescriptions)
}

extractFeatures("LANDRIJIET. Newly built bungalow, open views, on 2.75 tumoli of garden with pool, large living/ kitchen / dining, three bedrooms, gym. &euro;1,760,000. Phone 9987 0009.</p>")