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
   
  if (grepl("[^a-z]rent",line,ignore.case = TRUE)) {
    valid <- FALSE
  } else {
    valid <- !(any(sapply(FUN = lineContainsStr,X = invalidProps)))
  }
             
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

multiplierForUnits <- function(currencyValues) {
  if (grepl("[0-9]+[MK]$",currencyValues,ignore.case = TRUE)) {
    if (endsWith(currencyValues,"M")) {
      units <- "M"
    } else if (endsWith(currencyValues,"K")) {
      units <- "K"
    } else {
      units <- "";
    }
  } else {
    units <- ""
  }
  if (units == "M") {
    multiplier = 1e6
  } else if (units == "K") {
    multiplier = 1000
  } else {
    multiplier = 1
  }
  return(multiplier)
}

extractPrices <- function(line) {

  matches <- gregexpr("€(och)?([0-9 ,\\.]+ *[0-9,\\.]+[mk]?)",
                      line,
                      ignore.case = TRUE,perl = TRUE)
  currencyValues <- regmatches(line,matches)
  # any line with more than one currency values is ignored
  # since it may lead to misinterpretations (e.g initial price) 
  # and rent or a further offer (e.g ground floor costs EUR X 1st.
  # 1st floor with airspace further EUR Y)
  if (length(currencyValues[[1]]) == 1) {
  
    currencyValues <- str_to_upper(currencyValues)
    multiplier <- multiplierForUnits(currencyValues[[1]][1])
    currencyValues <- gsub("[€,ochmK ]","",ignore.case = TRUE,currencyValues)
    currencyValues <- gsub("\\.$","",ignore.case = TRUE,currencyValues)
    
    currencyValues <- lapply(currencyValues,function(v){
      return(as.numeric(v)*multiplier)
    })

  } else {
    currencyValues <- c();
  }
  return(currencyValues)
}

extractPricePossiblyWithArea <- function(line,area) {
  matches <- gregexpr("€(och)?([0-9 ,\\.]+) *[\\/]sqm",
                      line,ignore.case = TRUE,perl = TRUE)
  currencyValues <- regmatches(line,matches)
  if (length(currencyValues[[1]]) > 0) {
    currencyValues <- gsub("€|( *[\\/]sqm)","",currencyValues[[1]],ignore.case = TRUE)
    currencyValues <- as.numeric(currencyValues)
    if (!is.na(area) && isNumericFormat(area)) {
      currencyValues <- currencyValues*as.numeric(area)
    }
  } else {
    currencyValues <- extractPrices(line)
  }
  
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
    area <- extractArea(line)
    prices <- extractPricePossiblyWithArea(line,area)
    
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

extractFeatures("ST PAUL'S BAY. 4-star hotel accommodating 110 twin bedrooms, fully equipped, 48 self-catering apartments, pub / coffee shop / souvenir shop, wedding hall and indoor pool. €31,000,000. Phone 7713 4186.</p>")
