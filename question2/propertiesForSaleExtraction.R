library(rvest)
library(stringi)
library(stringr)
source('propertyFeatureExtraction.R')

extractDate <- function(dateStr) {
  parsedDate <- gsub('.*?[^,]+, ([^ ]+) ([^,]+), ([0-9]+).*','\\2-\\1-\\3',dateStr)
  extractedDate <- as.Date(c(parsedDate),'%d-%B-%Y')
  return(extractedDate)
}

readForPropertyForSale <- function(f) {
  html <- read_html(f)
  as <- html %>% html_nodes('li > p,.classified_date') %>% html_text()
  classDate <- ""
  for (a in as) {
    propSaleLen <- str_length('\nProperty For Sale\n')
    
    if (startsWith(a,'\nProperty For Sale\n') & str_length(a) > propSaleLen) {
      propertyDesc <- substr(a,propSaleLen+1,str_length(a))
      descs <- extractFeatures(propertyDesc)
      for (desc in descs) {
        if (str_length(desc) > 0) {
          print(paste(classDate,desc,sep=","))
        } 
      }
    } else {
      classDate <- extractDate(a);
    }
  }
}

extractDataFromDir <- function(sourceDir) {
  htmlFiles <- list.files(sourceDir,pattern="*.html*")
  fileToHtmlTextInfo <- list()
  
  htmlFiles <- htmlFiles[1:1]
  for (htmlFile in htmlFiles) {
    pathName <- paste('data2','/',htmlFile,sep = "")
    readForPropertyForSale(pathName)
  }
}

extractDataFromDir('data2')