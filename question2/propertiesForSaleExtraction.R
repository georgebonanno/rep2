library(rvest)
library(stringi)
library(stringr)
source('propertyFeatureExtraction.R')

# extracts the features from the adverts found int the 'data' folder.

# extract the article date 
extractDate <- function(dateStr) {
  parsedDate <- gsub('.*?[^,]+, ([^ ]+) ([^,]+), ([0-9]+).*','\\2-\\1-\\3',dateStr)
  extractedDate <- as.Date(c(parsedDate),'%d-%B-%Y')
  return(extractedDate)
}

# attempts to parse all the adverts in an html file by looking 
# at the li nodes described by the css selector 'li > p,.classified_date'.
# every parsed advert (i.e. advert date, locality, price etc) is printed 
# to standard out.
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

# process html files in directory sourceDir.
extractDataFromDir <- function(sourceDir) {
  htmlFiles <- list.files(sourceDir,pattern="*.html*")
  fileToHtmlTextInfo <- list()
  
  for (htmlFile in htmlFiles) {
    pathName <- paste('data','/',htmlFile,sep = "")
    readForPropertyForSale(pathName)
  }
}

# extract html files from the 'data' folder
extractDataFromDir('data')