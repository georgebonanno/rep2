library(rvest)
library(stringi)
library(stringr)

readForPropertyForSale <- function(f) {
  html <- read_html(f)
  as <- html %>% html_nodes('li > p,.classified_date') %>% html_text()
  classDate <- ""
  for (a in as) {
    propSaleLen <- str_length('\nProperty For Sale\n')
    
    if (startsWith(a,'\nProperty For Sale\n') & str_length(a) > propSaleLen) {
      len <- substr(a,propSaleLen+1,str_length(a))
      
      print(paste(classDate,len,sep="|"))
    } else {
      classDate <- a;
    }
  }
}

extractDataFromDir <- function(sourceDir) {
  htmlFiles <- list.files(sourceDir,pattern="*.html*")
  fileToHtmlTextInfo <- list()
  
  htmlFiles <- htmlFiles[1:1]
  for (htmlFile in htmlFiles) {
    pathName <- paste('data','/',htmlFile,sep = "")
    readForPropertyForSale(pathName)
  }
}

extractDataFromDir('data')