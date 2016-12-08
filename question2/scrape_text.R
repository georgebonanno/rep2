library(rvest)

source('tm_utils.R')

pastePrint <- function(...) {
  print(paste(...,sep = " "))
}


extractPs <- function(f) {
  pastePrint("extract html text from ",f,"...")
  scraping_times <- read_html(f)
  
  tags <- c("p","h1","h2","h3","li","table","div")
  

  htmlText = "";
  for (t in tags) {
    textInTag <- scraping_times %>% html_nodes(t) %>% html_text()
    textInTag <- paste(textInTag,collapse = "")
    htmlText <- paste(htmlText,textInTag)
  }

  pastePrint("data loaded from ",f)
  return(htmlText)
}

extractDataFromHtmlInDir <- function(sourceDir) {
  htmlFiles <- list.files(sourceDir,pattern="*.html*")
  fileToHtmlTextInfo <- list()
  #htmlFiles <- htmlFiles[1:2]
  pastePrint("html files = ",htmlFiles)
  for (htmlFile in htmlFiles) {
    pastePrint('working on file ',htmlFile)
    f <- paste(sourceDir,'/',htmlFile,sep="")
    htmlTextForF <- extractPs(f)
    fileToHtmlTextInfo[[htmlFile]] <- htmlTextForF
  }
  
  return(fileToHtmlTextInfo)
}

plotFreqs <- function(freqMat,n) {
  htm <- head(freqMat,n)
  p <- ggplot(htm,aes(htm$word,htm$freq))
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
  p <- p + geom_bar(stat="identity")
  p
}

print("starting...")
docTexts <- extractDataFromHtmlInDir('data')
termMatrix <- extractDocTermMatrixForListOfStrings(docTexts)

plotFreqs(freqMat = termMatrix$dtm,20)

