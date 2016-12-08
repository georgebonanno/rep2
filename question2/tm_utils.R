library(tm)

applyTM <- function(docs) {
  
  docs <- Corpus(VectorSource(docs))   
  
  docs <- tm_map(docs,removePunctuation)
  docs <- tm_map(docs,removeNumbers)
  docs <- tm_map(docs,tolower)
  
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, stemDocument) 
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, PlainTextDocument)  
  return (docs)
}

strings <- c("Hello, my name is George.", "And for all those who are defending");
minedText <- applyTM(strings)