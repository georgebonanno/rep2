library(stringr)

pastePrint <- function(...,sepr=" ") {
  print(paste(list(...),sep = sepr))
}


readPgnFile <- function(path) {
  #based on https://www.r-bloggers.com/read-line-by-line-of-a-file-in-r/
  #reads all the games from a given path
  con <- file(description=path,open="r");
  
  i <- 0;
  parseTagPairs = TRUE;
  pgnDoc = list()
  tagPairs = list()
  while((length(line <- readLines(con,n=1,encoding="UTF-8"))) > 0) {
    i <- i+1
    if (parseTagPairs) {
      if (startsWith(line,"[")) {
        tagPattern <- "\\[([^ ]+) \"([^\"]+)\""
        m <- str_match(line,tagPattern)
        if (length(m) < 3) {
          stop(paste("tag pair line",line,"does not match pattern",tagPattern))
        } else {
          tagPairs[[m[,2]]] <- m[,3]
        }
        pastePrint(line)
      } else {
        pastePrint("not tag pair",line)
        parseTagPairs=FALSE
      }
    } else {
      pastePrint("moves: ",line)
    }
  }
  pgnDoc[["TagPairs"]] <- tagPairs
  pgnDoc[["Moves"]] <- list()
  pastePrint("number of lines read: ",i)
  close(con)
  return (pgnDoc)
}

game <- readPgnFile('test2.txt')
