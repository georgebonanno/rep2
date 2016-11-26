library(stringr)

pastePrint <- function(...,sepr=" ") {
  print(paste(list(...),sep = sepr))
}

parseMoves <- function(moves,endResult) {
  #move pattern consists of a number, space followed
  #by the steps of each players separated by steps
  movePattern="(\\d+)\\.([^ ]+) ([^ ]+) "
  matches <- str_match_all(moves,pattern = movePattern)
  matches <- matches[[1]]
  rowCount <- dim(matches)[1]
  moves <- list()
  if (rowCount > 0) {
    for(i in seq(1,rowCount)) {
      movePlyr1 <- matches[i,3]
      movePlyr2 <- matches[i,4]
      moves[[i]] <- c(movePlyr1, movePlyr2)
    }
  }
  return(moves)
}

readPgnGame <- function(con) {
  i <- 0;
  parseTagPairs = TRUE;
  pgnDoc <- list()
  tagPairs <- list()
  allMoves <- "";
  readMoveLine <- TRUE
  print(paste("start of reading....",parseTagPairs,readMoveLine))
  while(readMoveLine & (length(line <- readLines(con,n=1,encoding="UTF-8"))) > 0) {
    print(paste("line read: ",line,parseTagPairs))
    i <- i+1
    if (parseTagPairs) {
      print("in parse tag pairs");
      if (startsWith(line,"[")) {
        tagPattern <- "\\[([^ ]+) \"([^\"]+)\""
        m <- str_match(line,tagPattern)
        if (length(m) < 3) {
          stop(paste("tag pair line",line,"does not match pattern",tagPattern))
        } else {
          #store tag in tagPairs
          tagPairs[[m[,2]]] <- m[,3]
        }
        pastePrint(line)
      } else {
        pastePrint("not tag pair",line)
        parseTagPairs=FALSE
      }
    } else {
      print(paste("not tags",line,str_length(line)))
      if (str_length(line) == 0) {
        readMoveLine <- FALSE
      }
      if (readMoveLine & !parseTagPairs) {
        allMoves <- paste(allMoves,line,sep="")
      }
    }
  }
  print(paste("lines read: ",i))
  if (length(tagPairs) > 0 & str_length(allMoves) > 0) {
    pgnDoc[["TagPairs"]] <- tagPairs
    pgnDoc[["Moves"]] <- parseMoves(allMoves)
  }
  return(pgnDoc)
}

readPgnFile <- function(path) {
  #based on https://www.r-bloggers.com/read-line-by-line-of-a-file-in-r/
  #reads all the games from a given path
  pgnDocs <- tryCatch({
    con <- file(description=path,open="r");
    pgnDocs <- list()
    i <- 1
    while(length(pgnDoc <- readPgnGame(con)) > 0) {
      pgnDocs[[i]] <- pgnDoc
      i <- i+1
    }
    return(pgnDocs)
  },finally = {
    close(con)
  });
    
  return (pgnDoc)
}

game <- readPgnFile('test.txt')
