library(stringr)
library("RSQLite")
source("ParseMoves.R")

pastePrint <- function(...,sepr=" ") {
  print(paste(list(...),sep = sepr))
}


readPgnGame <- function(con) {
  i <- 0;
  parseTagPairs = TRUE;
  pgnDoc <- list()
  tagPairs <- list()
  allMoves <- "";
  readMoveLine <- TRUE
  LINEBUFFER <- 1
  #print(paste("start of reading....",parseTagPairs,readMoveLine))
  while(readMoveLine & (length(line <- readLines(con,n=LINEBUFFER,encoding="UTF-8"))) > 0) {
    i <- i+1
    if (parseTagPairs) {
      #print("in parse tag pairs");
      if (startsWith(line,"[")) {
        tagPattern <- "\\[([^ ]+) \"([^\"]+)\""
        m <- str_match(line,tagPattern)
        if (length(m) < 3) {
          stop(paste("tag pair line",line,"does not match pattern",tagPattern))
        } else {
          #store tag in tagPairs
          tagPairs[[m[,2]]] <- m[,3]
        }
        #pastePrint(line)
      } else {
        #pastePrint("not tag pair",line)
        parseTagPairs=FALSE
      }
    } else {
      #print(paste("not tags",line,str_length(line)))
      if (str_length(line) == 0) {
        readMoveLine <- FALSE
      }
      if (readMoveLine & !parseTagPairs) {
        allMoves <- paste(allMoves,line,sep="")
      }
    }
  }
  #print(paste("lines read: ",i))
  if (length(tagPairs) > 0 & str_length(allMoves) > 0) {
    pgnDoc[["TagPairs"]] <- tagPairs
    pgnDoc[["Moves"]] <- parseMoves(allMoves,tagPairs$Result)
  }
  return(pgnDoc)
}

readPgnFile <- function(path,gameProcessor,dbConn) {
  #based on https://www.r-bloggers.com/read-line-by-line-of-a-file-in-r/
  #reads all the games from a given path
  pgnDocs <- tryCatch({
    con <- file(description=path,open="r");
    pgnDocs <- list()
    i <- 1
    while(length(pgnDoc <- readPgnGame(con)) > 0) {
      gameProcessor(pgnDoc,dbConn)
      i <- i+1
    }
    return(pgnDocs)
  },finally = {
    close(con)
  });
    
  return (pgnDoc)
}

gameCounter <<- 0
gProcessor <- function(gameDetails,con) {
  tryCatch(
    {
      
      gameCounter <<- gameCounter + 1
      print(paste(gameDetails$TagPairs$Round,gameCounter))
      storeGame(con,gameCounter,gameDetails)
      if (gameCounter %% 100 == 0) {
        gc()
      }
    },
    finally = {
    }
  )

}

findFirstMoveOfWinning <- function(game) {
  res <- game$TagPairs$Result
  if (length(game$Moves) > 0) {
    firstMove <- game$Moves[[1]]
    if (res == "1-0") {
      winningMove <- firstMove[1]
    } else if (res == "0-1") {
      winningMove <- firstMove[2]
    } else {
      winningMove <- ""
    }
  } else {
    winningMove <- ""
  }
  return (winningMove)
}

storeGame <- function(conn,index,game) {
  firstMove <- findFirstMoveOfWinning(game)
  insertQuery = paste("INSERT INTO games (game_id,event,site,result,first_move) VALUES (",
                      index,",'",
                      game$TagPairs$Event,"',\"",
                      game$TagPairs$Site,"\",'",
                      game$TagPairs$Result,"','",
                      firstMove,"')",
                      sep = "");
  
  
  print(paste("first move:",game$Moves[[1]][1],game$Moves[[1]][2]))
  print(paste("insertquery: ",insertQuery))
  tryCatch({
    q <- dbSendQuery(conn,insertQuery)
    fetch(q,n=-1)  
  }, finally = {
    dbClearResult(q)  
  })
}

loadPgnFile <- function(fileName) {
  con <- NULL
  tryCatch({
    con = dbConnect(RSQLite::SQLite(), dbname="chess.db")
    readPgnFile(fileName,gProcessor,con)
  },
  finally = {
    if (!is.null(con)) {
      dbDisconnect(con)
    }
  })
}

#loadPgnFile(fileName)

loadPgnFile('temp.txt')

