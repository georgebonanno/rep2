library(stringr)

# chess.R parses the pgn files passed as argument to the script. Several features
# of each game parsed are stored in an sqllite database located at db/chess.db. 
# It is assumed that chess.db already exists.

source("ParseMoves.R")
source("chessDataSource.R")

# prints a set of string together separated by a space
pastePrint <- function(...) {
  print(paste(...,sep = " "))
}

# 
readNextLine <- function(con,bufferSize) {
  #read next line from buffered line
  #read next bufferSize line in buffer
  #and read again if all buffer read.
  if(bufferPos == -1 || bufferPos > length(buf)) {
    buf <<- readLines(con,n=bufferSize,encoding="UTF-8")  
    bufferPos <<- 1
  }
  if (length(buf) > 0) {
    nextLine=c(buf[bufferPos])
    bufferPos <<- bufferPos + 1
  } else {
    nextLine = c()
  }
  return(nextLine)
}

# reads lines from a buffer of 10000 lines (abstracted in readNextLine)
# and parses the starts tags of every game. It then takes all the moves
# and called parseMoves() to returns a structure representing the parsed
# moves. It finally returns a structure containing the tag and parsed moves.
readPgnGame <- function(con) {
  i <- 0;
  parseTagPairs = TRUE;
  pgnDoc <- list()
  tagPairs <- list()
  allMoves <- "";
  readMoveLine <- TRUE
  LINEBUFFER <- 10000
  while(readMoveLine & (length(line <- readNextLine(con,LINEBUFFER))) > 0) {
    i <- i+1
    if (parseTagPairs) {
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
        allMoves <- paste(allMoves,line,sep=" ")
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
    storeGames(dbConn)
    return(pgnDocs)
  },finally = {
    close(con)
  });
    
  return (pgnDoc)
}

# stores the list of chess games into the database if
# the list exceed STORE_BUF_SIZE. This buffering is done
# rather than insert every time a new game is parsed.
storeGames <- function(con,gameDetails=list()) {
  dbBegin(con)
  bufferedGameCount <- length(gamesToStore)
  if (length(gameDetails) > 0) {
    gamesToStore[[bufferedGameCount+1]] <<- gameDetails
  }
  if (length(gameDetails) == 0 || length(gamesToStore) >= STORE_BUF_SIZE) {
    for (i in 1:length(gamesToStore)) {
      
      gameCounter <<- gameCounter+1
      
      if (length(gameDetails) == 0) {
        print(paste("counter",gameCounter))
      } else {
        storeGame(con,gameCounter,gamesToStore[[i]]) 
      }
    }
    gamesToStore <<- list()
  } 
  dbCommit(con)
}

gamesToStore <<- list()

# the number of parsed held in a buffer before they are saved.
# the games are insert in the databse every time STORE_BUF_SIZE
# become 20.
STORE_BUF_SIZE <<- 20

# stores the game details in the sqllite database
# using the connection con.
gProcessor <- function(gameDetails,con) {
  tryCatch(
    {
      storeGames(con,gameDetails)
      if (gameCounter %% 100 == 0) {
        gc()
      }
    },
    finally = {
    }
  )

}


# extract the first winning move from the parsed game
findFirstMoveOfWinning <- function(game) {
  if (!is.na(game[["TagPairs"]]) && !is.na(game$TagPairs[["Result"]])) {
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
  } else {
    winningMove <- ""
  }
  return (winningMove)
}

# inserts the information (variable game) in the games table 
# using connection conn. The record is identified by the 
# unique number index. 
storeGame <- function(conn,index,game) {
  firstMove <- findFirstMoveOfWinning(game)
  moveCount <- length(game$Moves)
  whiteCastlingMove <- game$Moves[["castlingMoves"]][1]
  blackCastlingMove <- game$Moves[["castlingMoves"]][2]
  if(is.na(game$TagPairs$Date)) {
    gameDate <- ""
  } else {
    gameDate <- str_replace_all(game$TagPairs$Date,"\\.","_")  
  }
  
  insertQuery = paste("INSERT INTO games (game_id,date_of_game,event,site,",
                      "result,first_move,move_count,white_castling_num,",
                      "black_castling_num) VALUES (",
                      index,",'",
                      gameDate,"','",
                      game$TagPairs$Event,"',\"",
                      game$TagPairs$Site,"\",'",
                      game$TagPairs$Result,"','",
                      firstMove,"',",
                      moveCount,",",
                      whiteCastlingMove,",",
                      blackCastlingMove,
                      ")",
                      sep = "");
  
  tryCatch({
    q <- dbSendQuery(conn,insertQuery)
    fetch(q,n=-1)  
  }, finally = {
    dbClearResult(q)  
  })
}



nextGameIdToInsertWith <- function(conn) {
  # returns the next game id with which the next game that
  # is parsed from file is inserted in the games table.
  # The games tables is created if it does no exist.
  tabs <- dbListTables(conn);
  createTableQuery <- paste("create table games (",
                              "game_id int primary key,",
                              "event varchar(30),",
                              "date_of_game text,",
                              "site varchar(30),",
                              "result varchar(10),",
                              "first_move varchar(10),",
                              "move_count numeric,",
                              "white_castling_num numeric,",
                              "black_castling_num numeric",
                            ")")
  tableMissing <- (length(tabs[tabs=="games"]) == 0)
  if (tableMissing) {
    print("table 'games' will be created.")
    executeAndGetAllRows(conn,createTableQuery)
    count <- 1
  } else {
    q <- executeAndGetAllRows(conn,"select ifnull(max(game_id),0)+1 n from games")
    count <- q$n[1]
  }
  return(count)
}

# parses a pgn file and stores information as a record
# in the games table.
loadPgnFile <- function(fileName) {
  con <- NULL
  tryCatch({
    # get connection
    con <- dbChessConnection();
    #get next game number (primary key of record)
    gameCounter <<- nextGameIdToInsertWith(con)
    print(paste("starting inserting games from id ",gameCounter))
    
    # parse the pgn file
    readPgnFile(fileName,gProcessor,con)
  },
  finally = {
    if (!is.null(con)) {
      dbDisconnect(con)
    }
  })
}


args <- commandArgs(trailingOnly=TRUE)
print(paste("files to parse for chess games: ",args))

bufferPos<<--1
gc()
for (f in args) {
  print(paste("loading pgns from file",f))
  
  # parse the pgn file
  system.time(loadPgnFile(f))
}
gc()
