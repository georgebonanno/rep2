library("RSQLite")

# contains helper functions to open and query the database
# where games are stored.

dbChessConnection <- function() {
  con <- dbConnect(RSQLite::SQLite(), dbname="db/chess.db")
  return(con)
}

executeAndGetAllRows <- function(conn,query) {
  rows <- tryCatch({
    q <- dbSendQuery(conn,query)
    rows <- fetch(q)
    rows;
  },error=function(e){
    print(paste("error while executing",query))
  },finally= {
    dbClearResult(q)  
  })
  return(rows)
  
}