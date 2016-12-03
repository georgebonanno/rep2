library("RSQLite")

dbChessConnection <- function() {
  con <- dbConnect(RSQLite::SQLite(), dbname="db/chess.db")
  return(con)
}

executeAndGetAllRows <- function(conn,query) {
  rows <- tryCatch({
    q <- dbSendQuery(conn,query)
    rows <- fetch(q)
    rows;
  },finally= {
    dbClearResult(q)  
  })
  return(rows)
  
}