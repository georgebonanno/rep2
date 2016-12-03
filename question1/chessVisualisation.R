library(RColorBrewer)

source("chessDataSource.R")

firstWinnerMoveQuery <- '
  select * from (
  	select first_move,count(*) move_count from games
  		where length(first_move) > 0
  	group by first_move
  )
  order by move_count desc
';

resultCount <- '
  select result result_type,count(*) cnt from games
  group by result
';

retrievefirstWinnerMoveCount <- function() {
  #initialise con to NA to allow finally 
  #block to close con only if a connection
  #was assigned to it
  con <- NA 
  tryCatch({
    con <- dbChessConnection();
    firstWinnerMoveCount <- executeAndGetAllRows(con,firstWinnerMoveQuery)
    resultCount <- executeAndGetAllRows(con,resultCount)
    
    return (list(firstWinnerMoveCount=firstWinnerMoveCount,resultCount=resultCount))
  },finally = {
    if (!is.na(con)) {
      dbDisconnect(con)
    }
  })  
}


chessStats <- retrievefirstWinnerMoveCount();
barplot(chessStats$resultCount$cnt,names.arg = chessStats$resultCount$result_type)
