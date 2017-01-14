library(RColorBrewer)

source("chessDataSource.R")

firstWinnerMoveQuery <- '
  select * from (
  	select first_move,
  	       substr(first_move,length(first_move)-1,1) rw,
  	       substr(first_move,length(first_move),1) col,count(*) move_count from games
  		where length(first_move) > 0
  	group by first_move
  )
  order by move_count desc
';

resultCount <- '
  select result result_type,count(*) cnt from games
  group by result
';

numberOfMovesCount <- 'select move_count from games'

retrievefirstWinnerMoveCount <- function() {
  #initialise con to NA to allow finally 
  #block to close con only if a connection
  #was assigned to it
  con <- NA 
  tryCatch({
    con <- dbChessConnection();
    firstWinnerMoveCount <- executeAndGetAllRows(con,firstWinnerMoveQuery)
    resultCount <- executeAndGetAllRows(con,resultCount)
    moveCount <- executeAndGetAllRows(con,numberOfMovesCount)
    
    return (list(firstWinnerMoveCount=firstWinnerMoveCount,
                 resultCount=resultCount,
                 moveCount=moveCount))
  },finally = {
    if (!is.na(con)) {
      dbDisconnect(con)
    }
  })  
}

chessStats <- retrievefirstWinnerMoveCount();
barplot(chessStats$resultCount$cnt,names.arg = chessStats$resultCount$result_type)

moveInGameBoxPlot <- function() {
  movesInGame <- chessStats$moveCount
  box <- ggplot(data=movesInGame, aes(x="move_count", y=move_count))
  box <- box + geom_boxplot() + ylab("number of moves") +
    ggtitle("Number of Moves Boxplot") +
     theme_classic()
  return(box)
}

formatWinnerMoveCount(chessStats$firstWinnerMoveCount)

winningMovesHeatMap <- function() {
  ggplot(data=firstWinnerCnt,aes(x=col,y=rw))+geom_tile(aes(fill=move_count))
}
