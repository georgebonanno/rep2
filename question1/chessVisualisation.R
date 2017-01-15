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

yearlyGamesQuery <- 'select substr(g.date_of_game,1,4) game_yr,count(*) number_of_games from games g 
  	                 group by substr(g.date_of_game,1,4)'

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
    yearlyGames <- executeAndGetAllRows(con,yearlyGamesQuery)
    
    return (list(firstWinnerMoveCount=firstWinnerMoveCount,
                 resultCount=resultCount,
                 moveCount=moveCount,
                 yearlyGames=yearlyGames))
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


winningMovesHeatMap <- function() {
  ggplot(data=firstWinnerCnt,aes(x=col,y=rw))+geom_tile(aes(fill=move_count))
}

plotYearlyGames <- function() {
  yGames <- chessStats$yearlyGames
  yGames$yearDate <- as.Date(yGames$game_yr,"%Y")
  ggplot(yGames, aes(x=yearDate, y=number_of_games)) + geom_line() + theme_classic() +
     xlab("year") + ylab("Games Per Year") + 
    ggtitle("A Time series of yearly game count") 
    
}
