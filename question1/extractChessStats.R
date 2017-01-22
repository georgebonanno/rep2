source("chessDataSource.R")

# extracts the statistics needed for visualisation by issuing queries to the
# sql lite database found in db/chess.db

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

numberOfMovesCount <- 'select substr(date_of_game,1,4) game_yr,move_count from games'

yearlyGamesQuery <- 'select substr(g.date_of_game,1,4) game_yr,count(*) number_of_games from games g 
group by substr(g.date_of_game,1,4)'


# executes all queries and returns a list with their result.
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
    tryCatch({
      dbDisconnect(con)
    },error=function(errorMessage){
      print("failed to close connection with error: "+errorMessage)
    })
    
  })  
}

chessStats <- retrievefirstWinnerMoveCount()