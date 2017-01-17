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

numberOfMovesCount <- 'select substr(date_of_game,1,4) game_yr,move_count from games'

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
    tryCatch({
      dbDisconnect(con)
    },error=function(errorMessage){
       print("failed to close connection with error: "+errorMessage)
    })
      
  })  
}

chessStats <- retrievefirstWinnerMoveCount();

resultCountBarPlot <- function() {
  resultCountPlot <- 
    ggplot(data=chessStats$resultCount,aes(x=result_type,y=cnt))+
    geom_bar(stat="identity",aes(fill=result_type)) +
    ylab("number of occurences") +
    xlab("result outcome") +
    ggtitle("Number of results per result type") +
    theme_classic();
  
  return(resultCountPlot);
}

generateBuckets <- function(minYr,maxYr,yr) {
  rangeMin<- minYr+((floor((yr-minYr)/5))*5)
  rangeMax <- rangeMin+5
  if (rangeMax > 2017) {
    rangeMax<-2017
  }
  return(paste(rangeMin,"-",rangeMax))
}

placeInBuckets <- function(movesInGame) {
  movesInGame$game_yr <- as.numeric(movesInGame$game_yr)
  minYear <- floor(min(movesInGame$game_yr)/5)*5
  maxYear <- floor(max(movesInGame$game_yr)/5)*5
  if (max(movesInGame$game_yr) %% 5 > 0) {
    maxYear <- maxYear+1
  }
  f <- function(yr) {
    return(generateBuckets(minYear,maxYear,yr))
  }

  movesInGame$yearRange <- sapply(X=movesInGame$game_yr,FUN = f)
  
  return(movesInGame)
}

moveInGameBoxPlot <- function() {
  movesInGame <- chessStats$moveCount
  movesInGame <- placeInBuckets(movesInGame)
  box <- ggplot(data=movesInGame, aes(x="move_count", y=move_count))
  box <- box + geom_boxplot(aes(fill=yearRange)) + 
    ylab("number of moves") +
    xlab("5 year group") +
    ggtitle("Box Plot of number of Moves to complete chess game") +
     theme_classic()
  return(box)
}

winningMovesHeatMap <- function() {
  firstWinnerCnt <- chessStats$firstWinnerMoveCount
  ggplot(data=firstWinnerCnt,aes(x=rw,y=col))+
    geom_tile(aes(fill=move_count)) + ylab("row") +
    xlab("column") +
    ggtitle("Most Common Starting Move of Winner Heat Map") +
    theme_classic()
}

plotYearlyGames <- function() {
  yGames <- chessStats$yearlyGames
  yGames$yearDate <- as.Date(yGames$game_yr,"%Y")
  ggplot(yGames, aes(x=yearDate, y=number_of_games)) + geom_line() + theme_classic() +
     xlab("year") + ylab("Games Per Year") + 
    ggtitle("A Time series of yearly game count") 
    
}
