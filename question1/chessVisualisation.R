library(RColorBrewer)
library(ggplot2)

# builds the visualisations using the statistics obtained by running
# extractChessStats.R.


# returns the count of the wins, losses and draws
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

# returns 5 years ranges between minYr and maxYr. The cut
# function could have been used instead...
generateBuckets <- function(minYr,maxYr,yr) {
  rangeMin<- minYr+((floor((yr-minYr)/5))*5)
  rangeMax <- rangeMin+5
  if (rangeMax > 2017) {
    rangeMax<-2017
  }
  return(paste(rangeMin,"-",rangeMax))
}

# places the games movesInGame inside a given date range (determined)
# from the games event
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

# builds a box plot with the number of moves of the games in
# 5 year ranges.
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


# builds a heat map to show the most common winning first moves.
winningMovesHeatMap <- function() {
  firstWinnerCnt <- chessStats$firstWinnerMoveCount
  ggplot(data=firstWinnerCnt,aes(x=rw,y=col))+
    geom_tile(aes(fill=move_count)) + ylab("row") +
    xlab("column") +
    ggtitle("Most Common Starting Move of Winner Heat Map") +
    theme_classic()
}


# plots the number of games played every year
plotYearlyGames <- function() {
  yGames <- chessStats$yearlyGames
  yGames$yearDate <- as.Date(yGames$game_yr,"%Y")
  ggplot(yGames, aes(x=yearDate, y=number_of_games)) + geom_line() + theme_classic() +
    scale_x_date(date_breaks="2 years") +
     xlab("year") + ylab("Games Per Year") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("A Time series of yearly game count") 
}


