#moveNumber <- parseMoveNumber(i,moves)
#whiteMove <- parseWhiteMove(i,moves)
#blackMove <- parseBlackMove(i,moves)

indexOf <- function(str,i) {
  return (substring(str,i,i))
}

parseMoveNumber <- function(i,moves) {
  #parse the move step numer (\\d+)
  moveNumber <- '';
  currentChar <- indexOf(moves,i)
  while (currentChar >= '0' & currentChar <= '9') {
    moveNumber <- paste(moveNumber,currentChar,sep="")
    i<-i+1
    currentChar <- indexOf(moves,i)
  }
  if (currentChar == '.') {
    i <- i+1
  } else {
    stop(paste("'.' expected after step number",moveNumber, "and not",currentChar,'at position',i))
  }
  return(list(number=moveNumber,lookahead=i))
} 


parseMove <- function(i,moves) {
  while(i <= str_length(moves) && indexOf(moves,i) == ' ') {
    i <- i+1
  }
  move<-"";
  while(i <= str_length(moves) && indexOf(moves,i) != ' ') {
    move <- paste(move,indexOf(moves,i),sep="");
    i <- i+1
  }
  
  return(list(moveRead=move,lookahead=i));
}

lastMoveIndex <- function(moves,endResult) {
  lastIndex <- str_length(moves)
  if (endResult != "*") {
    if (endsWith(moves,endResult)) {
      lastIndex <- lastIndex-str_length(endResult)
      while(indexOf(moves,lastIndex) == " ") {
        lastIndex<-lastIndex - 1
      }
    } else {
      stop(paste("result '",endResult,"' not found at end of these moves:",moves))
    }
  }
  return(lastIndex)
}

parseMoves <- function(moves,endResult) {
  #move pattern consists of a number, space followed
  #by the steps of each players separated by steps
  
  comment<-FALSE
  i <- 1;
  lastIndex <- lastMoveIndex(moves,endResult)
  parsedMoves <- list()
  while (i < lastIndex) {
    if (indexOf(moves,i) == '{') {
      comment <- TRUE
      i<-i+1;
    }
    while (i < str_length(moves) & comment & indexOf(moves,i) != '}') {
      i <- i+1
    }
    if (indexOf(moves,i) == '}') {
      comment <- FALSE
    } else {
      while(indexOf(moves,i) == ' ') {
        #skip whitspace
        i <- i+1;
      }
      moveNumber <- parseMoveNumber(i,moves)
      i <- moveNumber$lookahead
      whiteMove <- parseMove(i,moves)
      i <- whiteMove$lookahead
      blackMove <- parseMove(i,moves)
      i <- blackMove$lookahead
      parsedMoves[[moveNumber$number]] <- c(whiteMove$moveRead,
                                      blackMove$moveRead)
    }
  }
  return(parsedMoves)
}

parseMoves("1.Nf3 Nf6 2.g3 b6 3.Bg2 Bb7 4.O-O e6 5.d3 d5 6.Nbd2 Be7 7.e4 c5 8.e5 Nfd7 9.Re1 Nc6 10.h4 Qc7 11.Qe2 h6 12.h5 Nb4 13.Nf1 c4 14.d4 c3 15.Ne3 Ba6 16.Qd1 cxb2 17.Bxb2 Rc8 18.Qd2 b5 19.a4 Nb6 20.Ba3 Qc3 21.Qxc3 Rxc3 22.Bxb4 Bxb4 23.Reb1 Rxe3 24.fxe3 Bc3 25.axb5 Bxa1 26.Rxa1 Bxb5 27.Rxa7 Nc8 28.Rb7 Ba6 29.Rb8 Kd7 30.Bf1 Kc7 31.Rb3 Bxf1 32.Kxf1 Nb6 33.Nd2 Ra8 34.Ke2 Ra1 35.Rb1 Ra2 36.Kd3 Ra3+ 37.Rb3 Ra1 38.Rb1 Ra3+ 39.Nb3 Nd7 40.Rf1 f6 41.exf6 Nxf6 42.g4 Kd6 43.Rg1 Ne4 44.Ra1 Rxa1 45.Nxa1 Nf2+ 46.Ke2 Nxg4 47.Nb3 Nf6 48.Kf3 Nxh5 49.e4 dxe4+ 50.Kxe4 Nf6+ 51.Kd3 h5 52.c4 h4 53.Ke3 g5 54.Nd2 g4 55.Kf4 g3 56.Nf3 g2 57.Kg5 h3 58.Kxf6 h2 59.c5+ Kd5 0-1","0-1")
