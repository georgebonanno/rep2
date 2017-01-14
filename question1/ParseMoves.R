#moveNumber <- parseMoveNumber(i,moves)
#whiteMove <- parseWhiteMove(i,moves)
#blackMove <- parseBlackMove(i,moves)

indexOf <- function(str,i) {
  return (substring(str,i,i))
}

isDigit <- function(n) {
  return ((n >= '0') & (n <= '9'))
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
    stop(paste("'.' expected after step number",moveNumber, "and not",currentChar,'at position',i,"for moves",moves))
  }
  return(list(number=moveNumber,lookahead=i))
} 


parseMove <- function(i,moves) {
  while(i <= str_length(moves) && indexOf(moves,i) == ' ') {
    i <- i+1
  }
  move<-"";
  check<-FALSE
  while(!check && i <= str_length(moves) && indexOf(moves,i) != ' ') {
    move <- paste(move,indexOf(moves,i),sep="");
    
    if (indexOf(moves,i) == '#'
        || indexOf(moves,i) == '+') {
      #move ends if check or checkmate symbol found
      check=TRUE
    } else if (isDigit(indexOf(moves,i))) {
        if (isDigit(indexOf(moves,i+1))) {
          #when current char is a number and the next is a number
          #the last number pertains to the next move number
          #e.g. e616.
          check=TRUE
        }
    }
    i <- i+1
  }
  
  return(list(moveRead=move,lookahead=i));
}

lastMoveIndex <- function(moves,endResult) {
  lastIndex <- str_length(moves)
  if (endsWith(moves,endResult)) {
    lastIndex <- lastIndex-str_length(endResult)
    while(indexOf(moves,lastIndex) == " ") {
      lastIndex<-lastIndex - 1
    }
  } else {
    stop(paste("result '",endResult,"' not found at end of these moves:",moves))
  }
  return(lastIndex)
}

moveNumberFound <- function(i,moves,lastIndex) {
  numFound <- FALSE
  while(i<= lastIndex & indexOf(moves,i)==' ') {
    i<-i+1
  }
  
  twoDigitsFound <- FALSE
  while(!twoDigitsFound && i<= lastIndex & indexOf(moves,i)!=' '&&indexOf(moves,i)!='.' && indexOf(moves,i)!='='
        && indexOf(moves,i) != '+') {
    twoDigitsFound <- isDigit(indexOf(moves,i)) && isDigit(indexOf(moves,i+1))
    i<-i+1
  }
  numFound <- !twoDigitsFound && (indexOf(moves,i)=='.' || indexOf(moves,i)=='+' && indexOf(moves,i) == '=')
  return(numFound)
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
      if (moveNumberFound(i,moves,lastIndex)) {
        #there are instances where the 
        #black and white are not separated by a space
        #in such cases the move string is read entirely by the 
        #in the white move
        blackMove=list(moveRead="")
      } else {
        blackMove <- parseMove(i,moves)  
        i <- blackMove$lookahead  
      }
      
      parsedMoves[[moveNumber$number]] <- c(whiteMove$moveRead,
                                            blackMove$moveRead)
    }
  }
  return(parsedMoves)
}

parseMoves("1.Nf3 d5 2.c4 c6 3.e3 g6 4.Nc3 Bg7 5.d4 Nf6 6.h3 O-O 7.Bd3 Nbd7 8.O-O dxc4 9.Bxc4 c5 10.a4 cxd4 11.exd4 Nb6 12.Bb3 Nbd5 13.Re1 e6 14.Bg5 h6 15.Bh4 g5 16.Bg3 b6 17.h4 g4 18.Ne5 h5 19.Nc6 Qd7 20.Nxd5 Nxd5 21.Bxd5 exd5 22.Ne7+ Kh8 23.Rc1 Ba6 24.Rc7 Qd8 25.Qd2 f6 26.Nc6 Qxc7 27.Bxc7 Rac8 28.Qf4 Rf7 29.Re7 1-0*","*")