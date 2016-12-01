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
