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
    stop(paste("'.' expected after step number",moveNumber, "and not",currentChar,'at position',i))
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
    
    print(indexOf(moves,i))
    if (indexOf(moves,i) == '#'
        || indexOf(moves,i) == '+') {
      #move ends if check or checkmate symbol found
      check=TRUE
      print("checked!!!")
    } else if (isDigit(indexOf(moves,i))) {
        print("check is next digit is number...")
        if (isDigit(indexOf(moves,i+1))) {
          #when current char is a number and the next is a number
          #the last number pertains to the next move number
          #e.g. e616.
          check=TRUE
          print("number!!!")
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
  print(paste("moveNumberFound",numFound,indexOf(moves,i),i,lastIndex))
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

parseMoves("1.d4 e6 2.Nf3 b6 3.e4 d5 4.Bd3 Nf6 5.e5 Nfd7 6.O-O Be7 7.c4 c6 8.Nc3 Na6 9.a3 Nc7 10.b4 Ba6 11.c5 Bxd3 12.Qxd3 a5 13.Bd2 O-O 14.Rfe1 Qc8 15.Bg5 Re816.Bxe7 Rxe7 17.Ng5 f5 18.exf6 gxf6 19.Nh3 b5 20.Qg3+ Kf7 21.Qf4 Nf8 22.Qh6 Ne8 23.Nf4 Kg8 24.Rab1 axb4 25.axb4 Ra3 26.Re3 Rg7 27.Ncxd5 Rxe3 28.Nxe3 Qd7 29.Rd1 Qa7 30.Qh5 Nc7 31.Qf3 Nd5 32.Nfxd5 exd5 33.Qxf6 Qa4 34.Qxc6 Qb3 35.Kf4+ 1-0","1-0");
