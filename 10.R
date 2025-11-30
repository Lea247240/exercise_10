rm(list=ls())

#Cesta:
setwd('V:/MPA_PRG/exercise_10')
setwd('D:/VUT/4-5rocnik/moje/MPA-PRG/exercise_10') # home

# Suffix Trees

### Task 1
# In R, implement a function `SuffixArray()` to create a suffix array from a string.

# Input:
  # A `DNAString` object .

# Output:
  # A vector of integers.

library(Biostrings)


text <- DNAString('GCATCATA') # + -> A is dolar
vek <- c()
len <- length(text)

for (i in (1:len)){
  #print(text[i:len])
  suffix <- text[i:len]
  vek <-c(vek,suffix)
}

vek
vek <- DNAStringSet(vek)
vek

SA <- order(vek)
SA



#..............all function........................
SuffixArray1 <- function(text){
  vek <- c()
  len <- length(text)
  
  for (i in (1:len)){
    #print(text[i:len])
    suffix <- text[i:len]
    vek <-c(vek,suffix)
    
  }
  # to DNAstring
  vek <- DNAStringSet(vek)
  
  # sorted with order
  SA <- order(vek)
  return(SA)
}

library(Biostrings)
SuffixArray1(text = DNAString('GCATCATA')) #$ -> A at the end

#..............all function........................
#with +

add_sentinel <- function(text) {
  # from DNAString to  BString
  BString(paste0(as.character(text), "+"))
}

SuffixArray <- function(text) {
  text <- add_sentinel(text)           # BString s "+"
  m <- nchar(text)
  
  suffixes <- vector("list", m)
  for (i in 1:m) {
    suffixes[[i]] <- subseq(text, i, m)
  }
  
  ss <- BStringSet(suffixes)
  SA <- order(ss)                      
  return(as.integer(SA))
}

library(Biostrings)
SuffixArray(text = DNAString('GCATCAT')) #$ -> A at the end

### Task 2
# In R, implement a function `InverseSuffixArray()` to create an inverse suffix array from a suffix array.

# Input:
  # A vector of integers representing suffix array.

# Output:
  # A vector of integers.

#ISA[SA[i]] = i
ISA <- order(SA)

#..............all function........................
InverseSuffixArray <- function(SA){
  ISA <- order(SA)
  return(ISA)
}
InverseSuffixArray(SA = c(8,6,3,5,2,1,7,4)) # SA = c(8,6,3,5,2,1,7,4)


### Task 3
# In R, implement a function `LCPArray()` according to pseudocode.

# Input:
  # `text` A `DNAString` representing analyzed string.
  # `SA` A vector of integers representing a suffix array.
  # `ISA` A vector of integers representing an inverse suffix array.

# Output:
  # `LCP` A vector of integers.

#############################################
#LCPArray(text, SA, ISA)
#1   LCP[1] <- -1
#2   LCP[m + 1] <- -1
#3   l <- 0
#4   for i <- 1 to m
#5     j <- ISA[i]
#6     if j > 1 then
#7       k <- SA[j - 1]
#8       while text[k + l] = text[i + l] do
#9         l <- l + 1
#10      LCP[j] <- l
#11      l <- max{l - 1, 0}
#12  return LCP
###########################################

#**Hint:** 
#  The text will be indexed at *m* + 1 position, that does not exist. Add one character at the end of the text
#(in general use `$`, for `DNAString` in R use `+`).


text <- str1
SA <- order
ISA <- sorted


m <- length(text)
LCP <- c(numeric(m))
LCP


# experiment
LCP[1]<- -1
LCP[m + 1] <- -1
l <- 0

for (i in (1:m)){
  j <- ISA[i]
  if (j > 1){
    k <- SA[j-1]
    while (text[k+l] == text[i+l]){
      l <- l + 1
    }
    LCP[j] <- l
    l <- max((l - 1), 0)
  } 
}
print(LCP)

#..............all function........................

LCPArray <- function(text, SA, ISA){
  
  # initialization
  text <- c(text, "+")
  m <- length(text) - 1
  LCP <- integer(m + 1)
  
  # pseudo code
  LCP[1]<- -1
  LCP[m + 1] <- -1
  l <- 0
  for (i in (1:m)){
    j <- ISA[i]
    if (j > 1){
      k <- SA[j - 1]
      # check znak to znak
      while (substr(text, k + l, k + l) == substr(text, i + l, i + l)){ # (text[k + l] == text[i + l])
        l <- l + 1
      }
      LCP[j] <- l
      l <- max((l - 1), 0)
    } 
  }
  return(LCP)
}

LCPArray(text=DNAString('GCATCAT'), SA = c(8,6,3,5,2,1,7,4) , ISA = c(6,5,3,8,4,2,7,1))

### Task 4
# In R, implement a function `BinarySearchSA()` according to the following pseudocode.

# Input:
# `pattern` A `DNAString` representing a pattern to be found.
# `text` A `DNAString` representing a text to be searched.
# `SA` A vector of integers representing a suffix array of `text`.

# Output:
  # A vector of two integers (the first and the last indexes of suffix array, where the pattern was found).

########################################################################
#BinarySearchSA(pattern, text, SA)
#1   minIndex <- 1
#2   maxIndex <- length (text)
#3   while minIndex < maxIndex
#4     midlIndex <- floor(minIndex + maxIndex) / 2
#5     if pattern <= suffix of text starting at position SA(midlIndex)
#6       maxIndex <- midlIndex
#7     else
#8       minIndex <- midlIndex + 1
#9   First <- minIndex
#10  maxIndex <- length(text)
#11  while maxIndex > minIndex
#12    midlIndex <- floor(minIndex + maxIndex) / 2
#13    if suffix of text starting at position SA(midlIndex) <= pattern
#14      minIndex <- midlIndex + 1
#15    else
#16      maxInd <- midlIndex
#17  Last <- maxIndex - 1
#18  if Last < First
#19    return('Pattern does not appear in text')
#20  else
#21    return First, Last
########################################################################

#help function suffix
suffix <- function(text, start) {
  return(text[start:length(text)])
}

#..............my function........................
BinarySearchSA <- function(pattern, text, SA){
  m <- length(text)
  minIndex <- 1
  maxIndex <- m
  
  while (minIndex < maxIndex){
    midlIndex <- (floor((minIndex + maxIndex) / 2))
    
    if (pattern <= SuffixArray(text,SA[midlIndex])){
      maxIndex <- midlIndex
    }
    else {
      minIndex <- midlIndex + 1
    }
  }
  
  First <- minIndex
  # find last
  maxIndex <- m
  while (minIndex < maxIndex){
    midlIndex <- floor((minIndex + maxIndex) / 2)
    
    if (SuffixArray(text) <= pattern){
      minIndex <- midlIndex + 1
    }
    else {
      maxIndex <- midlIndex 
    }
  }
  
  Last <- maxIndex - 1
  if (Last < First){
    return('Pattern does not appear in text')
  }
  else {
    return (c(First, Last))
  }
}

BinarySearchSA(pattern=DNAString('CAT'), text=DNAString('GCATCAT'), SA = SuffixArray(text))


#..............all function........................

library(Biostrings)

# initialization text a SA
text_seq <- DNAString('GCATCAT')
SA <- SuffixArray(text_seq)
pattern <- DNAString('CAT')

# function
BinarySearchSA <- function(pattern, text, SA){
  
  text_plus <- BString(paste0(as.character(text), "+"))
  pattern_str <- as.character(pattern)
  plen <- nchar(pattern_str)
  
  m <- length(SA)
  
  # First
  minIndex <- 1
  maxIndex <- m
  while (minIndex < maxIndex){
    midlIndex <- floor((minIndex + maxIndex) / 2)
    sufx <- substr(as.character(subseq(text_plus, SA[midlIndex], nchar(text_plus))), 1, plen)
    if (pattern_str <= sufx){
      maxIndex <- midlIndex
    } else {
      minIndex <- midlIndex + 1
    }
  }
  First <- minIndex
  
  # Last
  minIndex2 <- First
  maxIndex2 <- m
  while (minIndex2 < maxIndex2){
    midlIndex <- floor((minIndex2 + maxIndex2) / 2)
    
    sufx <- substr(as.character(subseq(text_plus, SA[midlIndex], nchar(text_plus))), 1, plen)
    if (sufx <= pattern_str){
      minIndex2 <- midlIndex + 1
    } else {
      maxIndex2 <- midlIndex
    }
  }
  Last <- maxIndex2 - 1
  
  if (Last < First){
    return('Pattern does not appear in text')
  } else {
    return(c(First, Last))
  }
}

# TEST
BinarySearchSA(pattern, text_seq, SA)

SA[4:5]  # start position pattern
substr(as.character(text_seq), SA[4], SA[4]+nchar(pattern)-1)
substr(as.character(text_seq), SA[5], SA[5]+nchar(pattern)-1)






