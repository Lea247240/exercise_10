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
SuffixArray <- function(text){
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
SuffixArray(text = DNAString('GCATCATA')) #$ -> A at the end


### Task 2
# In R, implement a function `InverseSuffixArray()` to create an inverse suffix array from a suffix array.

# Input:
  # A vector of integers representing suffix array.

# Output:
  # A vector of integers.

ISA <- order(SA)

#..............all function........................
InverseSuffixArray <- function(SA){
  ISA <- order(SA)
  return(ISA)
}
InverseSuffixArray(SA = c(8,6,3,5,2,1,7,4))


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

#..............all function........................?
LCPArray <- function(text, SA, ISA){
  
  # initialiozation
  m <- length(text)
  LCP <- c(numeric(m))
  
  # pseudocode
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
  return(LCP)
}

LCPArray(text=text, SA = SA, ISA = ISA)



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


BinarySearchSA <- function(pattern, text, SA){
  minIndex <- 1
  maxIndex <- length(text)
  while (minIndex<maxIndex){
    midlIndex <- (floor(minIndex + maxIndex) / 2)
    if (pattern <- text[SA(midlIndex)]){
      maxIndex <- midlIndex
    }
    else {
      minIndex <- midlIndex + 1
    }
  }
  First <- minIndex
  maxIndex <- length(text)
  while (maxIndex > minIndex){
    midlIndex <- (floor(minIndex + maxIndex) / 2)
    if (pattern <- text[SA(midlIndex)]){
      minIndex <- midlIndex
    }
    else {
      maxIndex <- midlIndex + 1
    }
  }
  Last <- maxIndex - 1
  if (Last < First){
    return('Pattern does not appear in text')
  }
  else {
    return (First, Last)
  }
}

BinarySearchSA(pattern, text=text, SA=SA)

