#MonaDistMat
# The Mona Clustering algorithm is implemented in R.  This module
# implementation attempts to port the implementation to tidyverse
# P:\Barb_Harold\Business\Hensky\projects\Mona\code  HHmona.r
# 2005_Kaufman_Finding-Groups-in-Data-An-Introduction-to-Cluster-Analysis.pdf
# raw observations will be rows each column is a binary feature

library(dplyr)
library(tidyverse)


dist2calc <- function(ColA , ColB) {
  # calculate the similarity between two column vectors
  print('***line 13')
  ColA <- tibble(ColA= ColA)
  ColB <- tibble(ColB = ColB)
  inTib <- cbind(ColA,ColB)
  print(inTib)
  workTib <- as.tibble(inTib,.name_repair = "minimal") %>%
    mutate(allzero = ifelse(((ColA == 0) & (ColB == 0)),TRUE,FALSE),
           onezero = ifelse(((ColA == 1) & (ColB == 0)),TRUE,FALSE),
           zeroone = ifelse(((ColA == 0) & (ColB == 1)),TRUE,FALSE),
           allone  = ifelse(((ColA == 1) & (ColB == 1)),TRUE,FALSE)) %>%
    summarize(allzero = sum(allzero),
              onezero = sum(onezero),
              zeroone = sum(zeroone),
              allone  = sum(allone))
  # return estimate between two vectors 
  return(workTib)
}

# calculate closeness using Mona forumalue
getscore <-function(resvec) {
  # calc score
  print('resvec=')
  print(resvec)
  retval = resvec$allzero * resvec$allone -  resvec$onezero * resvec$zeroone
  return(retval)
}

# return matrix of distances between the columns
# return square symmetric matrix k by k
getmat <- function(themat) {
  print('*** Line 39')
  print(themat)
  k <- ncol(themat)
  print('print 42')
  print(k)
  sprintf("***Line 43 There are %d columns ",k)
  print('*** 45')
  retmat <- matrix(0,nrow=k,ncol=k)
  print('*** 47')
  for(irow in 1:k){
    for(icol in 1:k) {
      thecol <- themat[,icol]
      print(thecol)
      therow <- themat[,irow]
      print(therow)
      print(c(irow,icol))
      print('*** 55')
      theval <- dist2calc(therow,thecol)
      print(theval)
      retmat[irow,icol] <- getscore(theval)
    }
  }
  print('*** at line 69')
  return(retmat)
}


# calculate a closeness matrix
# should be symetric with zeros on diagonal


# first 4 obs and second 4 are the same
JunkVecA <- c(1,0,1,0,1,0,1,0 ) # A and B perfect zero correlation
JunkVecB <- c(1,1,0,0,1,1,0,0 ) # B and C perfect correlation
JunkVecC <- c(1,1,0,0,1,1,0,0 ) 
JunkVecD <- c(1,1,1,0,1,1,1,0 ) 
JunkVecE <- c(0,1,0,1,0,1,0,1 )
junkTiba <- tibble(VecA = JunkVecA,VecB = JunkVecB,
                   VecC = JunkVecC,VecD = JunkVecD,
                   VecE = JunkVecE)
junkMat <- dist2calc(JunkVecA,JunkVecB)
print(junkMat)
junkScore <- getscore(junkMat)
# should be zero
print(junkScore)
# should be one
print(getscore(dist2calc(JunkVecB,JunkVecC)))

print('****** at 78')
print(junkTiba)
junkdist <- getmat(junkTiba) 
print(junkdist)  



#distlevel(themat,thelevel)
# calculate the distant matrix for a give level

#distmat(themat, thelevel)
# generate a distant matrix

