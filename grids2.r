#
# Grids for M. Wall's tests as 109*181 matrices.
# 
#
# Author: Luke Chong
# Date: 2016-02-01 17:16:50 PST
#
#a <- which(!is.na(grid.practice),arr.ind=TRUE)
#b <- NULL

#for (row in 1:nrow(a)) {
#  b <- rbind(b,c(-3-14*6 + 6*(a[row,2]-1),3+8*6 - 6*(a[row,1]-1),grid.practice[a[row,1],a[row,2]]))
#}
  
#dimnames(b) <- list(NULL,c("x","y","wave"))
#write.csv(b,"test.csv")

#######################################################################################################
# Table outlining coordinates of test locations and their corresponding growth pattern wave number
# 
# column 1: x coordinate
# column 2: y coordinate
# column 3; wave number
#
# NOTE: Coordinates must be entered in RE format
#######################################################################################################
grid.peripheral.coords <- matrix(c(
  -51,	9,	5,
  -51,	3,	6,
  -51,	-3,	6,
  -51,	-9,	5,
  -45,	15,	4,
  -45,	-15,	4,
  -39,	21,	3,
  -39,	9,	3,
  -39,	3,	4,
  -39,	-3,	4,
  -39,	-9,	3,
  -39,	-21,	3,
  -33,	15,	2,
  -33,	-15,	2,
  -33,	-27,	2,
  -27,	21,	1,
  -27,	-21,	1,
  -27,	-33,	3,
  -21,	33,	3,
  -21,	27,	2,
  -21,	-27,	2,
  -21,	-39,	4,
  -15,	-33,	3,
  -15,	-51,	1,
  -3,	33,	1,
  -3,	-39,	2,
  -3,	-51,	1,
  3,	33,	2,
  3,	-39,	1,
  3,	-51,	2,
  15,	-33,	3,
  15,	-51,	1,
  21,	33,	3,
  21,	27,	2,
  21,	-27,	2,
  21,	-39,	4,
  27,	21,	1,
  27,	-21,	1,
  27,	-33,	3,
  33,	27,	2,
  33,	15,	2,
  33,	-15,	2,
  33,	-27,	2,
  39,	21,	3,
  39,	9,	3,
  39,	3,	4,
  39,	-3,	4,
  39,	-9,	3,
  39,	-21,	3,
  39,	-39,	2,
  45,	27,	4,
  45,	15,	4,
  45,	-15,	4,
  45,	-27,	4,
  45,	-45,	1,
  51,	21,	3,
  51,	9,	3,
  51,	3,	4,
  51,	-3,	4,
  51,	-9,	3,
  51,	-21,	3,
  51,	-39,	2,
  57,	27,	4,
  57,	15,	2,
  57,	-15,	2,
  57,	-27,	4,
  63,	21,	3,
  63,	9,	1,
  63,	-9,	1,
  63,	-21,	3,
  63,	-33,	5,
  69,	15,	2,
  69,	3,	2,
  69,	-3,	2,
  69,	-15,	2,
  75,	21,	3,
  75,	-21,	3,
  75,	-33,	5,
  81,	9,	1,
  81,	-9,	5,
  81,	-15,	4,
  81,	-27,	4,
  87,	3,	2,
  87,	-3,	6
), nrow = 84,ncol=3,byrow=TRUE)

grid.30.2.coords <- matrix(c(
  -27,	9,	4,
  -27,	3,	4,
  -27,	-3,	4,
  -27,	-9,	4,
  -21,	15,	4,
  -21,	9,	3,
  -21,	3, 3,
  -21,	-3,	3,
  -21,	-9,	3,
  -21,	-15,	4,
  -15,	21,	4,
  -15,	15,	2,
  -15,	9,	2,
  -15,	3,	2,
  -15,	-3,	2,
  -15,	-9,	2,
  -15,	-15,	2,
  -15,	-21,4,
  -9,	27,	4,
  -9,	21,	3,
  -9,	15,	2,
  -9,	9,	1,
  -9,	3,	2,
  -9,	-3,	2,
  -9,	-9,	1,
  -9,	-15,	2,
  -9,	-21,	3,
  -9,	-27,	4,
  -3,	27,	4,
  -3,	21,	3,
  -3,	15,	2,
  -3,	9,	2,
  -3,	3,	2,
  -3,	-3,	2,
  -3,	-9,	2,
  -3,	-15,	2,
  -3,	-21,	3,
  -3,	-27,	4,
  3,	27,	4,
  3,	21,	3,
  3,	15,	2,
  3,	9,	2,
  3,	3,	2,
  3,	-3,	2,
  3,	-9,	2,
  3,	-15,	2,
  3,	-21,	3,
  3,	-27,	4,
  9,	27,	4,
  9,	21,	3,
  9,	15,	2,
  9,	9,	1,
  9,	3,	2,
  9,	-3,	2,
  9,	-9,	1,
  9,	-15,	2,
  9,	-21,	3,
  9,	-27,	4,
  15,	21,	4,
  15,	15,	2,
  15,	9,	2,
  15,	-9,	2,
  15,	-15,	2,
  15,	-21,	4,
  21,	15,	4,
  21,	9,	3,
  21,	3,	3,
  21,	-3,	3,
  21,	-9,	3,
  21,	-15,	4,
  27,	9,	4,
  27,	3,	4,
  27,	-3,	4,
  27,	-9,	4
), nrow = 74,ncol=3,byrow=TRUE)

grid.30.1.coords <- matrix(c(
  -30,	0,	5,
  -24,	12,	4,
  -24,	6,	4,
  -24,	0,	4,
  -24,	-6,	4,
  -24,	-12,	4,
  -18,	18,	4,
  -18,	12,	3,
  -18,	6,	3,
  -18,	0,	3,
  -18,	-6,	3,
  -18,	-12,	3,
  -18,	-18,	4,
  -12,	24,	4,
  -12,	18,	3,
  -12,	12,	2,
  -12,	6,	2,
  -12,	0,	2,
  -12,	-6,	2,
  -12,	-12,	2,
  -12,	-18,	3,
  -12,	-24,	4,
  -6,	24,	4,
  -6,	18,	3,
  -6,	12,	2,
  -6,	6,	1,
  -6,	0,	2,
  -6,	-6,	1,
  -6,	-12,	2,
  -6,	-18,3,
  -6,	-24,	4,
  0,	30,	5,
  0,	24,	4,
  0,	18,	3,
  0,	12,	2,
  0,	6,	2,
  0,	-6,	2,
  0,	-18,	3,
  0,	-24,4,
  0,	-30,	5,
  6,	24,	4,
  6,	18,	3,
  6,	12,	2,
  6,	6,	1,
  6,	0,	2,
  6,	-6,	1,
  6,	-12,	2,
  6,	-18,	3,
  6,	-24,	4,
  12,	24,	4,
  12,	18,	3,
  12,	12,	2,
  12,	-12,	2,
  12,	-18,	3,
  12,	-24,	4,
  18,	18,	4,
  18,	12,	3,
  18,	-12,	3,
  18,	-18,	4,
  24,	12,	4,
  24,	6,	4,
  24,	0,	5,
  24,	-6,	4,
  24,	-12,	4,
  30,	0,	5
), nrow = 65,ncol=3,byrow=TRUE)
  
grid.24.2.coords <- matrix(c(
  -27,	3,	4,
  -27,	-3,	4,
  -21,	9,	3,
  -21,	3,	3,
  -21,	-3,	3,
  -21,	-9,	3,
  -15,	15,	2,
  -15,	9,	2,
  -15,	3,	2,
  -15,	-3,	2,
  -15,	-9,	2,
  -15,	-15,	2,
  -9,	21,	3,
  -9,	15,	2,
  -9,	9,	1,
  -9,	3,	2,
  -9,	-3,	2,
  -9,	-9,	1,
  -9,	-15,	2,
  -9,	-21,	3,
  -3,	21,	3,
  -3,	15,	2,
  -3,	9,	2,
  -3,	3,	2,
  -3,	-3,	2,
  -3,	-9,	2,
  -3,	-15,	2,
  -3,	-21,	3,
  3,	21,	3,
  3,	15,	2,
  3,	9,	2,
  3,	3,	2,
  3,	-3,	2,
  3,	-9,	2,
  3,	-15,	2,
  3,	-21,	3,
  9,	21,	3,
  9,	15,	2,
  9,	9,	1,
  9,	3,	2,
  9,	-3,	2,
  9,	-9,	1,
  9,	-15,	2,
  9,	-21, 3,
  15,	15,	2,
  15,	9,	2,
  15,	-9,	2,
  15,	-15,	2,
  21,	9,	3,
  21,	3,	3,
  21,	-3,	3,
  21,	-9,	3
), nrow = 52,ncol=3,byrow=TRUE)

grid.practice.coords <- matrix(c(
  -33,  21,	1,
  -21, -21,	1,
  -15,	-3,	1,
   -3,	21,	1,
    3,	 3,	1,
   15, -21,	1,
   27,	21,	1,
   39,	-3,	1
), nrow=8, ncol=3, byrow=TRUE)

grid.G1.coords <- matrix(c(
   -8,  26,4,
  8,  26,4,
   -20,  20, 4,
   -12,  20, 3,
    -4,  20, 3,
   4,  20, 3,
  12,  20,3,
  20,  20, 4,
    -4,  14, 2,
  4,  14, 2,
  -20,  12, 3,
  -12,  12, 2,
 12,  12, 2,
 20,  12, 3,
   -8,   8, 1,
   -2,   8, 2,
  2,   8, 2,
  8,   8, 1,
 26,   8, 4,
  -26,   4, 4,
  -20,   4, 3,
  -14,   4, 2,
   -4,   4, 2,
  4,   4, 2,
 22,   4, 3,
   -8,   2, 2,
   -2,   2, 3,
  2,   2, 3,
  8,   2, 2,
   -8,  -2, 2,
   -2,  -2, 3,
  2,  -2, 3,
  8,  -2, 2,
  -26,  -4, 4,
  -20,  -4, 3,
  -14,  -4, 2,
   -4,  -4, 2,
  4,  -4, 2,
 22,  -4, 3,
  -8,  -8, 1,
   -3,  -9, 2,
  3,  -9, 2,
  8,  -8, 1,
 26,  -8, 4,
  -20, -12, 3,
  -12, -12, 2,
 12, -12, 2,
 20, -12, 3,
   -4, -14, 2,
  4, -14, 2,
  -20, -20, 4,
  -12, -20, 3,
   -4, -20, 3,
  4, -20, 3,
 12, -20, 3,
 20, -20, 4,
   -8, -26, 4,
  8, -26, 4
), nrow = 58,ncol=3,byrow=TRUE)
  
  
##############################################################################################
# Function which makes test patterns on a 109*181 matrix.
# Input: 
#   coords - table of test location coordinates and their corresponding wave number
#
# Output:
#   final test pattern to be fed into algorithm
##############################################################################################
testPattern <- function (coords) {
  grid.base <- matrix(NA,nrow=109,ncol=181,byrow=TRUE)
  
  for (row in 1:nrow(coords)) {
    cl <- 91 + coords[row,1]   #convert x coordinate to column number
    rw <- 55 - coords[row,2]   #convert y coordinate to row number
    grid.base[rw,cl] <- coords[row,3] # place wave number at the correct position in grid.base
  }
  return(grid.base)
}

##############################################################################################
# Function which calculates the distance between locations
# column 1: x coordinate
# column 2: y coordinate
# column 3: distance
#
##############################################################################################
findDist <- function (grid_coords,invert) {
  distList <- list()
  
  if (invert == TRUE) { # need to invert coordinates if testing LE
    grid_coords[,1] <- grid_coords[,1] * -1  
  }
  
  for (wave in 1:(max(grid_coords[,3] - 1))) {
    wave_coords <- grid_coords[which(grid_coords[,3] == wave),]
    next_wave_coords <- grid_coords[which(grid_coords[,3] == (wave+1)),]
    waveList <- list()
    for (rr in 1:nrow(wave_coords)) {
      x1 <- wave_coords[rr,1]
      y1 <- wave_coords[rr,2]
      next_one <- NULL
      for (row in 1:nrow(next_wave_coords)) {
        x2 <- next_wave_coords[row,1] 
        y2 <- next_wave_coords[row,2]
        distance <- round(sqrt((x2 - x1)^2 + (y2 - y1)^2))
        next_one <- rbind(next_one,c(x2,y2,distance))
      }
      waveList <- c(waveList,list(next_one))
    }
    names(waveList) <- apply(wave_coords,1,function (x) {paste(x[1],x[2],sep=" ")})
    distList <- c(distList,list(waveList))
  }
  return(distList)
}

#############################################################################################
# Function which takes in the output from findDist function and finds next locations 
# to test in the growth pattern
#
#
#############################################################################################
makeGrowth <- function(growthList,search_extent) {
  for (i in 1:length(growthList)) {
    for (j in 1:length(growthList[[i]])) {
      loc <- growthList[[i]][[j]]
      if (all(loc[,3] > search_extent)) { # If there are no locations that fall within the search range, set as NULL
        growthList[[i]][j] <- list(NULL)
      } else {
        growthList[[i]][[j]] <- loc[which(loc[,3] <= search_extent),1:2]
      }
    }
  }
  growthList <- c(growthList,list(NULL))
  return(growthList)
}

##############################################################################################
# Function which combines findDist and makeGrowth functions to create the final
# lookup table
#
##############################################################################################

growth_lookup <- function (coords,invert = FALSE) {
  
  # calculate distance between locations of neighbouring wave number
  dist <- findDist(coords,invert)

  # create lookup list of next set of locations to open up
  growthList <- makeGrowth(dist,10)
  
  return (growthList)
}


#############################################
# flip right eye to left eye by reversing each row
grid.flip <- function(grid) {
    return(t(apply(grid, 1, rev)))
}