##################################################################################
# This file implements the function testStatus() and testStatusFinal() functions
# which plot a graphical representation of the visual field status
#
# Modified from Luke Chong's code
# Nikki Rubinstein
# 24 July 2017
#
# Modified 9/8/17 - added voronoi function to create a Voronoi tessellation 
#                   represenation of the visual field
##################################################################################

source("libraryCheckFunction.R")
libraryCheck("fields")
libraryCheck("tidyr")
libraryCheck("ggplot2")
libraryCheck("deldir")
libraryCheck("polyclip")

f <- function(m) { ## rotates image plot
  t(m)[,nrow(m):1]
}

################################################################################
# Function to plot voronoi tesselation. Seen at 5% - green, seen at 1% -
#     red, not seen at 1% - black
# 
# Inputs:
#   testLocationsResponse - data.frame of x, y, stimulus reseponses 
#                           (logicals) and terminated (whether 
#                           location has termianted)
#   details      - patient and grid details (created by inputs() 
#                  function)
#   finalVal     - function to determine final location status values
#   showNumber   - logical (should numbers be plotted within Voronoi 
#                  tiles)
#   plotAxes     - logical (should axes be drawn on the plot)
#   add          - logical (add to current plot or create new plot)
#   cex.pnt      - expansion factor for numbers plotted on tiles (only 
#                  applied if showNumber == TRUE)
#   dist.pnt     - Euclidean distance of convex hull from test locations
#   polygon.plot - logical (should a white polygon be plotted instead 
#                  of Voronoi tiles)
################################################################################

voronoi <- function(testLocationsResponse, details, finalVal,
                    showNumber = TRUE, 
                    plotAxes = TRUE,
                    add = FALSE, 
                    cex.pnt = 1, 
                    dist.pnt = 5, 
                    polygon.plot = FALSE){
  
  ####################################################
  # extracts x, y coordinates from list of tiles and adds
  # dist to the distance of each point from the origin
  ####################################################
  
  edgePntFn <- function(tile, dist = 5) {
    x <- tile$pt['x']
    y <- tile$pt['y']
    len <- sqrt(x^2 + y^2) + dist
    ang <- atan2(y, x)
    return(c(x = len * cos(ang), y = len * sin(ang)))
  }
  
  ####################################################
  # convert location status to plot label
  ####################################################
  
  locLabel <- function (z) {
    sapply(z, function(z){
      if(is.na(z))
        return("")
      if(z == 0) 
        return("N")
      if (z == 1)
        return("5")
      if (z == 2)
        return("1")
      return("")
      })
  }
  
  ####################################################
  # convert location status to tile colour
  ####################################################
  
  fillcol <- function(z){
    ifelse (is.na(z) | z == "", "white",
            ifelse (z == 0, "forestgreen",
                    ifelse (z == 1, "red",
                            ifelse (z == 2, "black",
                                    "blue"))))
  }
  
  ####################################################
  # Voronoi tesselation
  ####################################################
  
  if (!polygon.plot){
    vtess <- deldir(testLocationsResponse$x, 
                    testLocationsResponse$y,
                    z = finalVal(testLocationsResponse),
                    dpl = list(x = ifelse(details$eye == "right", 15, -15), y = -2))
    vtessTL <- tile.list(vtess)
    z <- sapply(vtessTL, "[[", "z")
  } else {
    vtess <- deldir(testLocationsResponse$x, 
                    testLocationsResponse$y)
    vtessTL <- tile.list(vtess)
  }
  
  ####################################################
  # Convex hull
  ####################################################
  
  outerEdges <- as.data.frame(t(sapply(vtessTL, edgePntFn, dist = dist.pnt)))
  outerEdges <- outerEdges[chull(outerEdges),]
  names(outerEdges) <- c('x', 'y')
  
  ####################################################
  # set up plot
  ####################################################
  
  if (!add){
    par(mar=c(4, 3, 4, 2) + 0.1)
    plot(x = c(min(outerEdges$x), 
               max(outerEdges$x)), 
         y = c(min(outerEdges$y), 
               max(outerEdges$y)), 
         xaxt = "n", yaxt = "n", 
         type = "n", bty = "n", xlab = "", ylab = "", las = 1, asp = 1)
  }
    
  ####################################################
  # add axes
  ####################################################
  
  if (plotAxes){  
    axis(side = 1, 
         at = seq(((min(outerEdges$x) %/% 20) + 1) * 20,
                  (max(outerEdges$x) %/% 20) * 20, 20))
    axis(side = 2, 
         at = seq(((min(outerEdges$y) %/% 20) + 1) * 20,
                  (max(outerEdges$y) %/% 20) * 20, 20), 
         las = 1)
  }
  
  ####################################################
  # plot Voronoi tiles or polygon
  ####################################################
  
  if (polygon.plot){
    polygon(outerEdges, col = "white", border = "white")
  } else {
    
    plot(vtessTL, close = TRUE, border = "white", 
         fillcol = fillcol(z), 
         showpoints = F,
         clipp = outerEdges, add = T)
    
    if (showNumber){
      points <- t(sapply(vtessTL, "[[", "pt"))
      if (sum(z == 0 | z == 1, na.rm = TRUE))
        text(points[z == 0 | z == 1,], locLabel(z[z == 0 | z == 1]))
      if (sum(z == 2, na.rm = TRUE))
        text(points[z == 2,], locLabel(z[z == 2]), col = "white")
    }
  }
}

################################################################################
#
# Plot test status information
#
# Input:
#   stimResponse     - response to previously presented stimulus
#   stimX            - x-coordinate of previously presented stimulus
#   stimY            - y-coordinate of previously presented stimulus
#   finished_counter - number of terminated locations
#   fp_counter - false positive counter
#   fn_counter - false negative counter
#   respTime   - vector of response times
#   plotStimulusResponse  - logical indicating whether to plot the previous stimulus response
#   details               - patient and grid details
#   testLocationsResponse - data.frame of x, y, stimulus reseponses (logicals) and terminated (whether location has termianted)
#   currentIntensities    - vector of intensities (next stimulus intensity for unterminated locations and previous stimulus intensitity for temrinated locations)
################################################################################
testStatus <- function (stimResponse,stimX, stimY, 
                        finished_counter,
                        fp_counter,fn_counter,
                        respTime,plotStimResponse=TRUE,
                        details, testLocationsResponse, 
                        currentIntensities,
                        subGrid) {
  
  gridType <- c("practice", "total", "central", "peripheral")
  grids <- c('Practice', 'Full Field', 'Central26', 'Peripheral')
  subGrid2 <- grids[which(gridType == subGrid)]
  
  par(mar=c(0, 4, 0, 0) + 0.1)
  layout(matrix(c(1,rep(2,2)),1,3))
  plot(1,type="n",xlim=c(1,10),ylim=c(1,100),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
  text(1,95,paste("ID:",details$name,sep=" "),pos=4,cex=2,font=2)
  text(1,90, ifelse (details$eye == "right","Eye: Right","Eye: Left"),pos=4,cex=2)
  text(1,85, paste0("Manifest Refraction: ",details$MRx),pos=4,cex=2)
  text(1,80, paste0("Over-refraction: ",details$OR),pos=4,cex=2)
  text(1,75, paste0("Visual Acuity: ",details$VA),pos=4,cex=2)
  text(1,70, paste0("Grid Type: Polar V2 ", subGrid2),pos=4,cex=2)
  text(1,65, paste("Stimulus Size: ",details$stimSizeRoman,sep=""),pos=4,cex=2)

  if (length(fp_counter) > 0){
    text(1,55,paste("FP Rate:",sum(fp_counter,na.rm=TRUE),"/",length(fp_counter),
                "(",round(sum(fp_counter,na.rm=TRUE)/length(fp_counter)*100),"%)",sep=" "),pos=4,cex=2, xpd = NA)
  } else {
    text(1,55,"FP Rate: 0 / 0 (0%)",pos=4,cex=2)
  }

  if (length(fn_counter) > 0){
    text(1,50,paste("FN Rate:",sum(fn_counter,na.rm=TRUE),"/",length(fn_counter),
                  "(",round(sum(fn_counter,na.rm=TRUE)/length(fn_counter)*100),"%)",sep=" "),pos=4,cex=2)
  } else {
    text(1,50,"FN Rate: 0 / 0 (0%)",pos=4,cex=2)
  }

  if (!is.null(respTime)) {
    text(1,40,paste("Responses < 150 ms:",sum(respTime < 150),sep=" "),pos=4,cex=2)
    text(1,35,paste("Responses > 600 ms:",sum(respTime > 600),sep=" "),pos=4,cex=2)
    text(1,30,paste("Response Time SD:",round(sd(respTime)),"ms",sep=" "),pos=4,cex=2)
  } else {
    text(1,40,"Responses < 150 ms: 0",pos=4,cex=2)
    text(1,35,"Responses > 600 ms: 0",pos=4,cex=2)
    text(1,30,"Response Time SD: NA",pos=4,cex=2)
  }
  
  text(1,20,paste("Presentations: ",sum(!is.na(testLocationsResponse[,-c(1:2,ncol(testLocationsResponse))])),sep=""),pos=4,cex=2)
  timeStamp <- Sys.time()
  text(1,15,paste("Test Time:",format(.POSIXct(difftime(timeStamp,commence,units="secs") - timePaused),"%M:%S")),pos=4,cex=2)
  text(1,10,paste("% Complete: ",sum(testLocationsResponse$terminated),"/",nrow(testLocationsResponse)," (",round(sum(testLocationsResponse$terminated)/nrow(testLocationsResponse)*100),"%)",sep=""),pos=4,cex=2)
  
  par(mar=c(4, 3, 1, 2) + 0.1)
  changeToNA <- function(vec, val){
    vec[vec == val] <- NA
    vec
  }
  xs <- sort(unique(testLocationsResponse$x))
  ys <- sort(unique(testLocationsResponse$y))
  testLocations.dataframe <- cbind(testLocationsResponse[,c('x','y','terminated')], 
                                   currentIntensities = round(currentIntensities))#,
  testLocations.matrix <- t(as.matrix(spread(testLocations.dataframe[,c('x','y','currentIntensities')], x, currentIntensities))[,-1])
  testLocations.intensities <- data.frame(x = match(testLocationsResponse$x, xs),
                                      y = match(testLocationsResponse$y, ys),
                                      IntensitiesRunning = changeToNA(round(currentIntensities) * !testLocationsResponse$terminated, 0),
                                      IntensitiesTerminated = changeToNA(round(currentIntensities) * testLocationsResponse$terminated, 0))
  image.plot(1:nrow(testLocations.matrix),1:ncol(testLocations.matrix),testLocations.matrix,xaxt="n",yaxt="n",col=gray(1:10/10, alpha = 0.6),xlab="",ylab="",zlim=c(-4,35))
  layout(matrix(c(1,rep(2,2)),1,3))
  grid(nx=nrow(testLocations.matrix),ny=ncol(testLocations.matrix),col="white",lty="solid")
  axis(1,at=1:nrow(testLocations.matrix),sort(unique(testLocationsResponse$x)))
  axis(2,at=1:ncol(testLocations.matrix),sort(unique(testLocationsResponse$y)), las = 1)
  box(col = "black")
  
  # terminated
  with(testLocations.intensities, text(x, y, IntensitiesRunning, col = "red", font = 2, cex = 1.5))
  with(testLocations.intensities, text(x, y, IntensitiesTerminated, col = "blue", font = 2, cex = 1.5))
  
  if (plotStimResponse) {
    if (stimResponse == TRUE) {
      colorbar.plot(match(stimX,xs),match(stimY, ys),41,strip.width=1/nrow(testLocations.matrix),strip.length=(1/ncol(testLocations.matrix)*0.55),col="green",adj.x=0.5,adj.y=0.5,horizontal = FALSE)
    } else {
      colorbar.plot(match(stimX,xs),match(stimY, ys),41,strip.width=1/nrow(testLocations.matrix),strip.length=(1/ncol(testLocations.matrix)*0.55),col="red",adj.x=0.5,adj.y=0.5,horizontal = FALSE)
    }
  }
}

################################################################################
#
# Plot test status information
# Location status indicates the number of stimuli that were not seen at that location
#
# Input:
#   fp_counter - false positive counter
#   fn_counter - false negative counter
#   respTime   - vector of response times
#   terminate  - the time at which the test was completed
#   details    - patient and grid details
#   testLocationsResponse - data.frame of x, y, stimulus reseponses (logicals) and terminated (whether location has termianted)
#   finalVal   - function to determine final status values of each of the tested locations
################################################################################

testStatusFinal <- function (fp_counter,fn_counter,
                        respTime, terminate,
                        details, testLocationsResponse, 
                        finalVal) {
  
  gridType <- c("Practice", "Screening_P-Total", "Screening_P-Central26", "Screening_P-Peripheral")
  grids <- c('Practice', 'Full Field', 'Central26', 'Peripheral')
  subGrid <- grids[which(gridType == details$gridType)]
  
  ########################################################
  # plot test details
  ########################################################
  
  par(mar=c(0, 4, 0, 0) + 0.1)
  layout(matrix(c(1,1, rep(2,4)),1,6))
  plot(1,type="n",xlim=c(1,10),ylim=c(1,100),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
  text(1,95 + 2.5,paste("ID:",details$name,sep=" "),pos=4,cex=2,font=2)
  text(1,90 + 2.5, ifelse (details$eye == "right","Eye: Right","Eye: Left"),pos=4,cex=2)
  text(1,85 + 2.5, paste0("Manifest Refraction: ",details$MRx),pos=4,cex=2)
  text(1,80 + 2.5, paste0("Over-refraction: ",details$OR),pos=4,cex=2)
  text(1,75 + 2.5, paste0("Visual Acuity: ",details$VA),pos=4,cex=2)
  text(1,70 + 2.5, paste0("Grid Type: Polar V2 ", subGrid),pos=4,cex=2)
  text(1,65 + 2.5, paste("Stimulus Size: ",details$stimSizeRoman,sep=""),pos=4,cex=2)
  
  if (length(fp_counter) > 0){
    text(1,55 + 2.5,paste("FP Rate:",sum(fp_counter,na.rm=TRUE),"/",length(fp_counter),
                    "(",round(sum(fp_counter,na.rm=TRUE)/length(fp_counter)*100),"%)",sep=" "),pos=4,cex=2, xpd = NA)
  } else {
    text(1,55 + 2.5,"FP Rate: 0 / 0 (0%)",pos=4,cex=2)
  }
  
  if (length(fn_counter) > 0){
    text(1,50 + 2.5,paste("FN Rate:",sum(fn_counter,na.rm=TRUE),"/",length(fn_counter),
                    "(",round(sum(fn_counter,na.rm=TRUE)/length(fn_counter)*100),"%)",sep=" "),pos=4,cex=2)
  } else {
    text(1,50 + 2.5,"FN Rate: 0 / 0 (0%)",pos=4,cex=2)
  }
  
  if (!is.null(respTime)) {
    text(1,40 + 2.5,paste("Responses < 150 ms:",sum(respTime < 150),sep=" "),pos=4,cex=2)
    text(1,35 + 2.5,paste("Responses > 600 ms:",sum(respTime > 600),sep=" "),pos=4,cex=2)
    text(1,30 + 2.5,paste("Response Time SD:",round(sd(respTime)),"ms",sep=" "),pos=4,cex=2)
  } 
  
  text(1,20 + 2.5,paste("Presentations: ",sum(!is.na(testLocationsResponse[,-c(1:2,ncol(testLocationsResponse))])),sep=""),pos=4,cex=2)
  timeStamp <- Sys.time()
  text(1,15 + 2.5,paste("Test Time:",format(.POSIXct(difftime(terminate,commence,units="secs") - timePaused),"%M:%S")),pos=4,cex=2)
  text(1,10 + 2.5,paste("% Complete: ",sum(testLocationsResponse$terminated),"/",nrow(testLocationsResponse)," (",round(sum(testLocationsResponse$terminated)/nrow(testLocationsResponse)*100),"%)",sep=""),pos=4,cex=2)
  
  text(1,0.5  + 2.5,paste("Comments:", details$comments),pos=4,cex=2, xpd = NA)
  
  ########################################################
  # plot Voronoi tessellation
  ########################################################
 
  if (subGrid == "Full Field" | subGrid == "Peripheral"){
    voronoi(testLocationsResponse = testLocationsResponse[(nrow(testLocationsResponse) - 63): nrow(testLocationsResponse),], 
            details = details, 
            finalVal = finalVal)
    if (subGrid == "Peripheral"){
      voronoi(testLocationsResponse = 
                normativeData(eye = details$eye, subGrid = "central"),
        details = details, 
        finalVal = finalVal,
        showNumber = FALSE,
        plotAxes = FALSE, 
        add = TRUE,
        dist.pnt = 0,
        polygon.plot = TRUE)
    } else {
      voronoi(testLocationsResponse = testLocationsResponse[1:64,], 
              details = details, 
              finalVal = finalVal, 
              plotAxes = FALSE, 
              add = TRUE,
              dist.pnt = 3,
              cex.pnt = 0.8)
    }
  } else {
    voronoi(testLocationsResponse = testLocationsResponse,
            details = details, finalVal = finalVal)
  }
  
  ########################################################
  # plot legend
  ########################################################
  
  z <- finalVal(testLocationsResponse)
  
  legend(x = "top", inset = -0.1, cex = 1.2,
         xpd = NA, bty = "n",horiz = TRUE, xjust = 0.5,
         legend = c(
           paste0("0 to Nl\n(", sum(z == 0, na.rm = TRUE), ")"),
           paste0("<5%\n(", sum(z == 1, na.rm = TRUE), ")"),
           paste0("<1%\n(", sum(z == 2, na.rm = TRUE), ")")),
         fill = c("forestgreen", "red", "black", "blue")[1:3]
  )
  
}

