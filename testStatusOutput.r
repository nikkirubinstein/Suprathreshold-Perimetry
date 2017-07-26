##################################################################################
# This file implements the function testStatus() and testStatusFinal() functions
# which plot a graphical representation of the visual field status
#
# Modified from Luke Chong's code
# Nikki Rubinstein
# 24 July 2017
##################################################################################

source("libraryCheckFunction.R")
libraryCheck("fields")
libraryCheck("tidyr")
libraryCheck("ggplot2")

f <- function(m) { ## rotates image plot
  t(m)[,nrow(m):1]
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
################################################################################

testStatusFinal <- function (fp_counter,fn_counter,
                        respTime, terminate,
                        details, testLocationsResponse) {
  
  gridType <- c("Practice", "Screening_P-Total", "Screening_P-Central26", "Screening_P-Peripheral")
  grids <- c('Practice', 'Full Field', 'Central26', 'Peripheral')
  subGrid <- grids[which(gridType == details$gridType)]
  
  par(mar=c(0, 4, 0, 0) + 0.1)
  layout(matrix(c(1,rep(2,2)),1,3))
  plot(1,type="n",xlim=c(1,10),ylim=c(1,100),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
  text(1,95,paste("ID:",details$name,sep=" "),pos=4,cex=2,font=2)
  text(1,90, ifelse (details$eye == "right","Eye: Right","Eye: Left"),pos=4,cex=2)
  text(1,85, paste0("Manifest Refraction: ",details$MRx),pos=4,cex=2)
  text(1,80, paste0("Over-refraction: ",details$OR),pos=4,cex=2)
  text(1,75, paste0("Visual Acuity: ",details$VA),pos=4,cex=2)
  text(1,70, paste0("Grid Type: Polar V2 ", subGrid),pos=4,cex=2)
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
  } 
  
  text(1,20,paste("Presentations: ",sum(!is.na(testLocationsResponse[,-c(1:2,ncol(testLocationsResponse))])),sep=""),pos=4,cex=2)
  timeStamp <- Sys.time()
  text(1,15,paste("Test Time:",format(.POSIXct(difftime(terminate,commence,units="secs") - timePaused),"%M:%S")),pos=4,cex=2)
  text(1,10,paste("% Complete: ",sum(testLocationsResponse$terminated),"/",nrow(testLocationsResponse)," (",round(sum(testLocationsResponse$terminated)/nrow(testLocationsResponse)*100),"%)",sep=""),pos=4,cex=2)
  
  par(mar=c(4, 3, 1, 2) + 0.1)
  finalVal <- function(testLocationsResponse){
    responses <- testLocationsResponse[,-c(1,2,ncol(testLocationsResponse))]
    apply(responses, 1, function(x) (sum(x * seq(length(x),1), na.rm = T)) - length(x)) * -1
  }
  xs <- sort(unique(testLocationsResponse$x))
  ys <- sort(unique(testLocationsResponse$y))
  testLocations.dataframe <- cbind(testLocationsResponse[,c('x','y')], 
                                   currentIntensities = finalVal(testLocationsResponse))#,
  testLocations.matrix <- t(as.matrix(spread(testLocations.dataframe[,c('x','y','currentIntensities')], x, currentIntensities))[,-1])
  testLocations.intensities <- data.frame(x = match(testLocationsResponse$x, xs),
                                          y = match(testLocationsResponse$y, ys),
                                          Levels = testLocations.dataframe$currentIntensities)
  # image.plot(1:nrow(testLocations.matrix),1:ncol(testLocations.matrix),testLocations.matrix,xaxt="n",yaxt="n",col=ggplot2::alpha(c("white", "red", "black","green")[1:(1+max(testLocations.dataframe$currentIntensities))], alpha = 0.6),xlab="",ylab="",zlim=c(0,max(testLocations.dataframe$currentIntensities)),axis.args=list(at=0:max(testLocations.dataframe$currentIntensities)))
  image.plot(1:nrow(testLocations.matrix),1:ncol(testLocations.matrix),testLocations.matrix,xaxt="n",yaxt="n",col=ggplot2::alpha(c("white", "red", "black","green"), alpha = 0.6),xlab="",ylab="",zlim=c(0,3),axis.args=list(at=0:3))
  layout(matrix(c(1,rep(2,2)),1,3))
  grid(nx=nrow(testLocations.matrix),ny=ncol(testLocations.matrix),col="white",lty="solid")
  axis(1,at=1:nrow(testLocations.matrix),sort(unique(testLocationsResponse$x)))
  axis(2,at=1:ncol(testLocations.matrix),sort(unique(testLocationsResponse$y)), las = 1)
  box(col = "black")
  
  # plot location status values
  with(testLocations.intensities, text(x, y, Levels, col = "blue", font = 2, cex = 1.5))
  
}

