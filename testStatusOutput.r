require(fields)

f <- function(m) { ## rotates image plot
  t(m)[,nrow(m):1]
}

################################################################################
#
# Plot test status information
#
#
################################################################################
testStatus <- function (stimResponse,currentNumPres,currentThresholds,finishedThresholds,finished_counter,gp,fp_counter,fn_counter,stateInfo,respTime,plotStimResponse=TRUE) {
  if (details$gridType == "30-1") {
    combos <- expand.grid(seq(-90,90,6),seq(54,-54,-6))
  } else {
    combos <- expand.grid(seq(-87,87,6),seq(51,-51,-6))
  }
  par(mar=c(5, 4, 4, 1) + 0.1)
  layout(matrix(c(1,rep(2,2)),1,3))
  plot(1,type="n",xlim=c(1,10),ylim=c(1,100),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
  text(1,100,paste("ID:",details$name,sep=" "),pos=4,cex=2,font=2)
  text(1,93, ifelse (details$eye == "right","Eye: Right","Eye: Left"),pos=4,cex=2)
  text(1,86,paste("Grid Type:",details$grid,sep=" "),pos=4,cex=2)
  text(1,79,paste("Stimulus Size: ",details$stimSizeRoman,sep=""),pos=4,cex=2)

  if (length(fp_counter) > 0){
    text(1,65,paste("FP Rate:",sum(fp_counter,na.rm=TRUE),"/",length(fp_counter),
                "(",round(sum(fp_counter,na.rm=TRUE)/length(fp_counter)*100),"%)",sep=" "),pos=4,cex=2)
  } else {
    text(1,65,"FP Rate: 0 / 0 (0%)",pos=4,cex=2)
  }

  if (length(fn_counter) > 0){
    text(1,58,paste("FN Rate:",sum(fn_counter,na.rm=TRUE),"/",length(fn_counter),
                  "(",round(sum(fn_counter,na.rm=TRUE)/length(fn_counter)*100),"%)",sep=" "),pos=4,cex=2)
  } else {
    text(1,58,"FN Rate: 0 / 0 (0%)",pos=4,cex=2)
  }

  if (!is.null(respTime)) {
    text(1,51,paste("Responses < 150 ms:",sum(respTime < 150),sep=" "),pos=4,cex=2)
    text(1,45,paste("Responses > 600 ms:",sum(respTime > 600),sep=" "),pos=4,cex=2)
    text(1,39,paste("Response Time SD:",round(sd(respTime)),"ms",sep=" "),pos=4,cex=2)
  } else {
    text(1,51,"Responses < 150 ms: 0",pos=4,cex=2)
    text(1,45,"Responses > 600 ms: 0",pos=4,cex=2)
    text(1,39,"Response Time SD: NA",pos=4,cex=2)
  }
  
  text(1,25,paste("Presentations: ",sum(unlist(currentNumPres),na.rm=TRUE),sep=""),pos=4,cex=2)
  timeStamp <- Sys.time()
  text(1,19,paste("Test Time:",format(.POSIXct(difftime(timeStamp,commence,units="secs") - timePaused),"%M:%S")),pos=4,cex=2)
  text(1,13,paste("% Complete: ",finished_counter,"/",length(which(gp > 0))," (",round(finished_counter/length(which(gp > 0))*100),"%)",sep=""),pos=4,cex=2)
  
  par(mar=c(4, 3, 1, 2) + 0.1)
  if (details$gridType == "30-1") {
    image.plot(seq(-90,90,6),seq(-54,54,6),f(currentThresholds),xaxt="n",yaxt="n",col=gray(1:10/10),xlab="Eccentricity (°)",ylab="Eccentricity (°)",zlim=c(-1,35))
    layout(matrix(c(1,rep(2,2)),1,3))
    grid(nx=31,ny=19,col="white",lty="solid")
    axis(1,at=c(seq(-90,-6,12),seq(6,90,12)))
    axis(2,at=c(seq(-54,-6,12),seq(6,54,12)))
  } else {
    image.plot(seq(-87,87,6),seq(-51,51,6),f(currentThresholds),xaxt="n",yaxt="n",col=gray(1:10/10),xlab="Eccentricity (°)",ylab="Eccentricity (°)",zlim=c(-1,35))
    layout(matrix(c(1,rep(2,2)),1,3))
    grid(nx=30,ny=18,col="white",lty="solid")
    axis(1,at=c(seq(-87,-3,12),seq(3,87,12)))
    axis(2,at=c(seq(-51,-3,12),seq(3,51,12)))
  }

  text(combos[,1], combos[,2],round(as.vector(t(currentThresholds))),col="red",font=2)
  text(combos[,1], combos[,2],round(as.vector(t(finishedThresholds))),col="blue",font=2)
  
  if (plotStimResponse) {
    if (stimResponse == TRUE) {
      colorbar.plot(stateInfo$x,stateInfo$y,41,strip.width=0.055,strip.length=0.057,col="green",adj.x=0.48,adj.y=0.51)
    } else {
      colorbar.plot(stateInfo$x,stateInfo$y,41,strip.width=0.055,strip.length=0.057,col="red",adj.x=0.48,adj.y=0.51)
      }
  }
}

########################################################################################################################
#
# Plot test status summary at the end of the test, merging outliers with the main test
#
#
#####################################################################################################################
testStatusFinal <- function (summary) {
  if (details$gridType == "30-1") {
    combos <- expand.grid(seq(-90,90,6),seq(54,-54,-6))
  } else {
    combos <- expand.grid(seq(-87,87,6),seq(51,-51,-6))
  }
  par(mar=c(1, 4, 1, 1) + 0.1)
  layout(matrix(c(1,rep(2,2)),1,3))
  plot(1,type="n",xlim=c(1,10),ylim=c(1,100),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
  text(1,100,paste("ID:",details$name,sep=" "),pos=4,cex=2,font=2)
  text(1,95, ifelse (details$eye == "right","Eye: Right","Eye: Left"),pos=4,cex=2)
  text(1,90,paste("Stimulus Size: ",details$stimSizeRoman,sep=""),pos=4,cex=2)
  text(1,85,paste("Total Test Time:",format(.POSIXct(difftime(terminate,commence,units="secs") - timePaused),"%M:%S")),pos=4,cex=2)
  
  text(1,75,paste(details$grid,"Test",sep=" "),pos=4,cex=2,font=2)
  if (length(summary$fp_counter) > 0){
    text(1,70,paste("FP Rate:",sum(summary$fp_counter,na.rm=TRUE),"/",length(summary$fp_counter),
                    "(",round(sum(summary$fp_counter,na.rm=TRUE)/length(summary$fp_counter)*100),"%)",sep=" "),pos=4,cex=2)
  } else {
    text(1,70,"FP Rate: 0 / 0 (0%)",pos=4,cex=2)
  }
  
  if (length(summary$fn_counter) > 0){
    text(1,65,paste("FN Rate:",sum(summary$fn_counter,na.rm=TRUE),"/",length(summary$fn_counter),
                    "(",round(sum(summary$fn_counter,na.rm=TRUE)/length(summary$fn_counter)*100),"%)",sep=" "),pos=4,cex=2)
  } else {
    text(1,65,"FN Rate: 0 / 0 (0%)",pos=4,cex=2)
  }
  
  text(1,60,paste("Responses < 150 ms:",sum(summary$rt < 150),sep=" "),pos=4,cex=2)
  text(1,55,paste("Responses > 600 ms:",sum(summary$rt > 600),sep=" "),pos=4,cex=2)
  text(1,50,paste("Response Time SD:",round(sd(summary$rt)),"ms",sep=" "),pos=4,cex=2)
  
  text(1,45,paste("Presentations: ",sum(unlist(summary$np),na.rm=TRUE),sep=""),pos=4,cex=2)
  text(1,40,paste("% Complete: ",length(which(!is.na(summary$th))),"/",length(which(!is.na(summary$th)))," (100%)",sep=""),pos=4,cex=2)
  
  if (!is.null(summary$thOutliers)) {
    text(1,30,"Outliers",pos=4,cex=2,font=2)
    text(1,25,paste("Responses < 150 ms:",sum(summary$rtOutliers < 150),sep=" "),pos=4,cex=2)
    text(1,20,paste("Responses > 600 ms:",sum(summary$rtOutliers > 600),sep=" "),pos=4,cex=2)
    text(1,15,paste("Response Time SD:",round(sd(summary$rtOutliers)),"ms",sep=" "),pos=4,cex=2)
    text(1,10,paste("Presentations: ",sum(unlist(summary$npOutliers),na.rm=TRUE),sep=""),pos=4,cex=2)
    text(1,5,paste("% Complete: ",length(which(!is.na(summary$thOutliers))),"/",length(which(!is.na(summary$thOutliers)))," (100%)",sep=""),pos=4,cex=2)
  }
  
  par(mar=c(4, 3, 1, 2) + 0.1)
  if (details$gridType == "30-1") {
    image.plot(seq(-90,90,6),seq(-54,54,6),f(summary$th),xaxt="n",yaxt="n",col=gray(1:10/10),xlab="Eccentricity (°)",ylab="Eccentricity (°)",zlim=c(-1,35))
    layout(matrix(c(1,rep(2,2)),1,3))
    grid(nx=31,ny=19,col="white",lty="solid")
    axis(1,at=c(seq(-90,-6,12),seq(6,90,12)))
    axis(2,at=c(seq(-54,-6,12),seq(6,54,12)))
  } else {
    image.plot(seq(-87,87,6),seq(-51,51,6),f(summary$th),xaxt="n",yaxt="n",col=gray(1:10/10),xlab="Eccentricity (°)",ylab="Eccentricity (°)",zlim=c(-1,35))
    layout(matrix(c(1,rep(2,2)),1,3))
    grid(nx=30,ny=18,col="white",lty="solid")
    axis(1,at=c(seq(-87,-3,12),seq(3,87,12)))
    axis(2,at=c(seq(-51,-3,12),seq(3,51,12)))
  }
  
  thresholds <- round(summary$th)
  if (!is.null(summary$thOutliers)) {
    outliersIndex <- which(!is.na(summary$thOutliers),arr.ind=TRUE)
    outliers <- apply(outliersIndex,1, function (x) {paste(thresholds[x[1],x[2]],"(",round(summary$thOutliers[x[1],x[2]]),")",sep="")})
    thresholds[outliersIndex] <- outliers
  }
  text(combos[,1], combos[,2],as.vector(t(thresholds)),col="blue",font=2,cex=0.81)
}





