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
testStatus <- function (stimResponse,currentNumPres,currentThresholds,finishedThresholds,finished_counter,gp,fp_counter,fn_counter,stateInfo,respTime,plotStimResponse=TRUE,testGrid) {
  
  colnames(currentThresholds) <- seq(-90,90,1)
  rownames(currentThresholds) <- seq(54,-54,-1)
  
  #take currentThresholds grid and remove all columns and rows which consist of only NAs
  dispCurrent <- currentThresholds[apply(testGrid,1,function (x) !all(is.na(x))),apply(testGrid,2,function (x) !all(is.na(x)))]
  dispFinished <- finishedThresholds[apply(testGrid,1,function (x) !all(is.na(x))),apply(testGrid,2,function (x) !all(is.na(x)))]
  combos <- expand.grid(1:ncol(dispCurrent),nrow(dispCurrent):1)
  
  par(mar=c(0, 4, 0, 0) + 0.1)
  layout(matrix(c(1,rep(2,2)),1,3))
  plot(1,type="n",xlim=c(1,10),ylim=c(1,100),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
  text(1,95,paste("ID:",details$name,sep=" "),pos=4,cex=2,font=2)
  text(1,90, ifelse (details$eye == "right","Eye: Right","Eye: Left"),pos=4,cex=2)
  text(1,85, paste0("Manifest Refraction: ",details$MRx),pos=4,cex=2)
  text(1,80, paste0("Over-refraction: ",details$OR),pos=4,cex=2)
  text(1,75, paste0("Visual Acuity: ",details$VA),pos=4,cex=2)
  text(1,70, paste("Grid Type:",details$grid,sep=" "),pos=4,cex=2)
  text(1,65, paste("Stimulus Size: ",details$stimSizeRoman,sep=""),pos=4,cex=2)

  if (length(fp_counter) > 0){
    text(1,55,paste("FP Rate:",sum(fp_counter,na.rm=TRUE),"/",length(fp_counter),
                "(",round(sum(fp_counter,na.rm=TRUE)/length(fp_counter)*100),"%)",sep=" "),pos=4,cex=2)
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
  
  text(1,20,paste("Presentations: ",sum(unlist(currentNumPres),na.rm=TRUE),sep=""),pos=4,cex=2)
  timeStamp <- Sys.time()
  text(1,15,paste("Test Time:",format(.POSIXct(difftime(timeStamp,commence,units="secs") - timePaused),"%M:%S")),pos=4,cex=2)
  text(1,10,paste("% Complete: ",finished_counter,"/",length(which(gp > 0))," (",round(finished_counter/length(which(gp > 0))*100),"%)",sep=""),pos=4,cex=2)
  
  par(mar=c(4, 3, 1, 2) + 0.1)
  #image.plot(as.numeric(colnames(dispCurrent)),rev(as.numeric(rownames(dispCurrent))),f(dispCurrent),xaxt="n",yaxt="n",col=gray(1:10/10),xlab="",ylab="",zlim=c(-4,35))
  image.plot(1:ncol(dispCurrent),1:nrow(dispCurrent),f(dispCurrent),xaxt="n",yaxt="n",col=gray(1:10/10),xlab="",ylab="",zlim=c(-4,35))
  layout(matrix(c(1,rep(2,2)),1,3))
  grid(nx=ncol(dispCurrent),ny=nrow(dispCurrent),col="white",lty="solid")
  axis(1,at=1:ncol(dispCurrent),as.numeric(colnames(dispCurrent)))
  axis(2,at=nrow(dispCurrent):1,as.numeric(rownames(dispCurrent)))
  box(col = "black")
  
  text(combos[,1],combos[,2],round(as.vector(t(dispCurrent))),col="red",font=2,cex=2)
  text(combos[,1],combos[,2],round(as.vector(t(dispFinished))),col="blue",font=2,cex=2)
  
  if (plotStimResponse) {
    if (stimResponse == TRUE) {
      colorbar.plot(match(stateInfo$x,colnames(dispCurrent)),(nrow(dispCurrent) + 1) - match(stateInfo$y,rownames(dispCurrent)),41,strip.width=1/ncol(dispCurrent),strip.length=(1/nrow(dispCurrent)*0.55),col="green",adj.x=0.5,adj.y=0.5,horizontal = FALSE)
    } else {
      colorbar.plot(match(stateInfo$x,colnames(dispCurrent)),(nrow(dispCurrent) + 1) - match(stateInfo$y,rownames(dispCurrent)),41,strip.width=1/ncol(dispCurrent),strip.length=(1/nrow(dispCurrent)*0.55),col="red",adj.x=0.5,adj.y=0.5,horizontal = FALSE)
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
  finalTh <- summary$th
  
  if (!is.null(summary$thOutliers)) {
    finalTh[!is.na(summary$thOutliers)] <- summary$thOutliers[!is.na(summary$thOutliers)]
  }  
  
  disp <- round(finalTh[apply(finalTh,1,function (x) !all(is.na(x))),apply(finalTh,2,function (x) !all(is.na(x)))])
  combos <- expand.grid(1:ncol(disp),nrow(disp):1)
  par(mar=c(0, 4, 0, 0) + 0.1)
  layout(matrix(c(1,rep(2,2)),1,3))
  plot(1,type="n",xlim=c(1,10),ylim=c(1,100),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
  text(1,100,paste("ID:",details$name,sep=" "),pos=4,cex=2,font=2)
  text(1,96, ifelse (details$eye == "right","Eye: Right","Eye: Left"),pos=4,cex=2)
  text(1,92, paste0("Manifest Refraction: ",details$MRx),pos=4,cex=2)
  text(1,88, paste0("Over-refraction: ",details$OR),pos=4,cex=2)
  text(1,84, paste0("Visual Acuity: ",details$VA),pos=4,cex=2)
  text(1,80,paste("Stimulus Size: ",details$stimSizeRoman,sep=""),pos=4,cex=2)
  text(1,76,paste("Total Test Time:",format(.POSIXct(difftime(terminate,commence,units="secs") - timePaused),"%M:%S")),pos=4,cex=2)
  
  text(1,67,paste(details$grid,"Test",sep=" "),pos=4,cex=2,font=2)
  if (length(summary$fp_counter) > 0){
    text(1,63,paste("FP Rate:",sum(summary$fp_counter,na.rm=TRUE),"/",length(summary$fp_counter),
                    "(",round(sum(summary$fp_counter,na.rm=TRUE)/length(summary$fp_counter)*100),"%)",sep=" "),pos=4,cex=2)
  } else {
    text(1,63,"FP Rate: 0 / 0 (0%)",pos=4,cex=2)
  }
  
  if (length(summary$fn_counter) > 0){
    text(1,59,paste("FN Rate:",sum(summary$fn_counter,na.rm=TRUE),"/",length(summary$fn_counter),
                    "(",round(sum(summary$fn_counter,na.rm=TRUE)/length(summary$fn_counter)*100),"%)",sep=" "),pos=4,cex=2)
  } else {
    text(1,59,"FN Rate: 0 / 0 (0%)",pos=4,cex=2)
  }
  
  text(1,55,paste("Responses < 150 ms:",sum(summary$rt < 150),sep=" "),pos=4,cex=2)
  text(1,51,paste("Responses > 600 ms:",sum(summary$rt > 600),sep=" "),pos=4,cex=2)
  text(1,47,paste("Response Time SD:",round(sd(summary$rt)),"ms",sep=" "),pos=4,cex=2)
  
  text(1,43,paste("Presentations: ",sum(unlist(summary$np),na.rm=TRUE),sep=""),pos=4,cex=2)
  text(1,39,paste("% Complete: ",length(which(!is.na(summary$th))),"/",length(which(!is.na(summary$th)))," (100%)",sep=""),pos=4,cex=2)
  
  if (!is.null(summary$thOutliers)) {
    text(1,30,"Outliers",pos=4,cex=2,font=2)
    text(1,26,paste("Responses < 150 ms:",sum(summary$rtOutliers < 150),sep=" "),pos=4,cex=2)
    text(1,22,paste("Responses > 600 ms:",sum(summary$rtOutliers > 600),sep=" "),pos=4,cex=2)
    text(1,18,paste("Response Time SD:",round(sd(summary$rtOutliers)),"ms",sep=" "),pos=4,cex=2)
    text(1,14,paste("Presentations: ",sum(unlist(summary$npOutliers),na.rm=TRUE),sep=""),pos=4,cex=2)
    text(1,10,paste("% Complete: ",length(which(!is.na(summary$thOutliers))),"/",length(which(!is.na(summary$thOutliers)))," (100%)",sep=""),pos=4,cex=2)
  }
  
  par(mar=c(4, 3, 1, 2) + 0.1)
  image.plot(1:ncol(disp),1:nrow(disp),f(disp),xaxt="n",yaxt="n",col=gray(1:10/10),xlab="",ylab="",zlim=c(-4,35))
  layout(matrix(c(1,rep(2,2)),1,3))
  grid(nx=ncol(disp),ny=nrow(disp),col="white",lty="solid")
  box(col = "black")
  text(combos[,1], combos[,2],as.vector(t(disp)),col="blue",font=2,cex=2)
  
  #thresholds <- round(summary$th)
  #if (!is.null(summary$thOutliers)) {
  #  outliersIndex <- which(!is.na(summary$thOutliers),arr.ind=TRUE)
  #  outliers <- apply(outliersIndex,1, function (x) {paste(thresholds[x[1],x[2]],"(",round(summary$thOutliers[x[1],x[2]]),")",sep="")})
  #  thresholds[outliersIndex] <- outliers
  #}
}





