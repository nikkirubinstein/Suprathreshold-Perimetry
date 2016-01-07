# Perform a ZEST procedure at each location in a 24-2 pattern (right eye)
# using the HFA growth pattern to set the priors at each location.
#
# The prior pdf at each location is bimodal, with the guess from the HFA growth
# pattern as the mode of the normal part of the pdf, and the damaged part
# is fixed. The guess for the primary points (+-9,+-9) is passed in as a
# parameter to runBiModalZEST().
#
# The domain of the prior is -5:40 dB, which you might want to change.
#
# See example usage at end of file. 
#
# Author: Andrew Turpin & Luke Chong
# Date: Wed 12 Jun 2013 13:06:22 EST
#
# Modified 1 Feb 2015: made 18*30 matrix of locations (aturpin)
#                    : added 3 choices of grid
#         22 Feb 2015: added window for entering patient details and initiating test (lxchong)
#         23 Feb 2015: added FP & FN catch trials
#         24 Feb 2015: added test duration calculation (lxchong)
#                    : altered growth pattern for "Peripheral" grid
#                    : added trial-by-trial data (location, stim, response, response time)
#                      gets written to a .txt file
#         25 Feb 2015: added x and y to state for use by procedureWithGrowthPattern()
#          9 Mar 2015: reduced response window to 1000 ms
#         10 Mar 2015: added nextStimState parameter - tells projector where to position itself for next stimulus. 
#         11 Mar 2015: added pause and resume button.
#         16 Mar 2015: made adaptive response window (respWin)
#         18 Mar 2015: added new rule for stim choice so that a location can't be tested
#                      twice in a row except if it is the last location in a wave. 
#         08 Apr 2015: created new pauseAtStart button so test commences with a pause.
#                      operator needs to press button to initiate test.
#         10 Apr 2015: added drop down box in menu interface to select stim size.
#         12 Apr 2015: added  live test status window 
#         14 Apr 2015: added testing of outliers
#         27 May 2015: 30-1 grid added
#         12 Jun 2015: practice test added
#         29 Jul 2015: FP and FN catch trials removed from outlier test
#         18 Aug 2015: output files are saved in separate folders based on diagnosis
#         20 Aug 2015: Added new button when test paused, allowing termination of test without saving.
#         22 Aug 2015: Added new function where you can pause and delete the most recent presentations
#          1 Jan 2016: customized primary start value of BM priors based on MW normal data
#          2 Jan 2016: Front loaded most catch trials to the first minute of test.
#          6 Jan 2016: Added response time reliability measures to the live perimetrist display. 


rm(list=ls()) 
source("growthPattern2.r")
source("grids2.r")
source("pauseButton.r")
source("testStatusOutput.r")
#########################################################################
# INPUTS:
#   eye               - "right" or "left"
#   primaryStartValue - the mode of the normal part of the prior pdf for
#                       locations (+-9,+-9).
#   gridType          - "24-2", "30-2" or "Peripheral"
#   outlierFreq &     - If there are an outlierValFreq number of neighbouring locations where difference 
#   outlierValue        is >= outlierValue, that location is flagged as an outlier and is re-tested
#
#   If running in simulation mode you need to specify 
#     tt - an 8*9 matrix 24-2 field OD (blind spot on right) 
#     fpv - false positive rate in range [0,1]
#     fnv - false negative rate in range [0,1]
#   
# RETURNS: a list with
#    np = matrix of number of presenations for each location
#    ae = matrix of absolute error for each location
#    th = matrix of measured thresholds for each location
#    trues = matrix of input thresholds
#
# ALGORITHMS: Makes use of procedureWithGrowthPattern(...).
#
# 
#########################################################################
Zest242 <- function(eye="right", primaryStartValue=30, gridType="24-2",
                    interStimInterval=c(minTime=0, maxTime=0),
                    tt=NA, fpv=0.00, fnv=0.00,outlierFreq=1,outlierValue=5) {
    ####################################################################
    # Each location derives its start value from the average of all of the
    # immediate 9 neighbours that are lower than it.
    # Numbers should start at 1 and increase, not skipping any.
    ####################################################################
    
    if (gridType == "Peripheral") {
        if (eye == "right") {
            growthPattern <- grid.peripheral
        } else {
            growthPattern <- grid.flip(grid.peripheral)
        }
    } else if (gridType == "30-2") {
        if (eye == "right") {
            growthPattern <- grid.30.2
        } else {
            growthPattern <- grid.flip(grid.30.2)
        }
    } else if (gridType == "24-2") {
        if (eye == "right") {
            growthPattern <- grid.24.2
        } else {
            growthPattern <- grid.flip(grid.24.2)
        }
    } else if (gridType == "30-1") {
        if (eye == "right") {
            growthPattern <- grid.30.1
        } else {
            growthPattern <- grid.flip(grid.30.1)
        }
    } else if (gridType == "practice") {
        if (eye == "right") {
            growthPattern <- grid.practice
        } else {
            growthPattern <- grid.flip(grid.practice)
        }
    } else {
        stop("Invalid gridType for Zest242")
    }

    onePriors <- apply(growthPattern, 1:2, function(x) ifelse(is.na(x), NA, ifelse(x == 1, primaryStartValue, NA)))

        # check tt is in the right format
    if (is.matrix(tt)) {
        if (gridType == "30-1" && (ncol(tt) != 31 || nrow(tt) != 19)) {
          stop("Zest242(): tt is not an 19*31 matrix")
        } else if ((ncol(tt) != 30 || nrow(tt) != 18) && gridType != "30-1"){
            stop("Zest242(): tt is not an 18*30 matrix")
        }
        z1 <- !is.na(growthPattern)
        z2 <- !is.na(tt)
        if (!all((z1 & z2) == z1))
            stop(paste("Zest242(): tt matrix is not the correct style for a",eye,"eye"))
    }


    ###############################
    # Set up fp and fn matrices
    ###############################
    z <- !is.na(growthPattern)
    fp <- z * matrix(fpv, nrow(growthPattern), ncol(growthPattern))
    fn <- z * matrix(fnv, nrow(growthPattern), ncol(growthPattern))

    #####################################################
    # Given guess, create a state for location (rw, cl)
    # where (rw,cl) is index into grid.* matrix
    # This version assumes
    #  1) bi-modal prior, constructed as in 
    #     Turpin et al  IOVS 44(11), November 2003. Pages 4787-4795.
    #  2) a domain of -5:40 dB
    #####################################################
    startF <- function(guess, rw, cl) {
        domain <- -5:40 # note min(domain) should be <= 0 for bimodal pdf

        glaucomaPDF <- rep(0.001,length(domain))
        glaucomaPDF[1:(6+abs(domain[1]))] <- c(rep(0.001,abs(domain[1])),0.2, 0.3, 0.2, 0.15, 0.1, 0.02)
        healthyPDF  <- function (normalModePoint) {
            temp <- c(rep(0.001,50), 0.009, 0.03, 0.05, 0.1, 0.2, 0.3, 0.2, 0.05, 0.025, 0.01,rep(0.001,50))
            mode <- which.max(temp)
            return(temp[(mode-normalModePoint+domain[1]):(mode-normalModePoint+domain[length(domain)])])
        }
        makeBiModalPDF <- function(normalModePoint, weight, pdf.floor) {
          npdf <- healthyPDF(normalModePoint)
          cpdf <- npdf * weight + glaucomaPDF
          cpdf[which(cpdf < pdf.floor)] = pdf.floor 
          return (cpdf)
        }	
        prior <- makeBiModalPDF(round(guess),4,0.001)
        prior <- prior/sum(prior)
      
        ###### helps create the makeStim function needed for the OPI
        makeStimHelper <- function(x, y) {  # returns a function of (db,n)
            ff <- function(db, n) db+n
            body(ff) <- substitute(
                {s <- list(x=x, y=y, level=dbTocd(db,4000/pi),
                        size=as.numeric(details$stimSize),
                        color="white",
                        duration=200, responseWindow=1000)
                 
                 class(s) <- "opiStaticStimulus"
                return(s)
                }
                , list(x=x,y=y))
            return(ff)
        }
        if (details$gridType == "30-1") {
          ms <- makeStimHelper(-6-14*6 + 6*(cl-1), 6+8*6 - (rw-1)*6)
          state <- ZEST.start(domain=domain, prior=prior, makeStim=ms, 
                              minStimulus=0,
                              tt=ifelse(!is.null(dim(tt)), tt[rw,cl], NA),
                              fpr=ifelse(!is.null(dim(tt)), fpv, NA),
                              fnr=ifelse(!is.null(dim(tt)), fnv, NA)
          )
          state$x <- -6-14*6 + 6*(cl-1)
          state$y <- 6+8*6 - (rw-1)*6
        } else {  
            ms <- makeStimHelper(-3-14*6 + 6*(cl-1), 3+8*6 - (rw-1)*6)
            state <- ZEST.start(domain=domain, prior=prior, makeStim=ms, 
                    minStimulus=0,
                    tt=ifelse(!is.null(dim(tt)), tt[rw,cl], NA),
                    fpr=ifelse(!is.null(dim(tt)), fpv, NA),
                    fnr=ifelse(!is.null(dim(tt)), fnv, NA)
                   )
            state$x <- -3-14*6 + 6*(cl-1)
            state$y <- 3+8*6 - (rw-1)*6
        }

        return(state)
    }

    #####################################################
    # Given a state, step the procedure and return new state
    #####################################################
    stepF <- function(state, nextStimState=NULL) { 
            Sys.sleep(runif(1, min=interStimInterval[1], max=interStimInterval[2])/1000)
            #print(state$pdf)
            if (is.null(nextStimState))
                return(ZEST.step(state)$state)
            else
                return(ZEST.step(state, nextStim=nextStimState$makeStim(0,0))$state)
    }

    #####################################################
    # Given a state, return TRUE for finished, FALSE otherwise
    #####################################################
    stopF <- ZEST.stop

    #####################################################
    # Given a state, return c(threshold, num presentations)
    # Round -ve thresholds to 0
    #####################################################
    finalF <- function(state) {
        t <- ZEST.final(state)
        #print(state$stimuli)
        return(c(t, state$numPresentations))
    }
    
    ####################################################
    # function for foveal threshold testing
    ###################################################
    while (details$fovea == TRUE) {
      resFovea <- function () {
        if (details$gridType == "30-1") {
          state.fovea <- startF(primaryStartValue,10,16)
        } else {
          state.fovea <- startF(primaryStartValue,9.5,15.5)
        }
        state.fovea$opiParams$tt <- 30
        while (stopF(state.fovea) == FALSE) {
          Sys.sleep(runif(1, min=400, max=800)/1000)
          state.fovea <- stepF(state.fovea)         
        }
        return(finalF(state.fovea)[1])   
      }
      opiSetBackground(fixation=.Octopus900Env$FIX_CROSS)
      pauseAtStartFovea() 
      fovealTH <- resFovea()
      fovealTestComplete(fovealTH)
    }
    
    windows(700,250)
    opiSetBackground(fixation=.Octopus900Env$FIX_CENTRE)
    pauseAtStart()
    commence <<- Sys.time()
    res1 <- procedureWithGrowthPattern(details$startTime,growthPattern, onePriors, startF, stepF, stopF, finalF,
            gridPat=growthPattern,
            respWinBuffer=250,
            FPLevel=dbTocd(55, 4000/pi), 
            FNDelta=10,
            FNPause=500,
            FNLocationThreshold=20,
            FPSize=as.numeric(details$stimSize),
            FNSize=as.numeric(details$stimSize))
    z <- res1$t < 0
    tz <- res1$t
    tz[z] <- 0
    
    #########################################################################
    # Calculate Difference between neighbours within each of the 4 quadrants
    # and identify outliers
    #########################################################################
    NeighbourDifference <- function(vf,outlierValue,outlierFreq) {
      neighbours <- list(
        c(-1, -1), c(0, -1), c(+1, -1), 
        c(-1,  0),           c(+1,  0), 
        c(-1, +1), c(0, +1), c(+1, +1)
      )
      
      result <- matrix(NA,nrow(vf),ncol(vf))
      for (i in list(1:(0.5*nrow(vf)),(0.5*nrow(vf)+1):nrow(vf))) {
        for (j in list(((0.5*ncol(vf)+1):ncol(vf)),(1:(0.5*ncol(vf))))) {
          quadrant <- vf[i,j]
          quadResult <- matrix(NA,nrow(quadrant),ncol(quadrant))
          for(rr in 1:nrow(quadrant))
            for(cc in 1:ncol(quadrant)) 
              if (!is.na(quadrant[rr,cc])) {
            
                deltas <- unlist(lapply(neighbours, function(p) {
                  n <- p + c(rr,cc)
                  if (all(n > 0) && all(n <= dim(quadrant)))
                    abs(quadrant[rr,cc] - quadrant[rr+p[1], cc+p[2]])
                }))
                if (sum(deltas >= outlierValue, na.rm=TRUE) >= outlierFreq) {quadResult[rr,cc] <- 1}  
              }
          result[i,j] <- quadResult
          }
      }
      return(result)
    }
    
    ###########################################################
    # Re-test outliers
    ###########################################################
    outliers <- NeighbourDifference(tz,outlierValue,outlierFreq)
    
    if (sum(outliers,na.rm=TRUE) > 0) {
      outlierPriors <- apply(outliers, 1:2, function(x) ifelse(is.na(x), NA, ifelse(x == 1, primaryStartValue, NA)))
      windows(width=700,height=250,ypos=300)
    
      res2 <- procedureWithGrowthPattern(details$startTime,outliers, outlierPriors, startF, stepF, stopF, finalF, 
            gridPat=growthPattern,
            respWinBuffer=250,
            catchTrialLoadFreq = 5000,
            catchTrialFreq=5000,
            FPLevel=dbTocd(55, 4000/pi), 
            FNDelta=10,
            FNPause=500,
            FNLocationThreshold=20,
            FPSize=as.numeric(details$stimSize),
            FNSize=as.numeric(details$stimSize))
      q <- res2$t < 0
      tq <- res2$t
      tq[q] <- 0
    } else {res2 <- list(n = 0)}
    
    if (exists("fovealTH")) {
      res <- list(np=res1$n, ae=abs(tz-tt), th=res1$t,thOutliers=res2$t,npOutliers=res2$n,thFovea=fovealTH,trues=tt,fp_counter=res1$fpc,fn_counter=res1$fnc,fp_outliers=res2$fpc,fn_outliers=res2$fnc,rt=res1$rt,rtOutliers=res2$rt)
    } else {
      res <- list(np=res1$n, ae=abs(tz-tt), th=res1$t,thOutliers=res2$t,npOutliers=res2$n,trues=tt,fp_counter=res1$fpc,fn_counter=res1$fnc,fp_outliers=res2$fpc,fn_outliers=res2$fnc,rt=res1$rt,rtOutliers=res2$rt)
    }
    return(res)
}

######################################################################
#
# create data frame containing px details and write to .csv file
# INPUT: 
#
# filename        - name of the output file
#
######################################################################
writeFile <- function (filename = paste(details$dx,"/",details$name,"_",details$dx,"_",details$grid,"_",details$stimSizeRoman,"_",details$eye,"Eye_",details$date,"_",details$startTime,".csv",sep="")) {
  px_info <- data.frame(stringsAsFactors=FALSE)
  labels <- c("#ID:","#Age:","#Diagnosis:","#Comments:","#Grid Type:","#Stimulus Size:","#Eye:")
  px_info <- data.frame(cbind(c(px_info,labels)))
  for (i in 1:7) {
    px_info[i,2] <- paste(details[i],sep="")
  }
  cat(paste("#",date(),sep=""), "\n",  file=filename, append=TRUE)
  px_info <- data.frame(lapply(px_info, as.character), stringsAsFactors=FALSE)
  write.table(px_info,filename,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)

   ######################################################################
   #
   # append threshold and presentation number data to px details file
   #
   #########################################################################

  cat(paste("#Test Duration: ",format(.POSIXct(difftime(terminate,commence,units="secs") - timePaused),"%M:%S"),sep=""),"\n",file=filename,append=TRUE)
  cat(paste("#Total Presentations: ", sum(c(unlist(z$np),unlist(z$npOutliers)),na.rm=TRUE),sep=""),"\n",file=filename,append=TRUE)
  cat(paste("#FP errors: ",sum(z$fp_counter,na.rm=TRUE),"/",length(z$fp_counter)," (",signif(sum(z$fp_counter,na.rm=TRUE)/length(z$fp_counter)*100,digits=3),"%)",sep=""), "\n",file=filename,append=TRUE)
  cat(paste("#FN errors: ",sum(z$fn_counter,na.rm=TRUE),"/",length(z$fn_counter)," (",signif(sum(z$fn_counter,na.rm=TRUE)/length(z$fn_counter)*100,digits=3),"%)",sep=""), "\n",file=filename,append=TRUE)

   if (!is.null(z$thFovea)) {
     cat(paste("#Foveal Threshold: ",round(z$thFovea)," dB", sep=""),"\n",file=filename,append=TRUE)
   } 

  cat("\n#Thresholds\n", file=filename, append=TRUE)
  write.table(round(z$th), file=filename, append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
  if (!is.null(z$thOutliers)){
    cat("\n#Outlier Thresholds\n", file=filename, append=TRUE)
    write.table(round(z$thOutliers), file=filename, append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
   }
  cat("\n#Num Presentations\n", file=filename, append=TRUE)
  write.table(z$np, file=filename, append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
  cat("\n", file=filename, append=TRUE)
}

##########################################################################################################################
#
# a second writeFile function which writes a .csv file with information displayed in columns
# 
#
##########################################################################################################################
writeFile2 <- function (details,filename = paste0(details$dx,"/",details$dx,"_",details$gridType,"_Grid_Size_",details$stimSizeRoman,".csv")) {
  
  #################################################################################
  # Arrange thresholds into an appropriate format
  #
  ################################################################################
  thTable <- function (details) {
    t <- NULL
    if (details$eye == "left") {
      th <- grid.flip(z$th)
    } else {th <- z$th}
    for (rw in 1:nrow(th)) {
    
      for (cl in 1:ncol(th)) {
        if (!is.na(th[rw,cl])) {
          t <- cbind(t,round(th[rw,cl]))
          if (details$gridType == "30-1") {
            colnames(t)[ncol(t)] <- paste(-6-14*6 + 6*(cl-1),6+8*6 - (rw-1)*6,sep=",")
          } else {
            colnames(t)[ncol(t)] <- paste(-3-14*6 + 6*(cl-1),3+8*6 - (rw-1)*6,sep=",")
          }
        }
      }
    }
    return(t)
  }
  ###############################################################################
  #
  # Compile the output spreadsheet
  #
  ###############################################################################
  output <- data.frame(ID=details$name,
                       Age = details$age,
                       Diagnosis = details$dx,
                       Comments = details$comments,
                       Grid = paste0("'",details$gridType,"'"),
                       Stimulus_Size = details$stimSizeRoman, 
                       Eye = ifelse(details$eye == "right","RE","LE"),
                       Date = details$date,
                       Time = details$startTime,
                       Test_Duration = format(.POSIXct(difftime(terminate,commence,units="secs") - timePaused),"%M:%S"),
                       Presentations = sum(c(unlist(z$np),unlist(z$npOutliers)),na.rm=TRUE),
                       FP_seen = sum(z$fp_counter,na.rm=TRUE),
                       FP_total = length(z$fp_counter),
                       FN_missed = sum(z$fn_counter,na.rm=TRUE),
                       FN_total = length(z$fn_counter),
                       Foveal_Th = ifelse(!is.null(z$thFovea),round(z$thFovea),NA))
  
  output <- cbind(output,thTable(details))
  
  if (!is.null(z$thOutliers)) {
    if (details$eye == "left"){
      outs <- grid.flip(z$thOutliers)
    } else {outs <- z$thOutliers}
  
    outlierIndex <- which(!is.na(outs),arr.ind=TRUE)
    for (i in 1:nrow(outlierIndex)) {
      round(outs[outlierIndex[i,][1],outlierIndex[i,][2]])
      if (details$gridType == "30-1") {
        output[,paste(-6-14*6 + 6*(outlierIndex[i,2]-1),6+8*6 - (outlierIndex[i,1]-1)*6,sep=",")] <- paste(output[,paste(-6-14*6 + 6*(outlierIndex[i,2]-1),6+8*6 - (outlierIndex[i,1]-1)*6,sep=",")], "(",round(outs[outlierIndex[i,][1],outlierIndex[i,][2]]),")",sep="")
      } else {
        output[,paste(-3-14*6 + 6*(outlierIndex[i,2]-1),3+8*6 - (outlierIndex[i,1]-1)*6,sep=",")] <- paste(output[,paste(-3-14*6 + 6*(outlierIndex[i,2]-1),3+8*6 - (outlierIndex[i,1]-1)*6,sep=",")], "(",round(outs[outlierIndex[i,][1],outlierIndex[i,][2]]),")",sep="")
      }
    }
  }
  
  if (file.exists(filename)) {
    write.table(output,file=filename,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
  } else {
    write.table(output,file=filename,append=TRUE,row.names=FALSE,sep=",")
  }
}

###########################################################################################
# Function which sets the primary start value for BM priors
# depending on test pattern/target size combination.
#
###########################################################################################
setPSV <- function (grid,size) {
  if ((grid == "30-1") && (size == "V")) {
    PSV <- 33
  } else if (grid == "30-2") {
      if (size == "III") {PSV <- 28} 
        else if (size == "V") {PSV <- 33} 
          else {PSV <- 30}
  } else if (grid == "Peripheral") {
      if (size == "V") {PSV <- 31} 
        else if (size == "VI") {PSV <- 32} 
          else {PSV <- 30} 
  } else {
    PSV <- 30
  }
  return(PSV)
}

###########################################################################################
# Function which writes a .csv file in a format that can be used in the "visualFields"
# package
#
###########################################################################################
writeFile3 <- function (details,filename = paste0(details$dx,"/",details$dx,"_",details$gridType,"_Grid_Size_",details$stimSizeRoman,"_vfPackage.csv")) {
  
  if (details$grid == "24-2") {
    if (details$stimSizeRoman == "III") {pattern <- "p24d2"}
    if (details$stimSizeRoman == "V") {pattern <- "p24d2v"}
    if (details$stimSizeRoman == "VI") {pattern <- "p24d2vi"}
  }
  if (details$grid == "30-2") {
    if (details$stimSizeRoman == "III") {pattern <- "p30d2"}
    if (details$stimSizeRoman == "V") {pattern <- "p30d2v"}
    if (details$stimSizeRoman == "VI") {pattern <- "p30d2vi"}
  }
  if (details$grid == "30-1") {
    if (details$stimSizeRoman == "III") {pattern <- "p30d1"}
    if (details$stimSizeRoman == "V") {pattern <- "p30d1v"}
    if (details$stimSizeRoman == "VI") {pattern <- "p30d1vi"}
    }
  if (details$grid == "Peripheral") {
    if (details$stimSizeRoman == "III") {pattern <- "peripheral"}
    if (details$stimSizeRoman == "V") {pattern <- "peripheralv"}
    if (details$stimSizeRoman == "VI") {pattern <- "peripheralvi"}
  }

  output <- data.frame(id=details$name,
                     tperimetry = "sap",
                     talgorithm = "zest",
                     tpattern = pattern,
                     tdate = format(Sys.Date(),"%m/%d/%Y"),
                     ttime = paste0(unlist(strsplit(details$startTime,".",fixed=TRUE))[1],":",unlist(strsplit(details$startTime,".",fixed=TRUE))[2],":",unlist(strsplit(details$startTime,".",fixed=TRUE))[3]),
                     stype = details$dx,
                     sage = details$age,
                     seye = ifelse(details$eye == "right","OD","OS"),
                     sbsx = 15,
                     sbsy = -1,
                     sfp = round(sum(z$fp_counter,na.rm=TRUE)/length(z$fp_counter),digits=2), 
                     sfn = round(sum(z$fn_counter,na.rm=TRUE)/length(z$fn_counter),digits=2),
                     sfl = 0,
                     sduration = format(.POSIXct(difftime(terminate,commence,units="secs") - timePaused,tz="GMT"),"%H:%M:%S"),
                     spause = format(.POSIXct(timePaused,tz="GMT"),"%H:%M:%S"))

  if (details$eye == "left") {
      th <- grid.flip(z$th)
  } else {th <- z$th}
  thresholds <- matrix(round(t(th)[!is.na(t(th))]),1,sum(!is.na(th)))
  
  if (!is.null(z$thOutliers)) {
    if (details$eye == "left"){
      outs <- grid.flip(z$thOutliers)
    } else {outs <- z$thOutliers}
    
    outliers <- t(outs)[which(!is.na(t(th)))]
    for (i in 1:length(outliers)) {
      if (!is.na(outliers[i])) {
        thresholds[i] <- round(outliers[i])
      }
    }
  }
  
  if (details$gridType == "30-2") { # add blind spot locations to 30-2 grid
  thresholds <- c(thresholds[1:35],NA,thresholds[36:44],NA,thresholds[45:length(thresholds)])
  thresholds <- matrix(thresholds,1,length(thresholds))
  }
  colnames(thresholds) <- sapply(1:ncol(thresholds), function (x) {paste0("L",x)})
  finalOutput <- cbind(output,thresholds)
    
  if (file.exists(filename)) {
    write.table(finalOutput,file=filename,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",",na="")
  } else {
    write.table(finalOutput,file=filename,append=TRUE,row.names=FALSE,sep=",",na="")
  }
}


####################################
# Example Usage
####################################
require(OPI)
chooseOpi("Octopus900")
source("query_patient_details.r")

gRunning <- TRUE

details <- practiceQuery()

while (details$practice == TRUE) {
  gRunning <- TRUE
  opiInitialize(eyeSuiteSettingsLocation="C:/ProgramData/Haag-Streit/EyeSuite/",eye=details$eye,gazeFeed=0,bigWheel=TRUE)
  Zest242(eye=details$eye, primaryStartValue=30, gridType="practice",outlierValue=5,outlierFreq=1)
  tkdestroy(tt)
  pracTestComplete()
  dev.off()
  details <- practiceQuery()
  opiClose()
}

gRunning <- TRUE # reset gRunning in case practice test was terminated early
details <- inputs()
if (dir.exists(details$dx) == FALSE) {dir.create(details$dx)}
opiInitialize(eyeSuiteSettingsLocation="C:/ProgramData/Haag-Streit/EyeSuite/",eye=details$eye,gazeFeed=0,bigWheel=TRUE)
PSV <- setPSV(details$grid,details$stimSizeRoman)


if (details$grid == "Peripheral") { 
  z <- Zest242(eye=details$eye, primaryStartValue=PSV, gridType=details$grid,outlierValue=5,outlierFreq=1,interStimInterval=c(minTime=0, maxTime=0))
} else {
  z <- Zest242(eye=details$eye, primaryStartValue=PSV, gridType=details$grid,outlierValue=5,outlierFreq=1,interStimInterval=c(minTime=0, maxTime=400))
}

terminate <- Sys.time()
tkdestroy(tt)  # closes the pause button upon completion of the test
opiClose()
graphics.off()

if (gRunning) {
  windows(700,250)
  testStatusFinal(z)
  pdf(file = paste(details$dx,"/",details$name,"_",details$dx,"_",details$grid,"_",details$stimSizeRoman,"_",details$eye,"Eye_",details$date,"_",details$startTime,".pdf",sep=""),width=13,height=5)
  testStatusFinal(z)
  dev.off()
  testComplete()
}

if (gRunning) {
  comments <- finalComments()
  details$comments <- paste(details$comments,comments,sep=".")
  writeFile()
  writeFile2(details)
  writeFile3(details)
}

opiClose()
#source('ZEST_big_grid_simulation.r')  # simulation test (only works for RE peripheral)
