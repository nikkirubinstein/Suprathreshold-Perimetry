# 
# Adapted from Luke Chong's growthPattern2.r (https://github.com/lxchong/Work-for-Wall/blob/master/growthPattern2.r).
# 
# Implements procedureSuprathreshold() which runs a SAP procedure, which presents stimuli
# at pre-determined stimulus intensity levels. The test terminates at each location, either
# after a stimulus has been detected or all stimuli have been presented.
#
# This version also has allowance for catch trials and an adaptive response
# window.
#
# Author: Andrew Turpin & Luke Chong
# Date: Sun 17 May 2015 09:26:05 AEST
# Modified by Nikki Rubinstein 24 July 2017

####################################################################
# INPUTS: 
#   startTime       - the time at which the test began
#   testIntensities - a data.frame with columns x, y and stimulus intensities - each location is a single row
#   makeStim - function to create a static stimulus of class "opiStaticStimulus"
#   details  - a list of patient and grid details with elements: name, age, dx, MRx, OR, VA, comments, gridType, stimSizeRoman, eye, startTime, date and stimSize
#   catchTrialLoadFreq - Frequency of catch trials in the first minute. Usually more frequent in order to front load.
#   catchTrialFreq - Frequency of catch trials for the remainder of the test
#   catchTrialMax - Maximum number of FP and FN catch trials for the test (e.g. if catchTrialMAx = 5, will result in 5 FP and 5 FN catch trials)
#   FPLevel  - level of stimuli for false positive in cd/m^2
#   FNPause  - duration of pause (in ms) after a FN presentation, to allow for recovery after bright stimulus.
#   FNDelta  - the degree of brightness relative to current threshold at that location (in dB)
#               e.g. if FNDelta = 10, FN stimulus of 10 dB brighter than current estimated threshold is presented.      
#   FNLocationThreshold - locations with a final threshold of at least this value (cd/m^2) 
#                         are candidates for a FN presentation.
#   FPSize  - size of FP stimulus (diameter in mm)
#   FNSize  - size of FN stimulus (diameter in mm)
#   initialRespWin  - starting response window in ms 
#   respWinBuffer   - time added to mean of respWin to determine final response Window
#   moveProj        - logical value indicating whether the nextStim should be defined for a stimulus presentation
#   minInterStimInt - minimum interstimulus interval when using an adaptive method to find the interstimulus interval
#   maxInt    - maximum perimter brithgness 
#   directory - directory in which to store output files
#   subGrid   - which portion of the grid is being tested (central, peripheral, total or practice)
#
# RETURNS:  ti is the data.frame of test intensities that was taken as an argument for this function
#           tr is a data.frame of responses - x, y, stimulus responses (TRUE - seen, FALSE - not seen, NA - not presented) correspond to stimuli in ti, terminate (logical whether the location reached termination)
#           n is number of presentations at each location
#           fpc is the false positive counter
#           fnc is the false negative counter
#           rt is a vector of the reaction times
#           terminate is the time at which the test was completed
####################################################################
source("testStatusOutput.r")
source("libraryCheckFunction.R")
libraryCheck("audio")

procedureSuprathreshold <- function(
  startTime,
  testIntensities, 
  makeStim, 
  details,
  catchTrialLoadFreq=6,
  catchTrialFreq=20,
  catchTrialMax=14,
  FPLevel=60,
  FNDelta=10,
  FNPause=300,
  FNLocationThreshold=20,
  FPSize=1.72, 
  FNSize=1.72,
  initialRespWin=1200,
  respWinBuffer=250,
  moveProj = T,
  minInterStimInt = 0, 
  maxInt = 4000, # maximum intensity - 4000 or 10000
  directory = NULL, # where should the file be saved?
  subGrid
  ) {

  # convert FP to cd
  FPLevel <- dbTocd(FPLevel, maxInt/pi)
  commence <<- Sys.time()
  if (details$gridType != "Practice")
    filename <- paste0(details$name,"_",details$dx,"_",details$grid,"_",details$stimSizeRoman,"_",details$eye,"Eye_",details$date,"_",details$startTime,"_stimResponses.txt")
  
    ####################################################################
    # Present FP or FN trial. Return TRUE(seen) or FALSE (not seen)
    ####################################################################
  presentCatch <- function(posOrNeg, responseWindow, currentIntensities, testLocationsResponse,index, maxInt) {
        if (posOrNeg == "POS") {
            s <- list(x=testLocationsResponse[index,1], y=testLocationsResponse[index,2], level=FPLevel, size=FPSize, duration=200,
                  responseWindow=round(responseWindow))
        } else {
            # check which locations have terminated with a seen response and use a brighter stimulus than last seen
          k <- which(testLocationsResponse$terminated & 
                       apply(testLocationsResponse[,-c(1:2,ncol(testLocationsResponse))], 1, sum, na.rm = T))  
            l <- k[sample(length(k),size=1)] # choose a location at random: l[rw,cl]
            
            s <- list(x=testLocationsResponse[l,1], y=testLocationsResponse[l,2], 
                      level=dbTocd(round(currentIntensities[l] - FNDelta),maxInt/pi),
                      size=FNSize, duration=200,
                      responseWindow=round(responseWindow))
        }
        class(s) <- "opiStaticStimulus"
        
        if (moveProj) {
          s2 <- list(x=testLocationsResponse[index,1], y=testLocationsResponse[index,2], 
                     level=FPLevel, size=FPSize, duration=200,
                     responseWindow=round(responseWindow))
          class(s2) <- "opiStaticStimulus"
          showStim <- opiPresent(stim=s,nextStim = s2)
                                 
        } else {
          showStim <- opiPresent(stim=s)
        }
        
        return (c(showStim,list(stimulus=s)))
    }
    
    ####################################################################
    # If there is only one location remaining, present a random stimulus
    # so that that particular location is not tested in a row
    #
    ####################################################################
 
  presentDummy <- function (index, testIntensities, responseWindow){
    locIndex <- sample((1:nrow(testIntensities))[-index], 1)
    stimulus <- sample(min(testIntensities[,-c(1,2,ncol(testIntensities))]):30, 1) # choose a stimulus intensity at random     
    s <- list(x=testIntensities[locIndex, 'x'], y=testIntensities[locIndex, 'y'],
                level=dbTocd(stimulus, maxInt/pi),size=FNSize,duration=200,responseWindow=round(responseWindow))
    
      class(s) <- "opiStaticStimulus"
      
      if (moveProj) {
        s2 <- list(x=testIntensities[index[1], 'x'], y=testIntensities[index[1], 'y'],
                   # dummyState(30,loc[1],loc[2])$x,y=dummyState(30,loc[1],loc[2])$y,
                   level=dbTocd(stimulus, maxInt/pi),size=FNSize,duration=200,responseWindow=round(responseWindow))
        class(s2) <- "opiStaticStimulus"
    
        showStim <- c(opiPresent(stim=s,nextStim = s2))
      } else {
        showStim <- c(opiPresent(stim=s),list(x=s$x,y=s$y,stimulus=stimulus))
      }
      return (c(showStim,list(stimulus=s)))
  }
    
    ####################################################################
    # Function to remove previous presentation response information
    # if the observer made a known false response
    #
    ####################################################################
    applyUndos <- function () {
      myEnv <- parent.env(environment())
      
      while ((gUndos > 0) & (length(locsPresented) >= gUndos)){
        loc <- locsPresented[length(locsPresented)]
        if (testLocationsResponse$terminated[loc]){
          print('removing a terminated location')
          myEnv$testLocationsResponse$terminated <- FALSE
          myEnv$finished_counter <- finished_counter - 1
          myEnv$idx.testLocationsResponse <- which(!testLocationsResponse$terminated)
        } else {
          print('removing an unterminated location')
        }
        myEnv$locsPresented <- locsPresented[-length(locsPresented)]
        idx.stim <- which(!is.na(testLocationsResponse[loc, -(1:2)]))[1]
        myEnv$testLocationsResponse[loc, 2 + idx.stim] <- NA
        myEnv$currentIntensities[loc] <- testIntensities[loc, 2 + idx.stim]
        gUndos <<- gUndos - 1
        
        if (details$gridType != "Practice") {
          cat(file=file.path(directory, filename),
              append=TRUE,paste("Presentation at location x =",testIntensities[loc, 'x'],"y =",testIntensities[loc, 'y'],"was deleted\n",sep=" "))
        }
      }
      
   
    }
    
    ##############################################################################################################
    # Function for the adaptive interstimulus interval
    # 
    # INPUTS
    #   responseTime - vector of total response times throughout test
    #   minISI - the minimum inter-stimulus interval
    #   interStimMultiplier - multiplier of the mean response time, which determines the max interstim interval
    #
    ################################################################################################################
    interStimInt <- function (responseTime = respTime,minISI,interStimMultiplier = 1) {
      if (!is.null(respTime)) {
        Sys.sleep(runif(1, min=minISI, max= max(minISI,mean(responseTime) * interStimMultiplier))/1000)  # pause before presenting next stimulus
      } else {
        Sys.sleep(200/1000)  #If there have been no response times recorded yet, make interstim interval 200 ms
      }
    }

    
    ####################################################################
    # set up answers and loop vars
    ####################################################################
    
    # argument to function data.frame: x, y, test intensities (testIntensities)
    testLocationsResponse <- testIntensities
    testLocationsResponse[,-c(1:2)] <- NA
    testLocationsResponse$terminated <- F
    idx.testLocationsResponse <- which(!testLocationsResponse$terminated)
    currentIntensities <- testIntensities[,3]
    
    respWin <- rep(initialRespWin,5)  ## set up adaptive response window
    
    respTime <- NULL      # vector of response times
    fp_counter <- NULL    # vector of responses for FP
    fn_counter <- NULL    # vector of responses for FN
    finished_counter <- 0 # number of terminated locations
    counter <- 1          # number of presentations
    locsPresented <- NULL # vector of locations presented in order
    gUndos <<- 0
    index <- sample(idx.testLocationsResponse, 2) # choose random location to test first
    
    ####################################################################
    # loop while still some unterminated locations
    ####################################################################
    while (length(idx.testLocationsResponse) > 0 && gRunning) {
        start_time <- Sys.time()
        applyUndos() 
      
        if ((counter <= 60 && length(fp_counter) < catchTrialMax && (counter %% catchTrialLoadFreq == 0) && ((counter/catchTrialLoadFreq) %% 2 != 0)) ||
            (counter > 60 && length(fp_counter) < catchTrialMax && (counter %% catchTrialFreq == 0) && ((counter/catchTrialFreq) %% 2 != 0))) {

            result <- presentCatch("POS", mean(respWin) + respWinBuffer, currentIntensities, testLocationsResponse,index[1], maxInt) #adaptive response window
            
            if (result$seen) {
              for (i in 1:2) {
                wait((play(sin(1:8000/20)))) ## play 2 beeps if FP error is made
              }
            }

            fp_counter <- c(fp_counter, min(1,result$seen))
            
            testStatus(result$seen,result$stimulus$x, result$stimulus$y, finished_counter, fp_counter,fn_counter,respTime,plotStimResponse=TRUE, details, testLocationsResponse, currentIntensities, subGrid)
      
           if (details$gridType != "Practice") {
              cat(file=file.path(directory, filename),
                  append=TRUE,sprintf("Location: %5s Stim: %2g dB Seen: %5s Resp Time: %5.2f Trial Time: %.0f\n", "FPCatch",cdTodb(FPLevel,maxInt/pi), result$seen, result$time, difftime(Sys.time(),start_time,units = "secs") * 1000))
            }
            counter <- counter + 1
            start_time <- Sys.time()  #reset start_time counter for trial time
        }
        
        if ((counter <= 60 && length(fn_counter) < catchTrialMax && ((counter %% catchTrialLoadFreq == 0) && ((counter/catchTrialLoadFreq) %% 2 == 0)) && any(currentIntensities > FNLocationThreshold,na.rm=TRUE) && any(testLocationsResponse$terminated & apply(testLocationsResponse[,-c(1:2,ncol(testLocationsResponse))], 1, sum, na.rm = T))) ||
            (counter > 60 && length(fn_counter) < catchTrialMax && ((counter %% catchTrialFreq == 0) && ((counter/catchTrialFreq) %% 2 == 0)) && any(currentIntensities > FNLocationThreshold,na.rm=TRUE) && any(testLocationsResponse$terminated & apply(testLocationsResponse[,-c(1:2,ncol(testLocationsResponse))], 1, sum, na.rm = T)))){
          
          result <- presentCatch("NEG", mean(respWin) + respWinBuffer, currentIntensities, testLocationsResponse,index[1], maxInt)
          
          Sys.sleep(FNPause/1000)
            fn_counter <- c(fn_counter, result$seen == FALSE)
            testStatus(result$seen,result$stimulus$x, result$stimulus$y, finished_counter, fp_counter,fn_counter,respTime,plotStimResponse=TRUE, details, testLocationsResponse, currentIntensities, subGrid)
            if (details$gridType != "Practice") {
              cat(file=file.path(directory,filename),
                append=TRUE,sprintf("Location: %5s Stim: %2g dB Seen: %5s Resp Time: %5.2f\n Trial Time: %.0f\n", "FNCatch",cdTodb(result$stimulus$level,maxInt/pi), result$seen, result$time,difftime(Sys.time(),start_time,units = "secs") * 1000))
            }
            
          counter <- counter + 1
          start_time <- Sys.time() #reset start_time counter for trial time
        } 

        counter <- counter + 1
      
        if (length(idx.testLocationsResponse) > 2) {
          index[2] <- sample(idx.testLocationsResponse[idx.testLocationsResponse != index[1]],1)
        } else if (length(idx.testLocationsResponse) == 2) {
          index[2] <- idx.testLocationsResponse[idx.testLocationsResponse != index[1]]
        } else {
          index[1] <- idx.testLocationsResponse
          index[2] <- idx.testLocationsResponse
        }
        
        locsPresented <- c(locsPresented, index[1])
     
        respWinCurrent <- respWinBuffer + mean(respWin)
        
        if (is.null(testIntensities[index[1], which(is.na(testLocationsResponse[index[1],]))[1]]))
          next
        stim <- makeStim(x = testIntensities$x[index[1]],
                         y = testIntensities$y[index[1]], 
                         stimSize = details$stimSize, 
                         responseWindow = round(respWinCurrent), 
                         db = testIntensities[index[1], which(is.na(testLocationsResponse[index[1],]))[1]])
        if (length(idx.testLocationsResponse) > 1 && moveProj == TRUE) {
          params <- list(stim = stim, nextStim = makeStim(x = testIntensities$x[index[2]],
                               y = testIntensities$y[index[2]], 
                               stimSize = details$stimSize, 
                               responseWindow = round(respWinCurrent), 
                               db = 0))
        } else {
          params <- list(stim = stim)
        }
        result <- do.call(opiPresent, params)
        
          interStimInt(respTime,minInterStimInt)#}
          
          if (details$gridType != "Practice") {
            cat(file=file.path(directory, filename),
                append=TRUE, sprintf("Location: x=%3g, y=%3g Stim: %2g dB Seen: %5s Resp Time: %5.2f Trial Time: %.0f\n", testIntensities$x[index[1]], testIntensities$y[index[1]], 
                                     round(testIntensities[index[1], which(is.na(testLocationsResponse[index[1],]))[1]]), result$seen, result$time,difftime(Sys.time(),start_time,units = "secs") * 1000))
          }
          
        if (result$seen){
          testLocationsResponse[index[1], which(is.na(testLocationsResponse[index[1],]))[1]] <- TRUE
          testLocationsResponse[index[1], 'terminated'] <- TRUE
        }  else {
          testLocationsResponse[index[1], which(is.na(testLocationsResponse[index[1],]))[1]] <- FALSE
          if (any(is.na(testLocationsResponse[index[1],])))
            currentIntensities[index[1]] <- testIntensities[index[1], which(is.na(testLocationsResponse[index[1],]))[1]]
          else
            testLocationsResponse[index[1], 'terminated'] <- TRUE
        }
        idx.testLocationsResponse <- which(!testLocationsResponse$terminated)
        finished_counter <- sum(testLocationsResponse$terminated)  
        
        testStatus(result$seen,testIntensities$x[index[1]], testIntensities$y[index[1]], finished_counter, fp_counter,fn_counter,respTime,plotStimResponse=TRUE, details, testLocationsResponse, currentIntensities, subGrid)
        
        
        if (length(idx.testLocationsResponse) == 1) {
          dummy_start_time <- Sys.time()
          result <- presentDummy (index, testIntensities, respWinCurrent)
          
          interStimInt(respTime,minInterStimInt)#}
          
          if (details$gridType != "Practice") {
            testStatus(result$seen,result$stimulus$x, result$stimulus$y, finished_counter, fp_counter,fn_counter,respTime,plotStimResponse=TRUE, details, testLocationsResponse, currentIntensities, subGrid)
            cat(file=file.path(directory, filename),
              append=TRUE,sprintf("Location: x=%3g, y=%3g Stim: %2g dB Seen: %5s Resp Time: %5.2f Trial Time: %.0f %5s\n",result$stimulus$x,result$stimulus$y,cdTodb(result$stimulus$level, maxInt/pi), result$seen, result$time,difftime(Sys.time(),dummy_start_time,units = "secs") * 1000,"(Dummy Trial)"))
          }
        }
        
        if (result$seen) {
          respWin <- c(result$time,respWin[-5])
          respTime <- c(result$time,respTime)
        }

        # if (all(testLocationsResponse$terminated))
          # finished_counter <- finished_counter + 1
        index[1] <- index[2]
    }
    
    testStatus(result$seen,testIntensities$x[index[1]], testIntensities$y[index[1]], finished_counter, fp_counter,fn_counter,respTime,plotStimResponse=FALSE, details, testLocationsResponse, currentIntensities, subGrid)
    terminate <- Sys.time()
    
    return(list(ti=testIntensities, tr=testLocationsResponse, n=apply(testLocationsResponse[,-(1:2)], 1, function(x) sum(!is.na(x)) - 1),fpc=fp_counter,fnc=fn_counter,rt=respTime, terminate=terminate))
}
