####################################################################################
# Adapted from Luke Chong's ZEST_big_grid2.r (https://github.com/lxchong/Work-for-Wall/blob/master/ZEST_big_grid2.r).
# 
# Performs a screening test using size V Goldmann stimuli at the 5th and 1st 
# percentiles of normal sensitivities for age. 
# Test pattern used is the polar V2 (128 locations).
#
# 
# Nikki Rubinstein
# 24 July 2017
####################################################################################


source("test_screeningStimulusValues.R")
source("query_patient_details.r")
source("suprathreshold.R")
source("writeFileFunctions.R")
source("pauseButton.r")
source("testStatusOutput.r")
#########################################################################
# Function: suprathreshold_PV2()
#
# INPUTS:
#   practice             - logical indicating whether this is a practice trial
#                          a test grid of 8 locations is used for practice trials and outcomes are not recorded
#   minInterStimInterval - minimum interval between stimuli 
#   maxInt               - maximum brightness of perimeter
#   minStimIntensity     - minimum stimulus intensity (dB) to be displayed 
#   moveProjector        - logical indicating whether nextStim should be specified for opiPresent()  
#   eyeSuiteSettingsLocation - directory containing EyeSuite setting files
#   gazeFeed                 - logical indicating whether gaze tracking should be used
#   bigWheel                 - logical indicating whether the larger aperture wheel should be used
#   resp_buzzer              - integer (0 - 3) representing buzzer volume
#   subGrids                 - 'total', 'central', 'peripheral' or 'practice'
#   
# RETURNS: a list with
#    ti   - data.frame of test intensities - x, y, stimulus intensities
#    tr   - data.frame of responses - x, y, stimulus responses (TRUE - seen, FALSE - not seen, NA - not presented) correspond to stimuli in ti, terminate (logical whether the location reached termination)
#    n    - number of presentations at each location
#    fpc  - vector of false positive responses
#    fnc  - vector of false negative responses
#    rt   - a vector of the reaction times
#    terminate - the time at which the test was completed
#    practice  - logical indicating whether a practice test should be offered next
#    details   - a list of patient and grid details with elements: 
#                   name, age, dx, MRx, OR, VA, comments, gridType, stimSizeRoman, 
#                   eye, startTime, date and stimSize
#
# ALGORITHMS: Makes use of procedureSuprathreshold(...).
#
# 
#########################################################################
suprathreshold_PV2 <- function(
                           practice = FALSE,
                           minInterStimInterval=300,
                           maxInt = 4000,
                           minStimIntensity = 15,
                           moveProjector = TRUE,
                           eyeSuiteSettingsLocation="C:/ProgramData/Haag-Streit/EyeSuite/",
                           gazeFeed=0,
                           bigWheel=TRUE,
                           resp_buzzer = 3) {
  
  ##################################################################
  # initialise global variables for tcltk package
  ##################################################################
  gRunning <<- TRUE
  mistakes <<- 0
  deletes <<- 0
  
  ##################################################################
  # get patient details
  ##################################################################
  if (practice){
    details <- practiceQuery()
    if (!details$practice){
      print("No practice test will be performed")
      print("Moving on to the real test!")
      return(list(practice = FALSE))
    }
  } else {
    details <- inputs()
    directory <- file.path("..",details$dx,paste0(details$gridType," ",details$stimSizeRoman))
    if (!dir.exists(directory))
      dir.create(directory, recursive = TRUE)
  }
  
  gridType <- c("Practice", "Screening_P-Total", "Screening_P-Central26", "Screening_P-Peripheral")
  grids <- list('practice', c('central', 'peripheral'), 'central', 'peripheral')
  subGrids <- grids[[which(gridType == details$gridType)]]
    
  ###################################################################
  # helper function used to make stimuli of class opiStaticStimulus  
  ###################################################################
    makeStim <- function (x, y, stimSize, responseWindow, db){
      s <- list(x=x, y=y, level=dbTocd(db,maxInt/pi),
                size=as.numeric(stimSize),
                color="white",
                duration=200, 
                responseWindow=responseWindow)
      
      class(s) <- "opiStaticStimulus"
      return(s)
    }
  
  
  
  ###################################################################
  # helper function used to determine next stimulus intensity to be
  # tested  
  # Input:  
  #     int  - testIntensitites
  #     loc  - which location was tested
  #     numPres - presentation number at location loc 
  # Output: 
  #     db    - next test intensity
  #     index - column index of test intensity in int and resp data 
  #             frames    
  ###################################################################
  nextStimdb <- function(int, loc, numPres){
    index <- ((numPres - 1) %% (ncol(int) - 2)) + 3
    db <- int[loc, index]
    list(index = index, db = db)
  }
  
  ###################################################################
  # helper function used to update testLocationsResponse
  # Input:  
  #     resp  - testLocationsResponse
  #     loc   - which location was tested
  #     index - column index of test intensity in resp data frame
  #     seen  - was the stimulus detected?
  #     
  # Output: 
  #     resp - testLocationsResponse
  ###################################################################
  stimUpdate <- function(resp, loc, dbIndex, seen){
    # resp$terminated[loc] <- (seen | dbIndex == (ncol(resp) - 1))
    resp$terminated[loc] <- ((seen & dbIndex == 3) |  # first location seen
                               (!seen & dbIndex == (ncol(resp) - 1)) |    # second location not seen
                               (dbIndex == 3 & !is.na(resp[loc, dbIndex + 1])))   # first location tested after second location tested
    resp[loc, dbIndex] <- seen
    return(resp)
  }

  ###################################################################
  # helper function used to determine final location status values
  # Input:  
  #     resp  - testLocationsResponse
  #     
  # Output: 
  #     vector of location statuses - 0 (5% seen), 1 (1% seen), 2 (1% not seen)
  ###################################################################
  
  finalVal <- function(resp) {
    responses <- resp[,-c(1,2,ncol(resp))]
    apply(responses, 1, function(x) 
      (max(x * seq(length(x),1), na.rm = T)) - length(x)) * -1
  }
  
    ###################################################################
    # helper function used to combine results from two separate calls
    # to procedureSuprathreshold() 
    ###################################################################
    combineOutput <- function (r1, r2){
      if (!length(r1)){
        return(r2)}
      result <- mapply(function(x, y){ 
        if (typeof(y) == "list"){
          rbind(x, y)
        } else {
          c(x, y)
        }
      }, r1, r2, SIMPLIFY = FALSE)
      names(result) <- names(r2)
      return(result)
    }

    
    ##################################################################
    # initialise perimeter
    ##################################################################
    opiInitialize(eyeSuiteSettingsLocation = eyeSuiteSettingsLocation,
                eye = details$eye,
                gazeFeed = gazeFeed,
                bigWheel = bigWheel,
                resp_buzzer = resp_buzzer,
                zero_dB_is_10000_asb = (maxInt == 10000))

  # set fixation marker
  opiSetBackground(
    fixation = .Octopus900Env$FIX_CENTRE,
    lum = .Octopus900Env$BG_10)

  # opiInitialize(type="C", A=NA, B=NA, cap=6, display=NULL, maxStim=10000/pi)
  
  
  # open external display window
  windows(700,250)
  
  res1 <- list()
  for (subGrid in subGrids){
    
    # check that the test hasn't been terminated 
    if (gRunning){

    ##################################################################
    # get test intensity values
    # capped at a minimum of 15 dB
    ##################################################################
    testIntensities <- normativeData(
      age = as.numeric(details$age), 
      eye = details$eye, 
      maxInt = maxInt,
      subGrid = subGrid)
    testIntensities[, -(1:2)][which(testIntensities[, -(1:2)] < minStimIntensity, arr.ind = TRUE)] <- minStimIntensity
    
    # prompt user to begin test
    pauseAtStart(subGrid)
    
    # run screening procedure
    res <- procedureSuprathreshold(startTime = details$startTime, 
                                   testIntensities = testIntensities, 
                                   makeStim = makeStim,
                                   nextStimdb = nextStimdb, 
                                   stimUpdate = stimUpdate,
                                   details = details,
                                   respWinBuffer=250,
                                   FPLevel=55, 
                                   FNDelta=10,
                                   FNPause=300,
                                   FNLocationThreshold=20,
                                   FPSize=as.numeric(details$stimSize),
                                   FNSize=as.numeric(details$stimSize),
                                   moveProj = moveProjector,
                                   minInterStimInt = minInterStimInterval,
                                   maxInt = maxInt,
                                   directory = directory,
                                   subGrid = subGrid)
    
    # print(cbind(res$tr, res$n))
    res1 <- combineOutput(res1, res)
    # browser()
    tkdestroy(tt) # destroy pause button
    }
  }  
  # save results
  if(!practice & gRunning){
    windows(900,350)
    with(res1, testStatusFinal(fpc, fnc, rt, sum(testTime), details, tr, finalVal, sum(n, na.rm = TRUE)))
    # tkdestroy(tt) # destroy pause button
    testComplete()
    if (gRunning){
      comments <- finalComments()
      details$comments <- paste(details$comments,comments,sep=" ")
      pdf(file = file.path(directory, paste0(details$name,"_",details$dx,"_",details$gridType,"_",details$stimSizeRoman,"_",details$eye,"Eye_",details$date,"_",details$startTime,".pdf")),width=14,height=6)
      with(res1, testStatusFinal(fpc, fnc, rt, sum(testTime), details, tr, finalVal, sum(n, na.rm = TRUE)))
      dev.off()
      writeFile(directory, details, res1, finalVal)
      writeFile2(directory, details, res1, finalVal)
      writeFile3(directory, details, res1, finalVal, details$gridType)
      px_database(details)
    }
      
  }
  tryCatch(opiClose(), error = function(x){})
   
  return(c(res1, practice = practice, details = list(details)))
}
