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
                           resp_buzzer = 3,
                           subGrids = c('central', 'peripheral')) {
  
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
    # helper function used to combine results from two separate calls
    # to procedureSuprathreshold() 
    ###################################################################
    combineOutput <- function (r1, r2){
      if (!length(r1)){
        return(r2)}
      mapply(function(x, y){ 
        # browser()
        if (typeof(y) == "list"){
          rbind(x, y)
        } else if (length(y) == 1){
          y
        } else {
          c(x, y)
        }
      }, r1, r2, SIMPLIFY = FALSE)
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
                                    directory = directory)
    res1 <- combineOutput(res1, res)
    tkdestroy(tt) # destroy pause button
  }  
  # save results
  if(!practice & gRunning){
    windows(900,350)
    with(res1, testStatusFinal(fpc, fnc, rt, terminate, details, tr))
    # tkdestroy(tt) # destroy pause button
    testComplete()
    if (gRunning){
      pdf(file = file.path(directory, paste0(details$name,"_",details$dx,"_",details$gridType,"_",details$stimSizeRoman,"_",details$eye,"Eye_",details$date,"_",details$startTime,".pdf")),width=14,height=6)
      with(res1, testStatusFinal(fpc, fnc, rt, terminate, details, tr))
      dev.off()
      comments <- finalComments()
      details$comments <- paste(details$comments,comments,sep=" ")
      writeFile(directory, details, res1)
      writeFile2(directory, details, res1)
      writeFile3(directory, details, res1)
      px_database(details)
    }
      
  }
  tryCatch(opiClose(), error = function(x){})
   
  return(c(res1, practice = practice, details = list(details)))
}
