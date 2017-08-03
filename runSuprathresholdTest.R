#############################################################
# Runs a screening test by implementing suprathreshold_PV2()
# 
# Nikki Rubinstein
# 24 July 2017
#############################################################
rm(list=ls())
setwd(dirname(parent.frame(2)$ofile))
source("suprathreshold_polar_v.2.R")
source("libraryCheckFunction.R")

# load OPI package
libraryCheck("OPI")
chooseOpi("Octopus900")
# chooseOpi("SimHenson")

# choose the OPI maximum brightness in asb (4000 or 10000)
maxInt <- 4000

# extra opiInitialize to light up bowl before procedure starts
opiInitialize(eyeSuiteSettingsLocation = "C:/ProgramData/Haag-Streit/EyeSuite/",
              eye = "right",
              gazeFeed = 0,
              bigWheel = TRUE,
              resp_buzzer = 3)
opiClose()

# run practice trials with subset of test grid
practice <- TRUE
while (practice) {
  
  res <- suprathreshold_PV2(
    practice = practice, 
    maxInt = maxInt,
    eyeSuiteSettingsLocation = "C:/ProgramData/Haag-Streit/EyeSuite/",
    gazeFeed = 0,
    bigWheel = TRUE,
    resp_buzzer = 3)
  
  practice <- res$practice
  if (practice){
    pracTestComplete()
    dev.off()
    tkdestroy(tt)
  }
}

# run real test
res <- suprathreshold_PV2(
  practice = practice, 
  maxInt = maxInt,
  eyeSuiteSettingsLocation = "C:/ProgramData/Haag-Streit/EyeSuite/",
  gazeFeed = 0,
  bigWheel = TRUE,
  resp_buzzer = 3)

graphics.off()

setwd("..")
