# The derivation of the 5% and 1% stimulus intensity for screening
# uses the normative values obtained with M Wall's dataset of healthy
# eyes as follows: (1) first obtain the TD values for these percentiles,
# (2) from the subject's age, obtain the mean normal sensitivity values.
# Then, just (3) subtract the TD for 1% and 5% percentiles from these
# age-corrected mean-normal sensitivity values.
#
# At the end of this code, two pdfs are generated with graphs showing the
# sensitivity values (or luminance values in dB of attenuation) in each
# location for the 1% and 5% percentiles.
#
# The derivation of the 5% and 1% stimulus intensity is based on
# pointwise normative values. There was no attempt at removing odd
# visual fields with e.g. lid or lens artifacts. Therefore, it is
# expected to find unreasonably low TD values for 1% and 5%. It is also
# expected to find non-smooth values, so that sensitivity does not
# necessarily decreases with eccentricity
#
# created by IMF 23 May 2017
#
# modified by Nikki Rubinstein 1st June 2017
#   comment out pdf creation
#   make into function normativeData, which takes two arguments: age and eye ('OD' or 'OS')
#   visualFields_0.5.tar.gz and nvsapmw_pointwise.rda need to be stored in the same location as this file

# wd <- dirname(parent.frame(2)$ofile)
source("libraryCheckFunction.R")
normativeData <- function(age = 85,    # age of subject
              eye = "right",           # eye to be tested
              maxInt = 4000,           # maximum stimulus intensity of the perimeter
              subGrid = 'total'){      # sub grid - 'total', 'central', 'peripheral' or 'practice'

  # setwd( wd ) # set working directory to source file location
  if((!'visualFields' %in% installed.packages()) | (installed.packages()[grep(pattern = 'visualFields', installed.packages()),]['Version'] != 0.5)){
    libraryCheck("spatstat") # this is the problem!!
    libraryCheck("deldir")
    libraryCheck("gtools")
    install.packages("visualFields_0.5.tar.gz", repos = NULL, type="source")
  }
  library( visualFields ) # to be used with visualFields 0.5, make sure this version is installed
  
  load( "nvsapmw_pointwise.rda" ) # the pointwise normative values calculated from M Wall's dataset of healthy eyes
  
  # testing locations for the central PC26 and peripheral Peri locations from visualFields package
  locmapc <- saplocmap$pPC26v[,c(1,2)] # central test
  locmapp <- saplocmap$pPeriv[,c(1,2)] # peripheral test
  
  # probability categories for the normative values for size V: we are interested on 1% and 5%
  pmap <- nvsapmw$pmapsettings$cutoffs
  # get TDs for 1% and 5%
  tdcent1 <- nvsapmw$pPC26v_zest$TDpercloc[,which( pmap == 1 )] # 1% percentile for the central test
  tdcent5 <- nvsapmw$pPC26v_zest$TDpercloc[,which( pmap == 5 )] # 5% percentile for the central test
  tdperi1 <- nvsapmw$pPeriv_zest$TDpercloc[,which( pmap == 1 )] # 1% percentile for the central test
  tdperi5 <- nvsapmw$pPeriv_zest$TDpercloc[,which( pmap == 5 )] # 5% percentile for the central test
  
  # retrieve age linear model
  agelmc <- nvsapmw$pPC26v_zest$agelm
  agelmp <- nvsapmw$pPeriv_zest$agelm
  
  # choose appropriate subGrid
  if (subGrid == 'total' | subGrid == 'practice') {
    locmap  <- rbind( locmapc, locmapp ) # merge all locations
    td1     <- c( tdcent1, tdperi1 )     # merge 1% TDs for all central and peripheral locations
    td5     <- c( tdcent5, tdperi5 )     # merge 5% TDs for all central and peripheral locations
    agelm   <- rbind( agelmc, agelmp )    # merge linear models for all locations
  } else if ( subGrid == 'central') {
    locmap  <- locmapc # central locations
    td1     <- tdcent1 # 1% TDs for all central locations
    td5     <- tdcent5 # 5% TDs for all central locations
    agelm   <- agelmc  # linear models for all central locations
  } else if (subGrid == 'peripheral') {
    locmap  <- locmapp # peripheral locations
    td1     <- tdperi1 # 1% TDs for all peripheral locations
    td5     <- tdperi5 # 5% TDs for all peripheral locations
    agelm   <- agelmp  # linear models for all peripheral locations
  } else {
    stop(paste(subGrid, "is not a valid subGrid entry for function normativeData()"))
  }
  
  if (subGrid == "practice"){
    idx    <- seq(1,nrow(locmap),length.out = 8)
    locmap <- locmap[idx,]
    td1    <- td1[idx]
    td5    <- td5[idx]
    agelm  <- agelm[idx,]
  }
  
  # if eye is OS then, change x locations
  if( eye == "left" ) locmap$xod <- -locmap$xod

    # obtain mean normal sensitivities from the age of the subject
  msens <- agelm$intercept + agelm$slope * age
  
  # calculate conversion factor for maximum brightness value to be used
  # values were obtained using a 4000 asb maximum brightness 
  convInt <- 10 * log10(maxInt / 4000)
  
  # obtain the screening stimulus intensity at each location
  msens1 <- msens + td1 + convInt # for 1st percentile
  msens5 <- msens + td5 + convInt # for 5th percentile
  
  # combine locations and age-matched normal sensitivity values
  agelms <- data.frame(locmap, msens5, msens1)
  names(agelms)[1:2] <- c('x', 'y')
  return(agelms)
}
