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

# parameters to change:
# working directory: change to the folder where this code resides.
wd  <- "/Users/ivanmarin-franch/03.glaucoma/03.projects/24.MikeWall/02.screeningFullPeriphery"
age <- 85   # age of the subject to calculate the 1% and 5% attenuation in dB for stimulus presentation
eye <- "OD" # eye to be tested

setwd( wd )
library( visualFields ) # to be used with visualFields 0.5, make sure this version is installed

load( "nvsapmw_pointwise.rda" ) # the pointwise normative values calculated from M Wall's dataset of healthy eyes

# testing locations for the central PC26 and peripheral Peri locations from visualFields package
locmapc <- saplocmap$pPC26v[,c(1,2)] # central test
locmapp <- saplocmap$pPeriv[,c(1,2)] # peripheral test
locmap  <- rbind( locmapc, locmapp ) # merge all locations

# if eye is OS then, change x locations
if( eye == "OS" ) locmap$xod <- -locmap$xod

# probability categories for the normative values for size V: we are interested on 1% and 5%
pmap <- nvsapmw$pmapsettings$cutoffs
# get TDs for 1% and 5%
tdcent1 <- nvsapmw$pPC26v_zest$TDpercloc[,which( pmap == 1 )] # 1% percentile for the central test
tdcent5 <- nvsapmw$pPC26v_zest$TDpercloc[,which( pmap == 5 )] # 5% percentile for the central test
tdperi1 <- nvsapmw$pPeriv_zest$TDpercloc[,which( pmap == 1 )] # 1% percentile for the central test
tdperi5 <- nvsapmw$pPeriv_zest$TDpercloc[,which( pmap == 5 )] # 5% percentile for the central test
td1     <- c( tdcent1, tdperi1 ) # merge 1% TDs for all central and peripheral locations
td5     <- c( tdcent5, tdperi5 ) # merge 5% TDs for all central and peripheral locations

# retrieve age linear model
agelmc <- nvsapmw$pPC26v_zest$agelm
agelmp <- nvsapmw$pPeriv_zest$agelm
agelm  <- rbind( agelmc, agelmp ) # merge linear models for all locations
# obtain mean normal sensitivities from the age of the subject
msens <- agelm$intercept + agelm$slope * age

# obtain the screening stimulus intensity at each location
msens1 <- msens + td1 # for 1st percentile
msens5 <- msens + td5 # for 5th percentile

pdf( file = "sl.pdf", width = 15, height = 10 )
plot( locmap$xod, locmap$yod, typ = "n" )
text( locmap$xod, locmap$yod, as.character( round( agelm$slope, 2 ) ) )
dev.off()

pdf( file = "intercept.pdf", width = 15, height = 10 )
plot( locmap$xod, locmap$yod, typ = "n" )
text( locmap$xod, locmap$yod, as.character( round( agelm$intercept ) ) )
dev.off()

pdf( file = "sens_meanNormal.pdf", width = 15, height = 10 )
plot( locmap$xod, locmap$yod, typ = "n" )
text( locmap$xod, locmap$yod, as.character( round( msens ) ) )
dev.off()

pdf( file = "td_percentile5.pdf", width = 15, height = 10 )
plot( locmap$xod, locmap$yod, typ = "n" )
text( locmap$xod, locmap$yod, as.character( round( td5 ) ) )
dev.off()

pdf( file = "td_percentile1.pdf", width = 15, height = 10 )
plot( locmap$xod, locmap$yod, typ = "n" )
text( locmap$xod, locmap$yod, as.character( round( td1 ) ) )
dev.off()

pdf( file = "brightness_percentile5.pdf", width = 15, height = 10 )
plot( locmap$xod, locmap$yod, typ = "n" )
text( locmap$xod, locmap$yod, as.character( round( msens5 ) ) )
dev.off()

pdf( file = "brightness_percentile1.pdf", width = 15, height = 10 )
plot( locmap$xod, locmap$yod, typ = "n" )
text( locmap$xod, locmap$yod, as.character( round( msens1 ) ) )
dev.off()