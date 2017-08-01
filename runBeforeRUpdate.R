########################################################################
# Code to save the names of the currently installed packages in the
# same working directory as this file
# 
# Nikki Rubinstein 1st August 2017
########################################################################

wd <- dirname(parent.frame(2)$ofile)
setwd(wd)
packages <- installed.packages()[,"Package"]
save(packages, file="Rpackages.RData")


