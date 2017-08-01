########################################################################
# Code to load the previously installed packages after an update of R
# and then load latest version of visualFields package
# 
# Nikki Rubinstein 1st August 2017
########################################################################

wd <- dirname(parent.frame(2)$ofile)
setwd(wd)
load("Rpackages.RData")
for (p in setdiff(packages, c(installed.packages()[,"Package"], "visualFields")))
  install.packages(p)

# install visualFields package
source("libraryCheckFunction.R")
libraryCheck("spatstat") 
libraryCheck("gtools")
install.packages("visualFields_0.5.tar.gz", repos = NULL, type="source")
