########################################################################
# Code to load the previously installed packages after an update of R
# and then load latest version of visualFields package
# 
# Nikki Rubinstein 1st August 2017
########################################################################

wd <- dirname(parent.frame(2)$ofile)
setwd(wd)
load("Rpackages.RData")
for (p in setdiff(packages, installed.packages()[,"Package"]))
  install.packages(p)
if (!'visualFields' %in% installed.packages()) {
  libraryCheck("spatstat") # this is the problem!!
  libraryCheck("gtools")
  install.packages("visualFields_0.5.tar.gz", repos = NULL, type="source")
}