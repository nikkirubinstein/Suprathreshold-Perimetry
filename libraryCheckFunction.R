########################################################################################
# Function used to check whether a package has been installed
# If the packge has not been installed, it is installed and then loaded
#
# Created by Nikki Rubinstein
# 22 July 2017
########################################################################################

libraryCheck <- function(package){
  if(!require(package, character.only = TRUE)){
    install.packages(package)
    library(package, character.only = TRUE)
  }
}
