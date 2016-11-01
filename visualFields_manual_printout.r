# Author: Luke Chong
# Date: 2016-09-23 16:48:45 PDT

###################################################################################################
# Create printout of data using visualFields package
###################################################################################################
  library(visualFields)
################################################################################
# load patches to visualFields, as new normative values, locations map, etc here
################################################################################
  load("nvsapmwcps.rda")
  load( "vfsettingsmw.rda" )
  source( "vflayoutmw_singleField.r" )

#CARE!!! set appropriate normative values
  setnv( "nvsapmwcps" )

#load data
  dx <- "CTL"
  test_pattern <- "P-Central26"
  stimulus_size <- "V"
  filename <- "CTL_P-Central26_Grid_Size_V_vfPackage.csv"
  loadfile <- read.csv(paste0(dx,"/",test_pattern," ",stimulus_size,"/",filename))
  vf <- loadvfcsv( filename = paste0(dx,"/",test_pattern," ",stimulus_size,"/",filename), patternMap = eval(parse(text = paste0("saplocmap$",as.character((tail(loadfile$tpattern,1)))))))

#enter row which you want to read in.
  row.number <- 1             # example code to print a single field
  #row.number <- c(1,3,4)      # example of code to use if you want to print a few select fields (rows 1, 3 and 4 in this case)
  #row.number <- 1:nrow(vf)    # use this line of code for batch processing of entire spreadsheet 

for (i in row.number) {
  #generate unique file name for printout
  fname <- paste0(vf[i,]$stype,"/",test_pattern," ",stimulus_size,"/",vf[i,]$id,"_",vf[i,]$stype,"_",test_pattern,"_",stimulus_size,"_",ifelse(vf[i,]$seye =="OD","right","left"),"Eye_",format(vf[i,]$tdate,"%d-%m-%Y"),"_",paste(strsplit(vf[i,]$ttime,":")[[1]][1:3],collapse="."),"_visualFields.pdf")
  
  #generate printout
    vflayoutmw_singleField(vf[i,], filename = fname)
}