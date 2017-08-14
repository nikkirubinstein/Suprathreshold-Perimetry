####################################################################################
# Functions used to write output of procedureSuprathreshold() to files
#     writeFile()
#     writeFile2()
#     writeFile3()
#     px_database()
#     VFPrintout()
#
# Modified from Luke Chong's original code by Nikki Rubintein
# 24 July 2017
####################################################################################

# function to find locations' statuses (status is the number of stimuli that were not seen)
# argument is testLocationsResponses
# finalVal <- function(z){
#   responses <- z[,-c(1,2,ncol(z))]
#   apply(responses, 1, function(x) (sum(x * seq(length(x),1), na.rm = T)) - length(x)) * -1
# }

# function to expand grid of locations' statuses (status is the number of stimuli that were not seen)
# argument is testLocationResponses
expandVal <- function(z, finalVal){
  df <- data.frame(x = z$x, y = z$y, val = finalVal(z))
  t(as.matrix(spread(df, x, val))[,-1])
}

######################################################################
#
# create data frame containing px details and write to .csv file
# INPUT: 
#
# filename        - name of the output file
#
######################################################################
writeFile <- function (directory, details, res, finalVal){
  
  filename <- file.path(directory,paste0(details$name,"_",details$dx,"_",details$gridType,"_",details$stimSizeRoman,"_",details$eye,"Eye_",details$date,"_",details$startTime,".csv"))
  px_info <- data.frame(stringsAsFactors=FALSE)
  labels <- c("#ID:","#Age:","#Diagnosis:","#Manifest Refraction:","#Over-refraction:","#VA:","#Comments:","#Grid Type:","#Stimulus Size:","#Eye:")
  px_info <- data.frame(cbind(c(px_info,labels)))
  for (i in 1:10) {
    px_info[i,2] <- paste(details[i],sep="")
  }
  cat(paste("#",date(),sep=""), "\n",  file=filename, append=TRUE)
  px_info <- data.frame(lapply(px_info, as.character), stringsAsFactors=FALSE)
  write.table(px_info,filename,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
  
  ######################################################################
  #
  # append threshold and presentation number data to px details file
  #
  #########################################################################
  
  cat(paste("#Test Duration: ",format(.POSIXct(sum(res$testTime)),"%M:%S"),sep=""),"\n",file=filename,append=TRUE)
  cat(paste("#Total Presentations: ", sum(res$n,na.rm=TRUE),sep=""),"\n",file=filename,append=TRUE)
  cat(paste("#FP errors: ",sum(res$fpc,na.rm=TRUE),"/",length(res$fpc)," (",signif(sum(res$fpc,na.rm=TRUE)/length(res$fpc)*100,digits=3),"%)",sep=""), "\n",file=filename,append=TRUE)
  cat(paste("#FN errors: ",sum(res$fnc,na.rm=TRUE),"/",length(res$fnc)," (",signif(sum(res$fnc,na.rm=TRUE)/length(res$fnc)*100,digits=3),"%)",sep=""), "\n",file=filename,append=TRUE)
 
  cat("\n#Location status (number of stimuli not seen)\n", file=filename, append=TRUE)
  write.table(expandVal(res$tr, finalVal), file=filename, append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
  cat("\n", file=filename, append=TRUE)
}

##########################################################################################################################
#
# a second writeFile function which writes a .csv file with information displayed in columns
# 
#
##########################################################################################################################
writeFile2 <- function (directory,details,res, finalVal){
  filename <- file.path(directory, paste0(details$dx,"_",details$gridType,"_Grid_Size_",details$stimSizeRoman,".csv"))
  
  #################################################################################
  # Arrange thresholds into an appropriate format
  #
  ################################################################################
  tr <- res$tr[with(res, order(tr$x, tr$y)),]
  vals <- finalVal(tr)
  names(vals) <- paste(tr$x, tr$y, sep = ",")
  vals <- t(data.frame(vals))

    ###############################################################################
  #
  # Compile the output spreadsheet
  #
  ###############################################################################
  output <- data.frame(ID=details$name,
                       Age = details$age,
                       Diagnosis = details$dx,
                       Manifest_Rx = details$MRx,
                       ORx = details$OR,
                       VA = details$VA,
                       Comments = details$comments,
                       Grid = paste0("'",details$gridType,"'"),
                       Stimulus_Size = details$stimSizeRoman, 
                       Eye = ifelse(details$eye == "right","RE","LE"),
                       Date = details$date,
                       Time = details$startTime,
                       Test_Duration = format(.POSIXct(sum(res$testTime)),"%M:%S"),
                       Presentations = sum(res$n,na.rm=TRUE),
                       FP_seen = sum(res$fpc,na.rm=TRUE),
                       FP_total = length(res$fpc),
                       FN_missed = sum(res$fnc,na.rm=TRUE),
                       FN_total = length(res$fnc))
  
  output <- cbind(output,vals)
  
  if (file.exists(filename)) {
    write.table(output,file=filename,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
  } else {
    write.table(output,file=filename,append=TRUE,row.names=FALSE,sep=",")
  }
}

###########################################################################################
# Function which writes a .csv file in a format that can be used in the "visualFields"
# package and creates printout using visualFields package
# see vfobject help for required fields
###########################################################################################
writeFile3 <- function (directory,details,res,finalVal,grid){
  filename <- file.path(directory, paste0(details$dx,"_",details$gridType,"_Grid_Size_",details$stimSizeRoman,"_vfPackage.csv")) 
  grids <- c("Screening_P-Total", "Screening_P-Central26", "Screening_P-Peripheral")
  pattern <- c("pPTv", "pPC26v", "pPeriv")[grids == grid]  
  output <- data.frame(id=details$name,
                       tperimetry = "sap",
                       talgorithm = "suprathreshold",
                       tpattern = pattern,
                       tdate = format(Sys.Date(),"%m/%d/%Y"),
                       ttime = paste0(unlist(strsplit(details$startTime,".",fixed=TRUE))[1],":",unlist(strsplit(details$startTime,".",fixed=TRUE))[2],":",unlist(strsplit(details$startTime,".",fixed=TRUE))[3]),
                       stype = details$dx,
                       sage = details$age,
                       seye = ifelse(details$eye == "right","OD","OS"),
                       sbsx = ifelse(details$eye == "right", 15, -15),
                       sbsy = -2,
                       sfp = round(sum(res$fpc,na.rm=TRUE)/length(res$fpc),digits=2), 
                       sfn = round(sum(res$fnc,na.rm=TRUE)/length(res$fnc),digits=2),
                       sfl = 0,
                       sduration = format(.POSIXct(sum(res$testTime),tz="GMT"),"%H:%M:%S"),
                       spause = format(.POSIXct(sum(res$tp),tz="GMT"),"%H:%M:%S"),
                       # fovth = ifelse(!is.null(z$thFovea),round(z$thFovea),NA),
                       np = sum(res$n,na.rm=TRUE),
                       # outnp = sum(unlist(z$npOutliers),na.rm=TRUE),
                       mistakes = mistakes,
                       deleted = deletes, 
                       comments = details$comments)
  
  tr <- res$tr[with(res, order(tr$x, tr$y)),]
  vals <- finalVal(tr)
  names(vals) <- paste0("L", 1:length(vals))
  vals <- t(data.frame(vals))
  
  finalOutput <- cbind(output,vals)
  
  if (file.exists(filename)) {
    write.table(finalOutput,file=filename,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",",na="")
  } else {
    write.table(finalOutput,file=filename,append=TRUE,row.names=FALSE,sep=",",na="")
  }
  
}

#####################################################################################################
# Compile a patient database in order to recall existing patient details for the input menu.
#
#
#####################################################################################################
px_database <- function (details) {
  output <- data.frame(ID=details$name,
                       Age = details$age,
                       Diagnosis = details$dx,
                       Manifest_Rx = details$MRx,
                       ORx = details$OR,
                       VA = details$VA,
                       Comments = details$comments,
                       Grid = details$gridType,
                       Stimulus_Size = details$stimSize, 
                       Eye = details$eye)
  filename <- file.path("..","patient_list.txt")
  if (file.exists(filename)) {
    PL <- read.csv(filename)
    if (any(grepl(paste0("^",details$name,"$"),PL$ID))) {
      if (!any(PL$Eye[which(PL$ID == details$name)] == details$eye)) {
        write.table(output,file=filename,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
      }
    } else {
      write.table(output,file=filename,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
    }
  } else {
    write.table(output,file=filename,append=TRUE,row.names=FALSE,sep=",")
  }
}


# NEEDS WORK!
# if (any(details$gridType == c("30-2","30-1","24-2","Peripheral","P-Peripheral","P-Central26"))) {  
###################################################################################################
# Create printout of data using visualFields package
###################################################################################################
VFPrintout <- function(directory, details){
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
  filename <- file.path(directory, paste0(details$dx,"_",details$gridType,"_Grid_Size_",details$stimSizeRoman,"_vfPackage.csv"))
  loadfile <- read.csv(filename)
  vf <- loadvfcsv( filename = filename, patternMap = eval(parse(text = paste0("saplocmap$",as.character((tail(loadfile$tpattern,1)))))))
  
  #generate unique file name for printout
  fname <- paste0(details$dx,"/",details$gridType," ",details$stimSizeRoman,"/",details$name,"_",details$dx,"_",details$gridType,"_",details$stimSizeRoman,"_",details$eye,"Eye_",details$date,"_",details$startTime,"_visualFields.pdf")
  #save printout
  vflayoutmw_singleField(vf[nrow(vf),], filename = fname)
}