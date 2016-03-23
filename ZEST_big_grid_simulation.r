####################################
# for simulation
####################################
require(OPI)
chooseOpi("SimHenson")
#chooseOpi("SimGaussian")
#chooseOpi("SimYes")
#chooseOpi("SimNo")
#opiInitialize(sd=1)
opiInitialize(type="C", cap=6, display=NULL)
source("query_patient_details.r")
gRunning <- TRUE

Peripheral_TT <- testPattern(grid.peripheral.coords)
Peripheral_TT[14815] <- 30

TT.30.1 <- testPattern(grid.30.1.coords)
TT.30.1[which(TT.30.1 == 2)] <- 30

TT.30.2 <- testPattern(grid.30.2.coords)
TT.30.2[which(TT.30.2 == 2)] <- 30

TT.24.2 <- testPattern(grid.24.2.coords)
TT.24.2[which(TT.24.2 == 1)] <- 30

TT.G1 <- testPattern(grid.G1.coords)
TT.G1[which(TT.G1 == 1)] <- 30

tt.practice <- testPattern(grid.practice.coords)

###############################################################################################
#
# Begin test!!
#
##############################################################################################
details <- practiceQuery()

while (details$practice == TRUE) {
  Zest242(eye=details$eye, primaryStartValue=30, gridType="practice",outlierValue=5,outlierFreq=2,tt=tt.practice)
  tkdestroy(tt)
  pracTestComplete()
  dev.off()
  details <- practiceQuery()
}

details <- inputs()
if (dir.exists(details$dx) == FALSE) {dir.create(details$dx)}
np <- NULL
PSV <- setPSV(details$gridType,details$stimSizeRoman)
#trials <- 500
#for ( i in 1:trials) {
#print(i)

if (details$gridType == "Peripheral") {
  TT <- Peripheral_TT
} else if (details$gridType == "30-1") {
  TT <- TT.30.1
} else if (details$gridType == "30-2") {
  TT <- TT.30.2
} else if (details$gridType == "G1") {
  TT <- TT.G1
} else if (details$gridType == "24-2") {
  TT <- TT.24.2
} else if (details$gridType == "practice") {
  TT <- tt.practice
}

if (details$eye == "left") {
  TT <- grid.flip(TT)
}

  z<-Zest242(eye=details$eye, primaryStartValue=PSV, gridType=details$gridType,outlierFreq=2,outlierValue=5,minInterStimInterval=0, tt=TT,moveProjector = FALSE)
  np <- c(np,sum(unlist(z$np),na.rm=TRUE)) # avg np 282 for tt=30
#}
terminate <- Sys.time()
tkdestroy(tt)
graphics.off()

if (gRunning) {
  windows(900,350)
  testStatusFinal(z)
  pdf(file = paste(details$dx,"/",details$name,"_",details$dx,"_",details$gridType,"_",details$stimSizeRoman,"_",details$eye,"Eye_",details$date,"_",details$startTime,".pdf",sep=""),width=14,height=6)
  testStatusFinal(z)
  dev.off()
  testComplete()
}

if (gRunning) {
  comments <- finalComments()
  details$comments <- paste(details$comments,comments,sep=".")
  writeFile()
  writeFile2(details)
  writeFile3(details)
  px_database(details)
}

