####################################
# for simulation
####################################
require(OPI)
#chooseOpi("SimHenson")
chooseOpi("SimGaussian")
#chooseOpi("SimYes")
#chooseOpi("SimNo")
opiInitialize(sd=1)
source("query_patient_details.r")
gRunning <- TRUE

TT <- matrix(c(
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  2, NA, NA,  2,    2, NA, NA,  2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  2, NA, NA, NA,   NA, NA, NA,  2, NA,  3, NA,  3, NA,  4, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA,  3, NA,  2, NA, NA, NA, NA,   NA, NA, NA, NA,  2, NA,  2, NA,  3, NA,  3, NA,  4, NA, NA,
  NA, NA, NA, NA, NA, NA, NA,  3, NA,  1, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA,  2, NA,  1, NA,  2, NA,  3, NA, NA, NA,
  NA, NA, NA, NA, NA, NA,  4, NA,  2, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA,  2, NA,  3, NA,  3, NA, NA,  4, NA,
  NA, NA, NA, NA, NA, NA,  3, NA,  2, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA,  2, NA,  3, NA, NA,  3, NA, NA,  4,
                         
  NA, NA, NA, NA, NA, NA,  3, NA,  2, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA,  2, NA,  3, NA, NA,  3, NA, NA,  4,
  NA, NA, NA, NA, NA, NA,  4, NA,  2, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA,  2, NA,  3, NA,  3, NA, NA,  4, NA,
  NA, NA, NA, NA, NA, NA, NA,  3, NA,  2, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA,  2, NA,  2, NA,  3, NA,  3, NA,  4, NA,
  NA, NA, NA, NA, NA, NA, NA, NA,  3, NA,  1, NA, NA, NA, NA,   NA, NA, NA, NA,  2, NA,  1, NA,  2, NA,  3, NA,  4, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA,  3, NA,  2, NA, NA, NA,   NA, NA, NA,  2, NA,  2, NA,  3, NA,  3, NA, NA, NA,  4, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  3, NA,  2, NA, NA,   NA, NA,  2, NA,  2, NA, NA, NA, NA, NA,  3, NA,  4, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  3, NA, NA,  2,    2, NA, NA,  3, NA, NA,  3, NA,  4, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA,  4, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  4, NA,  4,    4, NA,  4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
), nrow=18, ncol=30, byrow=TRUE)

#TT <- matrix(c(
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 30, NA, NA, 0,   30, NA, NA, 30, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 30, NA, NA, NA,   NA, NA, NA, 30, NA, 30, NA, 30, NA, 30, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, 30, NA, 30, NA, NA, NA, NA,   NA, NA, NA, NA, 10, NA, 30, NA, 30, NA, 30, NA, 30, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, 30, NA, 30, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, 30, NA, 30, NA, 30, NA, 30, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, 30, NA, 30, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, 30, NA, 30, NA, 30, NA, NA, 30, NA,
#  NA, NA, NA, NA, NA, NA, 0, NA, 30, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, 30, NA, 30, NA, NA, 30, NA, NA, 30,
                         
#  NA, NA, NA, NA, NA, NA, 0, NA, 30, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, 30, NA, 30, NA, NA, 30, NA, NA, 30,
#  NA, NA, NA, NA, NA, NA, 30, NA, 30, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, 30, NA, 30, NA, 30, NA, NA, 30, NA,
#  NA, NA, NA, NA, NA, NA, NA, 30, NA, 10, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, 30, NA, 30, NA, 30, NA, 30, NA, 30, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, 30, NA, 30, NA, NA, NA, NA,   NA, NA, NA, NA, 30, NA, 30, NA, 30, NA, 30, NA, 30, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, 30, NA, 30, NA, NA, NA,   NA, NA, NA, 30, NA, 30, NA, 30, NA, 30, NA, NA, NA, 30, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 30, NA, 30, NA, NA,   NA, NA, 0, NA, 30, NA, NA, NA, NA, NA, 30, NA, 30, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 30, NA, NA, 30,   30, NA, NA, 30, NA, NA, 30, NA, 30, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, 30, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 30, NA, 30,   0, NA, 30, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
#), nrow=18, ncol=30, byrow=TRUE)

#TT <- matrix(c(
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  4,  4,  3,  4,  4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  4,  3,  3,  3,  3,  3,  4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  4,  3,  25,  2,  2,  2,  2,  3,  4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  4,  3,  25, 1,  2,  1, NA, NA,  4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  4,  3,  3,  25,  2, NA,  2, NA, NA,  3,  4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  4,  3,  2,  2, 2,  1, NA, NA,  4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  4,  3,  2,  2,  2,  2,  2,  3,  4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  4,  3,  3,  3,  3,  3,  4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  20,  4,  3,  4,  4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
#), nrow=19, ncol=31, byrow=TRUE) 
#print(z<-Zest242(eye="right", primaryStartValue=25, gridType="Peripheral", tt=ttt))

#tt <- matrix(c(  # for Tester
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, 35, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
#), nrow=18, ncol=30, byrow=TRUE)

tt.practice <- matrix(c(
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA,  1, NA, NA, NA, NA,  1,   NA, NA, NA, NA,  1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,    1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  1, NA, NA,   NA, NA, NA, NA, NA, NA,  1, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  1, NA, NA, NA,   NA, NA,  1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
), nrow=18, ncol=30, byrow=TRUE)

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
PSV <- setPSV(details$grid,details$stimSizeRoman)
#trials <- 500
#for ( i in 1:trials) {
#print(i)

if (details$eye == "left") {
  tt <- grid.flip(tt)
}
  z<-Zest242(eye=details$eye, primaryStartValue=PSV, gridType=details$grid,outlierFreq=2,interStimInterval=c(0,0), tt=TT)
  np <- c(np,sum(unlist(z$np),na.rm=TRUE)) # avg np 282 for tt=30
#}
terminate <- Sys.time()
tkdestroy(tt)
graphics.off()

if (gRunning) {
  windows(700,250)
  testStatusFinal(z)
  pdf(file = paste(details$dx,"/",details$name,"_",details$dx,"_",details$grid,"_",details$stimSizeRoman,"_",details$eye,"Eye_",details$date,"_",details$startTime,".pdf",sep=""),width=13,height=5)
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
}

