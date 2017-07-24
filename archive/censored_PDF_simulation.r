####################################
# for simulation
####################################
start <-Sys.time()
setwd("D:/Documents/LUKE CHONG/work for Wall/Source")

require(OPI)
chooseOpi("SimHenson")
#chooseOpi("SimGaussian")
#chooseOpi("SimYes")
#chooseOpi("SimNo")
#opiInitialize(sd=1)
opiInitialize(type="C", cap=6, display=NULL,maxStim=4000/pi)
source("query_patient_details.r")
details <- inputs()

####################################
# Set constant variables
#
#####################################
#if (details$stimSizeRoman == "III") {
#  MIN_DOMAIN <- 20
#} else if (details$stimSizeRoman == "V" || details$stimSizeRoman == "VI") {
  MIN_DOMAIN <- 15
#}

TRIALS <- 20

####################################
# define test grids
#
####################################

Peripheral_TT <- testPattern(grid.peripheral.coords)
Peripheral_TT[14815] <- 30

TT.30.1 <- testPattern(grid.30.1.coords)
TT.30.1[which(TT.30.1 == 2)] <- 30

TT.30.2 <- testPattern(grid.30.2.coords)
TT.30.2ref <- testPattern(grid.30.2.coords) 
TT.30.2[!is.na(TT.30.2)] <- 33
#TT.30.2[which(TT.30.2 == 1)] <- 35
#TT.30.2[which(TT.30.2 == 2)] <- 33
#TT.30.2[which(TT.30.2 == 3)] <- 31
#TT.30.2[which(TT.30.2 == 4)] <- 29

TT.24.2 <- testPattern(grid.24.2.coords)
TT.24.2[which(TT.24.2 == 1)] <- 30

TT.G1 <- testPattern(grid.G1.coords)
TT.G1[which(TT.G1 == 1)] <- 30

TT.PTotal <- testPattern(grid.PTotal.coords)
TT.PTotal[which(TT.PTotal == 1)] <- 30

TT.PCentral10 <- testPattern(grid.PCentral10.coords)
TT.PCentral10[which(TT.PCentral10 == 2)] <- 30

tt.practice <- testPattern(grid.practice.coords)

###############################################################################################
#
# Begin test!!
#
##############################################################################################
gRunning <- TRUE
if (dir.exists(paste0(details$dx,"/",details$gridType," ",details$stimSizeRoman)) == FALSE) {dir.create(paste0(details$dx,"/",details$gridType," ",details$stimSizeRoman),recursive = TRUE)}
PSV <- setPSV(details$gridType,details$stimSizeRoman)

ae_list <- NULL
np_list <- NULL
th_list <- NULL
for (tt in MIN_DOMAIN:35) {
  print(tt)
  if (details$gridType == "Peripheral") {
    Peripheral_TT[!is.na(Peripheral_TT)] <- tt
    TT <- Peripheral_TT
  } else if (details$gridType == "30-1") {
    TT.30.1[!is.na(TT.30.1)] <- tt
    TT <- TT.30.1
  } else if (details$gridType == "30-2") {
    #TT.30.2[!is.na(TT.30.2)] <- tt
    TT.30.2[which(TT.30.2ref == 2)] <- tt
    TT <- TT.30.2
  } else if (details$gridType == "G1") {
    TT.G1[!is.na(TT.G1)] <- tt
    TT <- TT.G1
  } else if (details$gridType == "24-2") {
    TT.24.2[!is.na(TT.24.2)] <- tt
    TT <- TT.24.2
  } else if (details$gridType == "practice") {
    tt.practice[!is.na(tt.practice)] <- tt
    TT <- tt.practice
  } else if (details$gridType == "P-Total") {
    TT.PTotal[!is.na(TT.PTotal)] <- tt
    TT <- TT.PTotal
  } else if (details$gridType == "P-Central10") {
    TT.PCentral10[!is.na(TT.PCentral10)] <- tt
    TT <- TT.PCentral10
  }
  
  if (details$eye == "left") {
    TT <- grid.flip(TT)
  }
  
  np <- NULL
  ae <- NULL
  th <- NULL
  for ( i in 1:TRIALS) {
    z<-Zest242(eye=details$eye, primaryStartValue=PSV, gridType=details$gridType,outlierFreq=6,outlierValue=100,minInterStimInterval=0, tt=TT,moveProjector = FALSE)
    resNP <- z$np[rowSums(is.na(z$np)) != ncol(z$np), colSums(is.na(z$np)) != nrow(z$np)] 
    resNP <- as.vector(t(resNP))
    np <- rbind(np,resNP) # avg np 282 for tt=30
    
    resAE <- z$ae[rowSums(is.na(z$ae)) != ncol(z$ae), colSums(is.na(z$ae)) != nrow(z$ae)] #remove rows and cols with NA
    resAE <- as.vector(t(resAE))
    ae <- rbind(ae,resAE)
    
    resTH <- z$th[rowSums(is.na(z$th)) != ncol(z$th), colSums(is.na(z$th)) != nrow(z$th)] #remove rows and cols with NA
    resTH <- as.vector(t(resTH))
    th <- rbind(th,resTH)
    #writeFile()
    #writeFile2(details)
    graphics.off()
  }
  ae_list <- c(ae_list,list(ae))
  np_list <- c(np_list,list(np))
  th_list <- c(th_list,list(th))
}

resList <- list(ae=ae_list,np=np_list,th=th_list)
save(resList,file = paste0("../Results/Simulation Results/Gauss_SD6_localLoss_",details$gridType,"_",details$stimSizeRoman,".RData"))

end <- Sys.time()
print(end - start)


