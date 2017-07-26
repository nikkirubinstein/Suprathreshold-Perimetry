#############################################################
# 22/02/2015
# function written by Luke Chong to enter patient details
# uses tcltk package
#
# Modified by Nikki Rubinstein for screening test
# 24 July 2017
#############################################################
source("libraryCheckFunction.R")
libraryCheck("tcltk")
tclRequire("BWidget")

inputs <- function(){
  tt <- tktoplevel()
  #as.TclList <- function(object,...) UseMethod("as.TclList")
  #as.TclList.list <- function(stringList)
  #{
  #  result <-"{"
  #  for (i in (1:length(stringList)))
  #    result <- paste(result,"{",stringList[[i]],"} ",sep="")
  #  result <- paste(result,"}",sep="")
  #  result
  #}
  
  if (exists("px_info")) {
    var1 <- tclVar(as.character(px_info$ID))
    var2 <- tclVar(as.character(px_info$Age))
    var3 <- tclVar(as.character(px_info$Diagnosis))
    var4 <- tclVar(as.character(px_info$Comments))
    var5 <- tclVar(as.character(px_info$Manifest_Rx))
    var6 <- tclVar(as.character(px_info$ORx))
    # rbValue2 <- tclVar(as.numeric(px_info$Stimulus_Size))
    rbValue3 <- tclVar(as.character(px_info$Eye))
    comboVal1 <- tclVar(as.character(px_info$VA))
    comboVal2 <- tclVar("")
    # comboVal2 <- tclVar(as.character(px_info$Grid))
    
  } else {
    var1 <- tclVar("")
    var2 <- tclVar("")
    var3 <- tclVar("")
    var4 <- tclVar("")
    var5 <- tclVar("")
    var6 <- tclVar("")
    # rbValue2 <- tclVar(0.43)
    rbValue3 <- tclVar ("right")
    comboVal1 <- tclVar("20/20")
    comboVal2 <- tclVar("")
  }
  
  VA <- c("20/10","20/12.5","20/16","20/20","20/25","20/32","20/40","20/50","20/63","20/80","20/100","20/125",
             "20/160","20/200","20/250","20/320","20/400","20/500","20/630","20/800","CF","LP")
  comboBox <- tkwidget(tt,"ComboBox",editable=FALSE,values=VA,textvariable=comboVal1)
  # gridType <- c("Peripheral","30-2","30-1","24-2","G1","P-Total","P-Central10","P-Central26","P-Peripheral","P-Edge")
  # gridType <- c("Suprathreshold", "Practice")
  gridType <- c("Screening_P-Total", "Screening_P-Central26", "Screening_P-Peripheral")
  # grids <- c('total', 'central', 'peripheral')
  comboBox2 <- tkwidget(tt,"ComboBox",editable=FALSE,values=gridType,textvariable=comboVal2)
  
  rb4 <- tkradiobutton(tt)
  rb5 <- tkradiobutton(tt)
  rb6 <- tkradiobutton(tt)
  rb7 <- tkradiobutton(tt)
  rb8 <- tkradiobutton(tt)
  cb1 <- tkcheckbutton(tt)
  cb2 <- tkcheckbutton(tt)
  cb1Value <- tclVar("0")
  cb2Value <- tclVar("0")
  
  # tkconfigure(rb4,variable=rbValue2,value=0.43)
  # tkconfigure(rb5,variable=rbValue2,value=1.72)
  # tkconfigure(rb6,variable=rbValue2,value=3.44)
  tkconfigure(rb7,variable=rbValue3,value="right")
  tkconfigure(rb8,variable=rbValue3,value="left")
  tkconfigure(cb1,variable=cb1Value)
  tkconfigure(cb2,variable=cb2Value)
  
  tkwm.title(tt,"Patient Details - Screening Test")
  entry1 <- tkentry(tt, textvariable=var1,width="40")
  entry2 <- tkentry(tt, textvariable=var2,width="40")
  entry3 <- tkentry(tt, textvariable=var3,width="40")
  entry5 <- tkentry(tt, textvariable=var5,width="40")
  entry6 <- tkentry(tt, textvariable=var6,width="40")
  entry4 <- tkentry(tt, textvariable=var4,width="40")

  loadPx <- function () {
    tkdestroy(tt)
    patient_list <- tryCatch(read.csv(file.path("..", "patient_list.txt")),
                             error = function(x){
                               data.frame("ID" = c("", ""),"Age" = c("", ""),"Diagnosis" = c("", ""),"Manifest_Rx" = c("", ""),"ORx" = c("", ""),"VA" = c("20/20","20/20"),"Comments" = c("", ""),"Grid" = c("", ""),"Stimulus_Size" = c("", ""),"Eye" = c("right", "right"))
                               # names(a) <- c("ID","Age","Diagnosis","Manifest_Rx","ORx","VA","Comments","Grid","Stimulus_Size","Eye")
                               # return(a)
                             })
    tt <- tktoplevel()
    px <- patient_list[order(patient_list$ID),] # Arrange patients in alphabetical or numerical order
    if (px$ID[1] == ""){
      val <- c('1' = "There are no patients in the database", '2' = "")
    } else if (nrow(px) == 1) {
      px <- rbind(px, data.frame("ID" = "","Age" = "","Diagnosis" = "","Manifest_Rx" = "","ORx" = "","VA" = "20/20","Comments" = "","Grid" = "","Stimulus_Size" = "","Eye" = "right"))
      val <- c('1' = paste0(px[1,1],": ",px[1,10]," eye"), '2' = "")
    } else {
      val <- apply(px,1, function (x) {paste0(x[1],": ",x[10]," eye")})
    }

    comboBox <- tkwidget(tt,"ComboBox",editable=FALSE,values=val)
    
    submit <- function() {
      px <- px[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1,]
      environ <- parent.frame()
      environ$px_info <- px
      tkdestroy(tt)
      inputs()
    }
    
    cancel <- function () {
      tkdestroy(tt)
      inputs()
    }
    
    cancel.but <- tkbutton(tt,text="Cancel",command=cancel)
    submit.but <- tkbutton(tt, text="Submit", command=submit)
    tkgrid(tklabel(tt,text="Please Select Patient"),columnspan=5,padx=30,pady=10,sticky="s")
    tkgrid.configure(comboBox,columnspan=5,padx=30,sticky="s")
    tkgrid(submit.but,cancel.but,sticky="s",pady=10)
    tkgrid.configure(submit.but,column=1)
    tkgrid.configure(cancel.but,column=3)
    tkfocus(tt)
    tkbind(tt,"<Return>",submit)
    tkwait.window(tt)
  }
  
  submit <- function() {
    if (tclvalue(var1) == "") {
      tkmessageBox(message = "Please enter patient ID", icon = "warning", type = "ok")
    } else if (tclvalue(var2) == "") {
      tkmessageBox(message = "Please enter patient age", icon = "warning", type = "ok")
    } else if (is.na(as.numeric(tclvalue(var2)))){
      tkmessageBox(message = "Please enter a numeric value for the patient's age", icon = "warning", type = "ok")
    } else if (tclvalue(var3) == "") {
      tkmessageBox(message = "Please enter diagnosis", icon = "warning", type = "ok")
    } else if (!length(gridType[as.numeric(tclvalue(tcl(comboBox2,"getvalue")))+1])) {
      tkmessageBox(message = "Please enter test type", icon = "warning", type = "ok")
    } else if (tclvalue(rbValue3) == "") {
      tkmessageBox(message = "Please select a single eye", icon = "warning", type = "ok")
    } else {
      a <- tclvalue(var1)
      b <- tclvalue(var2)
      c <- tclvalue(var3)
      d <- tclvalue(var4)
      e <- gridType[as.numeric(tclvalue(tcl(comboBox2,"getvalue")))+1]
      # f1 <- tclvalue(rbValue2)
      g <- tclvalue(rbValue3)
      h <- as.logical(as.numeric(tclvalue(cb1Value)))
      i <- tclvalue(var5)
      j <- tclvalue(var6)
      k <- VA[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]
      l <- as.logical(as.numeric(tclvalue(cb2Value)))
      # 
      # if (f1 == 0.43) {
      #   f2 <- "III"
      # } else if (f1 == 1.72) {
        f1 <- 1.72
        f2 <- "V"
      # } else if (f1 == 3.44) {
      #   f2 <- "VI"
      # }
      
      env <- parent.frame()
      env$aaa <- a
      env$bbb <- b
      env$ccc <- c
      env$ddd <- d
      env$eee <- e
      env$f1 <- f1
      env$f2 <- f2
      env$ggg <- g
      # env$hhh <- h
      env$iii <- i
      env$jjj <- j
      env$kkk <- k
      env$lll <- l
      
      tkdestroy(tt)
    }
  }
  
  submit.but <- tkbutton(tt, text="   Submit   ", command=submit)
  loadpx.but <- tkbutton(tt, text="Add Existing Patient", command=loadPx)
  
  tkgrid(tklabel(tt,text="Enter Patient Details"),columnspan=12)
  tkgrid(tklabel(tt,text="I.D."), entry1,pady = 10, padx =10)
  tkgrid.configure(entry1,columnspan=7,sticky="new")
  tkgrid(tklabel(tt,text="Age"), entry2,pady = 10, padx =10)
  tkgrid.configure(entry2,columnspan=7,sticky="new")
  tkgrid(tklabel(tt,text="Diagnosis"), entry3, pady = 10, padx =10)
  tkgrid.configure(entry3,columnspan=7,sticky="new")
  tkgrid(tklabel(tt,text="Manifest\nRefraction"),entry5, pady = 10, padx =10)  
  tkgrid.configure(entry5,columnspan=7,sticky="new")
  tkgrid(tklabel(tt,text="Over-refraction"),entry6, pady = 10, padx =10)  
  tkgrid.configure(entry6,columnspan=7,sticky="new")
  tkgrid(tklabel(tt,text="Visual Acuity"),comboBox,pady=10,padx=10)
  tkgrid.configure(comboBox,columnspan=7,sticky="new")
  tkgrid(tklabel(tt,text="Additional\nComments"),entry4, pady = 10, padx =10)  
  tkgrid.configure(entry4,columnspan=7,sticky="new")

  tkgrid(tklabel(tt,text="Test Type  "),comboBox2,pady=10,padx=10)
  tkgrid.configure(comboBox2,columnspan=7,sticky="new")
  # tkgrid(tklabel(tt,text="     Stimulus\n     Size"),tklabel(tt,text="III"),rb4,tklabel(tt,text="V"),rb5,tklabel(tt,text="VI"),rb6,pady=10,padx=10,sticky="w")
  eyeLab <- tklabel(tt,text="Eye  ")
  tkgrid(eyeLab,tklabel(tt,text="Right"),rb7,tklabel(tt,text="Left"),rb8,
         pady=10,padx=10,sticky="w")
  tkgrid.configure(eyeLab,sticky="new")
  # tkgrid.configure(rb4,rb7,column=1,sticky="e")
  # tkgrid.configure(rb5,rb8,column=3,sticky="ns")
  # tkgrid(tklabel(tt,text=""),tklabel(tt,text="Foveal Test?"),cb1,tklabel(tt,text="Full From Prior?"),cb2,pady=10,padx=10,sticky="w")
  # tkgrid.configure(cb1,sticky="w")
  # tkgrid.configure(cb2,sticky="e",column=3)
  tkgrid(submit.but,loadpx.but,padx=10,pady=10,sticky="s")
  tkgrid.configure(submit.but,column=1)
  tkgrid.configure(loadpx.but,column=3)
  tkbind(tt,'<Return>', submit)
  tkwait.window(tt)
  
  # return(list(name=aaa,age=bbb,dx=ccc,MRx=iii,OR = ifelse(jjj=="","NO LENS",jjj),VA=kkk,comments=ddd,gridType=eee,stimSizeRoman=f2,eye=ggg,fovea=hhh,retest=lll,startTime=format(Sys.time(),"%H.%M.%S"),date=format(Sys.Date(),"%d-%m-%Y"),stimSize=f1))
  return(list(name=aaa,age=bbb,dx=ccc,MRx=iii,OR = ifelse(jjj=="","NO LENS",jjj),VA=kkk,comments=ddd,gridType=eee,stimSizeRoman=f2,eye=ggg,startTime=format(Sys.time(),"%H.%M.%S"),date=format(Sys.Date(),"%d-%m-%Y"),stimSize=f1))
}

###################################################################################################################
#
# Edit box that appears upon completion of the test for perimetrist to add in any closing comments
#
#
##################################################################################################################
finalComments <- function(){
  tt <- tktoplevel()  
  var1 <- tclVar("")
  tkwm.title(tt,"Final Comments")
  entry1 <- tkentry(tt, textvariable=var1,width="40")
  
  submit <- function() {
      a <- tclvalue(var1)
      env <- parent.env(environment())
      env$a <- a      
      tkdestroy(tt)
  }
  submit.but <- tkbutton(tt, text="   Submit   ", command=submit)
  
  tkgrid(tklabel(tt,text="Please enter any final comments\n (leave blank and press submit\n if no comments to add)"),columnspan=9)
  tkgrid.configure(entry1,columnspan=7)
  tkgrid(submit.but,columnspan=4,padx=10,pady=10,sticky="s",column=1)
  tkfocus(tt)
  tkbind(tt,"<Return>",submit)
  tkwait.window(tt)
  
  return(a)
}

###################################################################################################################
# create a window to query if operator wants to run a practice test
# Gives operator option to choose stimulus size and eye
#
#
####################################################################################################################
practiceQuery <- function () {
    tt <- tktoplevel()
    
    var2 <- tclVar("50")
    # rb1 <- tkradiobutton(tt)
    # rb2 <- tkradiobutton(tt)
    # rb3 <- tkradiobutton(tt)
    rb4 <- tkradiobutton(tt)
    rb5 <- tkradiobutton(tt)
    
    # rbValue1 <- tclVar(0.43)
    entry2 <- tkentry(tt, textvariable=var2,width="40")
    rbValue2 <- tclVar ("right")
    
    
    # tkconfigure(rb1,variable=rbValue1,value=0.43)
    # tkconfigure(rb2,variable=rbValue1,value=1.72)
    # tkconfigure(rb3,variable=rbValue1,value=3.44)
    tkconfigure(rb4,variable=rbValue2,value="right")
    tkconfigure(rb5,variable=rbValue2,value="left")    
    
    tkwm.title(tt,"")
    
    yes <- function() {
      if (tclvalue(var2) == "") {
        tkmessageBox(message = "Please enter patient age", icon = "warning", type = "ok")
      } else if (is.na(as.numeric(tclvalue(var2)))){
        tkmessageBox(message = "Please enter a numeric value for the patient's age", icon = "warning", type = "ok")
      } else {
        # a <- tclvalue(rbValue1)
        b <- tclvalue(rbValue2)
        d <- tclvalue(var2)
        
        env <- parent.env(environment())
        env$a <- 1.72
        env$b <- b
        env$d <- d
        
        # if (a == 0.43) {
        #   f2 <- "III"
        # } else if (a == 1.72) {
        #   f2 <- "V"
        # } else if (a == 3.44) {
        #   f2 <- "VI"
        # }
        f2 <- "V"
        
        env$f2 <- f2
        
        tkdestroy(tt)
        env$prac <- TRUE
    }}
    
    no <- function () {
      tkdestroy(tt)
      env <- parent.env(environment())
      env$prac <- FALSE
    }
    
    yes.but <- tkbutton(tt, text="   Yes   ", command=yes)
    no.but <- tkbutton(tt, text="   No   ", command=no)
    
    
    tkgrid(tklabel(tt,text="Run a practice test?"),columnspan=9)
    # tkgrid(tklabel(tt,text="Stimulus\nSize"),tklabel(tt,text="III"),rb1,tklabel(tt,text="V"),rb2,tklabel(tt,text="VI"),rb3,pady=10,padx=10)
    tkgrid(tklabel(tt,text="Age"), entry2,pady = 10, padx =10)
    tkgrid.configure(entry2,columnspan=7,sticky="new")
    eyeLab <- tklabel(tt,text="Eye")
    tkgrid(eyeLab,tklabel(tt,text="Right"),rb4,tklabel(tt,text="Left"),rb5,
           pady=10,padx=10)
    tkgrid.configure(eyeLab,sticky="new")
    tkgrid(yes.but,no.but,sticky="sew",padx=10,pady=10)
    tkgrid.configure(yes.but,column=2)
    tkgrid.configure(no.but,column=3)
    tkfocus(tt)
    tkwait.window(tt)
    
    if (prac == FALSE) {
      return (list(practice=FALSE))
    } else if (prac==TRUE) {
      return(list(stimSize=a,eye=b,age=d,stimSizeRoman=f2,fovea=FALSE,gridType="Practice",practice=TRUE))
    }
}

            
      




