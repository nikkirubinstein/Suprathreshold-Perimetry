#############################################################
# 22/02/2015
# function written by Luke Chong to enter patient details
# uses tcltk package
#############################################################

require(tcltk)
tclRequire("BWidget")
inputs <- function(){
  tt <- tktoplevel()
  as.TclList <- function(object,...) UseMethod("as.TclList")
  as.TclList.list <- function(stringList)
  {
    result <-"{"
    for (i in (1:length(stringList)))
      result <- paste(result,"{",stringList[[i]],"} ",sep="")
    result <- paste(result,"}",sep="")
    result
  }
  
  var1 <- tclVar("")
  var2 <- tclVar("")
  var3 <- tclVar("")
  var4 <- tclVar("")
  var5 <- tclVar("")
  var6 <- tclVar("")
  
  VA <- c("20/10","20/12.5","20/16","20/20","20/25","20/32","20/40","20/50","20/63","20/80","20/100","20/125",
             "20/160","20/200","20/250","20/320","20/400","20/500","20/630","20/800","CF","LP")
  comboBox <- tkwidget(tt,"ComboBox",editable=FALSE,values=VA,textvariable=tclVar("20/20"))
  gridType <- c("Peripheral","30-2","30-1","24-2")
  comboBox2 <- tkwidget(tt,"ComboBox",editable=FALSE,values=gridType) 
  
  rb4 <- tkradiobutton(tt)
  rb5 <- tkradiobutton(tt)
  rb6 <- tkradiobutton(tt)
  rb7 <- tkradiobutton(tt)
  rb8 <- tkradiobutton(tt)
  cb <- tkcheckbutton(tt)  
  
  rbValue2 <- tclVar(0.43)
  rbValue3 <- tclVar ("right")
  cbValue <- tclVar("0")
  
  tkconfigure(rb4,variable=rbValue2,value=0.43)
  tkconfigure(rb5,variable=rbValue2,value=1.72)
  tkconfigure(rb6,variable=rbValue2,value=3.44)
  tkconfigure(rb7,variable=rbValue3,value="right")
  tkconfigure(rb8,variable=rbValue3,value="left")
  tkconfigure(cb,variable=cbValue)
  
  
  tkwm.title(tt,"Patient Details")
  entry1 <- tkentry(tt, textvariable=var1,width="40")
  entry2 <- tkentry(tt, textvariable=var2,width="40")
  entry3 <- tkentry(tt, textvariable=var3,width="40")
  entry5 <- tkentry(tt, textvariable=var5,width="40")
  entry6 <- tkentry(tt, textvariable=var6,width="40")
  entry4 <- tkentry(tt, textvariable=var4,width="40")

  
  
  submit <- function() {
    if (tclvalue(var1) == "") {
      tkmessageBox(message = "Please enter patient ID", icon = "warning", type = "ok")
    }
    if (tclvalue(var2) == "") {
      tkmessageBox(message = "Please enter patient age", icon = "warning", type = "ok")
    }
    if (tclvalue(var3) == "") {
      tkmessageBox(message = "Please enter diagnosis", icon = "warning", type = "ok")
    } else {
      a <- tclvalue(var1)
      b <- tclvalue(var2)
      c <- tclvalue(var3)
      d <- tclvalue(var4)
      e <- gridType[as.numeric(tclvalue(tcl(comboBox2,"getvalue")))+1]
      f <- tclvalue(rbValue2)
      g <- tclvalue(rbValue3)
      h <- as.logical(as.numeric(tclvalue(cbValue)))
      i <- tclvalue(var5)
      j <- tclvalue(var6)
      k <- VA[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]
      
      env <- parent.env(environment())
      env$a <- a
      env$b <- b
      env$c <- c
      env$d <- d
      env$e <- e
      env$f <- f
      env$g <- g
      env$h <- h
      env$i <- i
      env$j <- j
      env$k <- k

      tkdestroy(tt)
    }
  }
  submit.but <- tkbutton(tt, text="   Submit   ", command=submit)
  
  
  tkgrid(tklabel(tt,text="Enter Patient Details"),columnspan=9)
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

  tkgrid(tklabel(tt,text="Grid Type  "),comboBox2,pady=10,padx=10)
  tkgrid.configure(comboBox2,columnspan=7,sticky="new")
  tkgrid(tklabel(tt,text="Stimulus\nSize"),tklabel(tt,text="III"),rb4,tklabel(tt,text="V"),rb5,tklabel(tt,text="VI"),rb6,pady=10,padx=10)
  eyeLab <- tklabel(tt,text="Eye")
  tkgrid(eyeLab,tklabel(tt,text="Right"),rb7,tklabel(tt,text="Left"),rb8,
         pady=10,padx=10)
  tkgrid.configure(eyeLab,sticky="new")
  tkgrid.configure(rb7,rb8,sticky="w")
  tkgrid(tklabel(tt,text="  Test Foveal Threshold?"),cb,pady=10,padx=10,sticky="new",columnspan=2)
  tkgrid.configure(cb,sticky="w")
  tkgrid(submit.but,columnspan=4,padx=10,pady=10,sticky="s",column=1)
  tkbind(tt,'<Return>', submit)
  tkwait.window(tt)
  
  if (f == 0.43) {
    f2 <- "III"
  } else if (f == 1.72) {
      f2 <- "V"
  } else if (f == 3.44) {
        f2 <- "VI"
  }
    
  return(list(name=a,age=b,dx=c,comments=d,gridType=e,stimSizeRoman=f2,eye=g,fovea=h,startTime=format(Sys.time(),"%H.%M.%S"),date=format(Sys.Date(),"%d-%m-%Y"),stimSize=f,MRx=i,OR=j,VA=k))
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
    
    rb1 <- tkradiobutton(tt)
    rb2 <- tkradiobutton(tt)
    rb3 <- tkradiobutton(tt)
    rb4 <- tkradiobutton(tt)
    rb5 <- tkradiobutton(tt)
    
    rbValue1 <- tclVar(0.43)
    rbValue2 <- tclVar ("right")
    
    tkconfigure(rb1,variable=rbValue1,value=0.43)
    tkconfigure(rb2,variable=rbValue1,value=1.72)
    tkconfigure(rb3,variable=rbValue1,value=3.44)
    tkconfigure(rb4,variable=rbValue2,value="right")
    tkconfigure(rb5,variable=rbValue2,value="left")    
    
    tkwm.title(tt,"")
    
    yes <- function() {
        a <- tclvalue(rbValue1)
        b <- tclvalue(rbValue2)
        
        env <- parent.env(environment())
        env$a <- a
        env$b <- b
        
        if (a == 0.43) {
          f2 <- "III"
        } else if (a == 1.72) {
          f2 <- "V"
        } else if (a == 3.44) {
          f2 <- "VI"
        }
        
        env$f2 <- f2
        
        tkdestroy(tt)
        env$prac <- TRUE
    }
    
    no <- function () {
      tkdestroy(tt)
      env <- parent.env(environment())
      env$prac <- FALSE
    }
    
    yes.but <- tkbutton(tt, text="   Yes   ", command=yes)
    no.but <- tkbutton(tt, text="   No   ", command=no)
    
    
    tkgrid(tklabel(tt,text="Run a practice test?"),columnspan=9)
    tkgrid(tklabel(tt,text="Stimulus\nSize"),tklabel(tt,text="III"),rb1,tklabel(tt,text="V"),rb2,tklabel(tt,text="VI"),rb3,pady=10,padx=10)
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
      return(list(stimSize=a,eye=b,stimSizeRoman=f2,fovea=FALSE,gridType="practice",practice=TRUE))
    }
}

            
      




