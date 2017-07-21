# Code written by Luke Chong

if((!'tcltk' %in% installed.packages()))
  install.packages("tcltk")
library(tcltk)
library(OPI)

timePaused <- 0
mistakes <- 0
deletes <- 0

mywait <- function() {
  tt <- tktoplevel()
  tktitle(tt) <- "Test Paused"
  resume.but <- tkbutton(tt, text='Resume Test', command=function () {
    pauseTimerStop <- Sys.time()
    timePaused <<- timePaused + (pauseTimerStop - pauseTimerStart)
    tkdestroy(tt)
    pause.button()
  })
  
  terminate.but <- tkbutton(tt, text='Terminate Test', command=function () {
    tkdestroy(tt)
    tt <- tktoplevel()
    tktitle(tt) <- "Are you sure?"
    yes <- tkbutton(tt,text="    Yes    ",command= function () {
      tkdestroy(tt)
      gRunning <<- FALSE
      print("Test terminated by operator")
    })
    no <- tkbutton(tt,text= "    No    ",command= function () {
      tkdestroy(tt)
      mywait()
    })
    tkgrid (tklabel(tt,text="Are you sure you want to terminate this test?"),padx=10,pady=10,columnspan=3)
    tkgrid(yes,no,sticky="sew",padx=10,pady=10)
    tkfocus(tt)
    tkwait.window(tt)
  })
  
  undo.but <- tkbutton(tt, text = "Delete Previous Presentations", command = function () {
    tkdestroy(tt)
    tt <- tktoplevel()
    tktitle(tt) <- "Delete previous presentations"
    as.TclList <- function(object,...) UseMethod("as.TclList")
    as.TclList.list <- function(stringList) {
      result <-"{"
      for (i in (1:length(stringList)))
        result <- paste(result,"{",stringList[[i]],"} ",sep="")
      result <- paste(result,"}",sep="")
      result
    }
    undo <- list(1,2,3,4,5)
    undoTclList <- as.TclList(undo)
    comboBox <- .Tk.subwin(tt)
    .Tcl(paste("ComboBox",.Tk.ID(comboBox),"-editable false -values",undoTclList))
    
    submit.but <- tkbutton(tt, text="   Submit   ", command= function () {
      e <- undo[[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]]
      gUndos <<- e
      deletes <<- deletes + e
      mistakes <<- mistakes + 1
      tkdestroy(tt)
      tkmessageBox(title = "", message = paste("The last",e,"presentations have been deleted",sep=" "), icon = "warning")
      mywait()
    })
    
    back.but <- tkbutton(tt,text="Go Back", command = function () {
      tkdestroy(tt)
      mywait()
    })
    
    tkgrid(tklabel(tt,text="Select number of presentations to delete"),sticky="new",padx=10,pady=10)
    tkgrid.configure(comboBox,columnspan=7,sticky="new")
    tkgrid(submit.but,back.but,sticky="sew",padx=10,pady=10)
    tkwait.window(tt)
  })
  
  tkgrid(resume.but,terminate.but,undo.but,padx=20,pady=30)
  tkbind(tt,'<Return>', function() {
    pauseTimerStop <<- Sys.time()
    timePaused <<- timePaused + (pauseTimerStop - pauseTimerStart) 
    tkdestroy(tt)
    pause.button()
  })
  tkfocus(tt)
  tkwait.window(tt)
}

pause.button <- function() {
  tt <<- tktoplevel()
  tkpack(tkbutton(tt, text = "Pause Test", 
                  command = function() {
                    pauseTimerStart <<- Sys.time()
                    tkdestroy(tt)
                    mywait()
                  }),side='bottom',pady=30,padx=50)
  
  tktitle(tt) <- ""
  tkbind(tt,'<Return>', function() {
    pauseTimerStart <<- Sys.time()
    tkdestroy(tt)
    mywait()
  })
  tkfocus(tt)
}

pauseAtStart <- function(test) {
  tt <- tktoplevel()
  tktitle(tt) <- "Press button to begin test"
  tkgrid(tklabel(tt,text=paste0("Press button to start\n", test," test")),sticky="new",padx=20)
  tkgrid(tkbutton(tt,text='Begin Test', command=function () {
    tkdestroy(tt)
    pause.button()
  }), sticky='wes',pady=10,padx=10)
  tkfocus(tt)
  tkbind(tt,'<Return>',function () {
    tkdestroy(tt)
    pause.button()
  })
  tkwait.window(tt)
}

pauseAtStartFovea <- function() {
  tt <- tktoplevel()
  tktitle(tt) <- "Press button to begin test"
  tkgrid(tklabel(tt,text="Press button to start \n foveal testing"),sticky="new",padx=10)
  tkgrid(tkbutton(tt,text='Begin Test', command=function () {
    tkdestroy(tt)
    #pause.button()
  }), sticky="wes",pady=10,padx=10)
  tkfocus(tt)
  tkbind(tt,'<Return>',function () {tkdestroy(tt)})
  tkwait.window(tt)
}

# fovealTestComplete <- function (threshold) {
#   tt <- tktoplevel()
#   tktitle(tt) <- "Test Complete"
#   yes.but <- tkbutton(tt, text = " Yes ", command = function() {
#     tkdestroy(tt)
#   })
#   
#   no.but <- tkbutton(tt, text = " No ", command = function() {
#     tkdestroy(tt)
#     .GlobalEnv$details$fovea <- FALSE
#   })
#   
#   tkgrid(tklabel(tt,text=paste0("Foveal threshold is ",round(threshold)," dB.   \n 
#                                 Do you want to repeat this test?    ")),padx=10,pady=10,columnspan=9)
#   tkgrid(yes.but, no.but,sticky="sew",pady=10,padx=5)
#   tkgrid.configure(yes.but,column=3,columnspan=1)
#   tkgrid.configure(no.but,column=5,columnspan=1)
#   tkfocus(tt)
#   tkwait.window(tt)
# }

pracTestComplete <- function () {
  tkmessageBox(title = "", message = "Practice Test Complete!", icon = "warning")
} 

testComplete<- function () {
  tt <- tktoplevel()
  yes.but <- tkbutton(tt,text="    Yes    ",command= function () {tkdestroy(tt)})
  no.but <- tkbutton(tt,text= "    No    ",command= function () {
    tkdestroy(tt)
    tt <- tktoplevel()
    
    yes <- tkbutton(tt,text="    Yes    ",command= function () {
      tkdestroy(tt)
      gRunning <<- FALSE
      graphics.off()
    })
    
    no <- tkbutton(tt,text= "    No    ",command= function () {
      tkdestroy(tt)
      testComplete()
    })
    
    tkgrid(tklabel(tt,text="Are you sure you want to discard test without saving?"),padx=10,pady=10,columnspan=3,sticky="w")
    tkgrid(yes,no,sticky="e",pady=20,padx=5)
    tkwait.window(tt)
  })
  
  tkgrid (tklabel(tt,text="Test Complete! \nDo you want to save this test?"),padx=10,pady=10,columnspan=3)
  tkgrid(yes.but,no.but,sticky="sew",pady=20,padx=5)
  tkwait.window(tt)
}