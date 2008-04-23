## some dialogs for R
## dialogs don't get windows, they make them
## dialogs are modal
## dialogs return their value -- not an object. so source(gfile()) should work

## we don't implement gbasiddialog. -- how to do so not clear?

## TODO:

## used to create all three dialogs
tcltkDialog = function(
  message,
  text = "",
  title = "Input",
  icon = c("info","warning","error","question"),
  type = c("message","confirm","input"),
  parent = NULL,
  handler = NULL,
  action = NULL,
  ...
  ) {

  ## top level widnow
  dlg <- tktoplevel()

  if(!is.null(parent)) {
    parent <- getBlock(parent) ## needs to be top level window
    curgeo <- tclvalue(tkwm.geometry(parent))
    ## widthXheight+xpos+ypos
    pos <- unlist(strsplit(curgeo, "\\+"))
    sz <- unlist(strsplit(pos[1],"x"))
    xpos = as.numeric(pos[2]); ypos=as.numeric(pos[3])
    tkwm.geometry(dlg,paste("+",xpos+10,"+",ypos+10,sep="")) # shift
    
    tkwm.transient(dlg, parent) # set transient
    tkbind(parent,"<Destroy>",function(...) tkdestroy(dlg))
  }

      
  
  ## set up icon
  icon = match.arg(icon)
  allIcons = getStockIcons()
  iconFile = switch(icon,
    "warning"=allIcons$alert,
    "error" = allIcons$error,
    "question" = allIcons$help,
    allIcons$ok
    )
  imageID = paste("gdialogs",as.character(runif(1)),sep="")
  tcl("image","create","photo",imageID,file=iconFile)  
  icon = ttklabel(dlg,image=imageID)
  
  ## set up dlg window
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,title)


  
  tkgrid(icon,row=0,column=0)
  tkgrid(ttklabel(dlg,text=message,justify="left"),
         row=0,column=1,padx=5,pady=5, stick="w")

  
  ## entry widget for input
  if(type == "input") {
    textEntryVarTcl <- tclVar(text)
    textEntryWidget <-
      ttkentry(dlg,
              width=max(25,as.integer(1.3*nchar(text))),
              textvariable=textEntryVarTcl)
    tkgrid(textEntryWidget,column=1,stick="nw", padx=5,pady=5)
  }
  
  ## what to return? TRUE or FALSE or string for ginput
  ReturnVal <- FALSE
  
  
  onOK <- function() {
    if(type == "input") 
      ReturnVal <<- tclvalue(textEntryVarTcl)
    else
      ReturnVal <<- TRUE
    
    ## call handler if asked
    if(!is.null(handler)) 
      handler(list(obj=NULL, action=action, input=ReturnVal))
    
    tkgrab.release(dlg)
    tkdestroy(dlg)
  }
  onCancel <- function(){
    if(type == "input")
      ReturnVal <<- NA
    else
      ReturnVal <<- FALSE
    tkgrab.release(dlg)
    tkdestroy(dlg)
  }
  
  gp <- ttkframe(dlg)
  OK.but     <-ttkbutton(gp,text="   OK   ",command=onOK)
  Cancel.but <-ttkbutton(gp,text=" Cancel ",command=onCancel)
  
  tkgrid(gp, column=1,padx=5,pady=5)
  tkpack(OK.but,side="left")
  if(type == "confirm" || type == "input")
    tkpack(Cancel.but,side="left")
  
  
  tkfocus(dlg)
  tkbind(dlg, "<Destroy>", function() {
    tkgrab.release(dlg)
  })
  if(type == "input")
    tkbind(textEntryWidget, "<Return>", onOK)

  tkwait.window(dlg)

  invisible(ReturnVal)
}




setMethod(".gmessage",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   message,
                   title = "message",
                   icon = c("info","warning","error","question"),
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {

            return(tcltkDialog(
                               message,
                               title=title,
                               icon=icon,
                               type="message",
                               parent = parent,
                               handler=handler,
                               action=action,
                               ...))

##             icon = match.arg(icon)
            
##             ret = tkmessageBox(
##               message=message,
##               title=title,
##               icon=icon)
##             if(as.character(ret) == "ok")
##               TRUE
##             else
##               FALSE
          })
  
## if OK then run handler, else not
setMethod(".gconfirm",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   message,
                   title = "Confirm",
                   icon = c("info", "warning", "error", "question"),
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {
            
            return(tcltkDialog(
                               message,
                               title=title,
                               icon=icon,
                               type="confirm",
                               parent = parent,
                               handler=handler,
                               action=action,
                               ...))

##             icon = match.arg(icon)

##             ret = tkmessageBox(
##               message=message, 
##               title=title,
##               icon=icon,
##               type="yesnocancel"
##               )

##             val = switch(as.character(ret),
##               "yes"=1,
##               "no"=0,
##               "cancel"=-1)

##             if(!is.null(handler)) {
##               h = list()
##               h$obj=NULL; h$action=action
##               handler(h)
##             }
              
            
##             return(val)

          })

 
## Add input to the above
## h,... in handler has componets action, input (for value)
setMethod(".ginput",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   message,
                   text = "",
                   title = "Input",
                   icon = c("info","warning","error","question"),
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {

            return(tcltkDialog(
                               message,
                               text = text,
                               title=title,
                               icon=icon,
                               type="input",
                               parent = parent,
                               handler=handler,
                               action=action,
                               ...))

          })

## add a widget to the dialog. This is modal
setMethod(".gbasicdialog",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   title = "Dialog",
                   widget,
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {

            cat(gettext("gbasiddialog isn't implemented in tcltk"),"\n")
            return()

            
            icon = match.arg(icon)

            w = gwindow(title=title)
            tt <- getBlock(w)
            
            g = ggroup(horizontal=FALSE,container=w)
            add(g,widget)

            buttonGroup = ggroup(cont=g)
            ans <- 0
            OKbutton = gbutton("OK",cont=buttonGroup,handler=function(h,...) {
              ans <<- 1
              tkgrab.release(tt)
            })
            Cancelbutton = gbutton("OK",cont=buttonGroup,handler=function(h,...) {
              ans <<- 0
              tkgrab.release(tt)
            })

            ## make modal
            tkgrab.set(tt)

            if(ans == 1) {
              ## yes
              if(!is.null(handler)) {
                handler(list(ref=widget,widget=widget,action=action, ...))
              }
              return(TRUE)
            } else {
              ## no
              return(FALSE)
            }

              
              tkdestroy(tt)
              
            return(ans)
          })

