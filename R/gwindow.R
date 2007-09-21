## constructor
setMethod(".gwindow",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   title="Window", visible=TRUE,
                   width = NULL, height = NULL, location=NULL,
                   handler=NULL, action = NULL,
                   ...
                   ) {

            force(toolkit)
            
            win <- tktoplevel()
            tktitle(win) <- title
            ## enable autoresizing
            tkwm.geometry(win,"")
            ## set default size? only minsize here
            if(!is.null(width)) {
              if(is.null(height)) height = .7*width
              tkwm.minsize(win, width, height)
            }

            ## how to set location???
            if(!is.null(location)) cat("location argument not yet implemented\n")
            
            obj = new("gWindowtcltk",block=win, widget=win, toolkit=toolkit, ID=getNewID())
            
            if (!is.null(handler)) {
              id <- addhandlerdestroy(obj, handler=handler, action=action)
            }

            if(visible)
              visible(obj) <- visible

            return(obj)
          })
##################################################
## Methods 

setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowtcltk", value="gWidgettcltk"),
          function(obj, toolkit, value, ...) {
            ## adding widget to window means pack
            theArgs = list(...)
            packArgs = list(getBlock(value))
            if(!is.null(theArgs$expand) && theArgs$expand) {
             packArgs$expand=TRUE
              packArgs$fill = "both"
              packArgs$side="top"
            } else {
              packArgs$side="top"
            }
            if(!is.null(theArgs$anchor)) {
              an = theArgs$anchor
              if(an[1] == 1)
                packArgs$side = "right"
              else if(an[1] == -1)
                packArgs$side = "left"
              else if(an[2] == 1)
                packArgs$side = "top"
              else
                packArgs$side = "bottom"
            }

            do.call("tkpack", packArgs)
          })


## methods

## svalue refers to title
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowtcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ..) {
            ## return title
            cat("return the title")
            tktitle(obj@widget)
          })

setMethod(".svalue<-",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowtcltk"),
          function(obj, toolkit, index=NULL,..., value) {
            ## set the title
            tktitle(obj@widget) <- title
            return(obj)
          })

## no visible() method
setMethod(".visible<-",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowtcltk"),
          function(obj, toolkit, ..., value) {
            value = as.logical(value)
#            cat("gwindow: implement visible<-\n")
            return(obj)
          })


setMethod(".size",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowtcltk"),
          function(obj, toolkit, ...) {
            missingMsg(".size,gwindow")
            return()
          })

setReplaceMethod(".size",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowtcltk"),
          function(obj, toolkit, ...,value) {
            tkwm.minsize(obj@widget, value[1], value[2])
            return(obj)
          })

setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowtcltk"),
          function(obj, toolkit, ...) {
            tcl("after",5,function() {
              tkdestroy(getBlock(obj))
            })
          })


##################################################
## handlers
