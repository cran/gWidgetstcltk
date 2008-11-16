## constructor
setMethod(".gwindow",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   title="Window", visible=TRUE,
                   width = NULL, height = NULL, parent=NULL,
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
            location <- parent          # renamed
            if(!is.null(location)) {
              if(is(location,"guiWidget") ||
                 is(location, "gWindowtcltk") ||
                 is(location, "tkwin")) {
                location <- getWidget(location)
                curgeo <- tclvalue(tkwm.geometry(location))
                ## widthXheight+xpos+ypos
                pos <- unlist(strsplit(curgeo, "\\+"))
                sz <- unlist(strsplit(pos[1],"x"))
                xpos = as.numeric(pos[2]); ypos=as.numeric(pos[3])
                tkwm.geometry(win,paste("+",xpos+10,"+",ypos+10,sep="")) # shift

                tkwm.transient(win, location) # set transient
                tkbind(location,"<Destroy>",function(...) tkdestroy(win))
              } else if(is.numeric(location) && length(location) == 2) {
                tkwm.geometry(win, location[1], location[2])
              }

            }
            
            obj = new("gWindowtcltk",block=win, widget=win, toolkit=toolkit,
              ID=getNewID(),e=new.env())


            
            
            if (!is.null(handler)) {
              id <- addhandlerdestroy(obj, handler=handler, action=action)
            }

            if(visible)
              visible(obj) <- visible

            return(obj)
          })
##################################################
## Methods 



## general add
setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowtcltk", value="gWidgettcltk"),
          function(obj, toolkit, value, ...) {

            ## where to put
            if(!is.null(tag(obj,"toolbar")))
              tkpack(getBlock(value),
                     after=tag(obj,"toolbar"),
                     expand=TRUE, fill="both")
            else if(!is.null(tag(obj,"statusbar")))
              tkpack(getBlock(value),
                     before=tag(obj,"statusbar"),
                     expand=TRUE, fill="both")
            else
              tkpack(getBlock(value),
                     expand=TRUE, fill="both")
            return(TRUE)
            
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
            ## override with anchor argument
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

            
            #do.call("tkpack", packArgs)
            packArgs$side <- NULL       # clera out for test
            do.call("tkgrid", packArgs)
          })


## add toolbar, menubar, statusbar
## menubar -- in gmenu

## toolbar
setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowtcltk", value="gToolbartcltk"),
          function(obj, toolkit, value, ...) {
            ## put before all others.
            ## get children, check then put in. XXX
            ## XXX  -- not working
            g <- getWidget(obj)
            slaves <- unlist(strsplit(tclvalue(tkpack("slaves",g))," "))
            args <- list(getBlock(value),
                         side="top",anchor="w",expand=FALSE, fill="x")
            if(length(slaves))
              args$before = slaves[1]
            do.call("tkpack",args)

            tag(obj,"toolbar") <- getBlock(value)
          })
## statusbar
setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowtcltk", value="gStatusbartcltk"),
          function(obj, toolkit, value, ...) {
            ## put after all others
            ## XXX Get children, put last -- NOT WORKING!!
            g = getWidget(obj)
            slaves = unlist(strsplit(tclvalue(tkpack("slaves",g))," "))
            args <- list(getBlock(value),
                         side="top",anchor="w",expand=FALSE, fill="x")
            if(length(slaves))
              args$after <- slaves[length(slaves)]
            do.call("tkpack",args)

            tag(obj,"statusbar") <- getBlock(value)
            
          })


## methods

## svalue refers to title
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowtcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ..) {
            ## return title
            val <- tcl("wm","title",getWidget(obj))
            tclvalue(val)
          })

setMethod(".svalue<-",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowtcltk"),
          function(obj, toolkit, index=NULL,..., value) {
            ## set the title
            tcl("wm","title",getWidget(obj), as.character(value))
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


setMethod(".addhandlerunrealize",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowtcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            win <- getWidget(obj)
            h <- list(obj = obj, action=action,...)
            tkwm.protocol(win, "WM_DELETE_WINDOW",
                          function(...) {
                            val <- handler(h,...)
                            ## FALSE -- destroy, TRUE -- keep
                            if(!as.logical(val)) tkdestroy(win) ## revers
                          })
          })


setMethod(".addhandlerdestroy",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowtcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="<Destroy>", handler, action, ...)
          })
