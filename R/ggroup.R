## class in aaaClasses.R
## constructor
setMethod(".ggroup",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   horizontal = TRUE, spacing = 5,
                   use.scrollwindow = FALSE, 
                   container = NULL, ... 
                   ) {

            force(toolkit)
            
            theArgs = list(...)                   # raise.on.dragmotion
            
            if(is.null(spacing))
              spacing = 0


            if(is.null(container)) {
              cat(gettext("No NULL containers in tcltk. Creating a new window\n"))
              container=gwindow()
            } else if(is.logical(container) && container) {
              container = gwindow()
            }

            if(!is(container,"guiWidget")) {
              container = gwindow()
            }

            tt <- getBlock(container)

            ## implement scrollbars if asked. 
            if(use.scrollwindow == TRUE) {
              cat("use.scrollwindow not implemented in gWidgetstcltk\n") 
              gp = ttkframe(tt)
            } else {
              gp = ttkframe(tt)
            }
            

            obj = new("gGrouptcltk",block=gp, widget=gp, horizontal=horizontal,
              e = new.env())
            

            ## attach to container if there
            if(!is.null(container)) {
              add(container, obj,...)
            }

            ## raise if we drag across
            if(!is.null(theArgs$raise.on.dragmotion)) {
            }
            return(obj)
          })


##################################################
## methods

setReplaceMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gGrouptcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ..., value) {
            ## adds some breathing room to object
            ## value is pixels
            gp <- getWidget(obj)
#            tkcofigure(gp,padx=value,pady=value)
            tkconfigure(gp,padding = value)            

            return(obj)
          })

##################################################
## handlers
