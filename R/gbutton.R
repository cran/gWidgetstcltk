setClass("gButtontcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )


setMethod(".gbutton",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   text="", border = TRUE, handler=NULL, action=NULL, container=NULL,...
                   ) {

            force(toolkit)
            
            theArgs = list(...)
            ## look like label if border=FALSE
            if(border == FALSE) {
              return(glabel(text,handler,action,container,...))
            }
            ## compound is tcltk speak for where to put icon. One of
            ## top, left, right or bottom
            ## http://search.cpan.org/~ni-s/Tk-804.027/pod/Button.pod
            if(is.null(theArgs$compound))
              compound = "left"
            else
              compound = theArgs$compound

            
            
            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }

            tt = getWidget(container)
            gp = ttkframe(tt)
            
            gWidgetstcltkIcons = getStockIcons()
            
            if(text %in% names(gWidgetstcltkIcons))
              iconFile = gWidgetstcltkIcons[[text]]
            else 
              iconFile = NULL
            if(!is.null(iconFile)) {
              ## put icon and text
              imageID = paste("gimage",gp$ID,sep="")
              x = try(tcl("image","create","photo",imageID,file=iconFile),
                silent=TRUE)
              if(inherits(x,"try-error")) {
                cat(gettext("gimage had issues. Only gif and pnm in gWidgetstcltk\n"))
                button = ttkbutton(gp, text=text)
              } else {
                button = ttkbutton(gp, text=text, image=imageID,
                  compound=compound)
              }
            } else {
              button = ttkbutton(gp, text=text)
            }
            ## pack into gp.
            if(!is.null(theArgs$expand) && theArgs$expand)
              tkpack(button, expand=TRUE, fill="both")
            else
              tkpack(button)
            
            
            obj = new("gButtontcltk",
              block=gp, widget=button, toolkit=toolkit,ID=getNewID(),
              e = new.env())

            ## add gp to container
            add(container, obj, ...)

            
            ## add handler
            if (!is.null(handler)) {
              id = addhandlerchanged(obj,handler,action)
            }
            
            invisible(obj)
          })


## handle gaction
## constructor for action=gaction_instance
setMethod(".gbutton",signature(action="guiComponent", toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   text="", border = TRUE, handler=NULL, action=NULL, container=NULL,...
                   ) {
            .gbutton(toolkit,
                     text = text, border=border, handler = handler,
                     action = action@widget,
                     container = container, ...)
          })

## constructor for action=gaction_instance
setMethod(".gbutton",signature(action="gActiontcltk", toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   text="", border = TRUE, handler=NULL, action=NULL, container=NULL,...
                   ) {

            alst <- action@widget
            obj <- .gbutton(toolkit,
                     text = alst$label,
                     border = border,
                     handler = alst$handler,
                     action = alst$action,
                     container = container, ...)

            if(!is.null(alst$tooltip))
              .tooltip(obj,toolkit) <- alst$tooltip
            
            action@e$buttons <- c(action@e$buttons,obj)
            return(obj)
          })


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gButtontcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            val = paste(as.character(tkcget(getWidget(obj),"-text")),
              sep=" ",collapse=" ")
            return(val)
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gButtontcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   text = value
                   gWidgetstcltkIcons = getStockIcons()                   
                   iconFile = gWidgetstcltkIcons[[text]]
                   if(!is.null(iconFile) && iconFile != "" ) {
                     ## put icon and text
                     imageID = paste("gimage",obj@ID,sep="")
                     x = try(tcl("image","create","photo",imageID,file=iconFile),
                       silent=TRUE)
                     if(inherits(x,"try-error")) {
                       cat(gettext("gimage had issues. Only gif and pnm in gWidgetstcltk\n"))
                       button = tkconfigure(obj@widget,
                         text=as.character(text))
                     } else {
                       button = tkconfigure(obj@widget,
                         text=text, image=imageID)
                     }
                   } else {
                     tkconfigure(obj@widget, text=as.character(value))
                   }
                   return(obj)
                 })

## size has no height
setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gButtontcltk"),
                 function(obj, toolkit, ..., value) {
                   width <- ceiling(value[1]/widthOfChar)
                   tkconfigure(getWidget(obj), width=width)
                   return(obj)
                 })

### handlers
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gButtontcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            ID = .addHandler(obj,toolkit,"<Button-1>", handler, action)
            return(ID)
          })
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gButtontcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandlerclicked(obj, handler, action)
          })

