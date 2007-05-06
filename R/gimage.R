## I should make an abstract class for gButton, gImage and gLabel
## instead I get lots of repeated code.


setClass("gImagetcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

## image use 


setMethod(".gimage",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   filename = "", dirname="",
                   size="",
                   handler=NULL, action=NULL, 
                   container=NULL, ...) {

            force(toolkit)

            ## container in tcltk
            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }


            
            if (size != "") cat("gimage: size is currently ignored\n")

            
            ## get filename
            iconFile = NULL
            if(dirname == "stock") {
              gWidgetstcltkIcons = getStockIcons()
              iconFile = gWidgetstcltkIcons[[filename]]
            } else if(dirname != "") {
              iconFile = paste(dirname,filename,sep=.Platform$file.sep)
            } else {
              iconFile = filename
            }

            tt <- getBlock(container)
            gp <- tkframe(tt)

            imageID = paste("gimage",gp$ID,sep="")

            ## base tk support gif, ppm and bitmap (ppm doesn't seem to though)
            if(!is.null(iconFile) && file.exists(iconFile)) {
              x = try(tcl("image","create","photo",imageID,file=iconFile),silent=TRUE)
              ## now try as bitmap
              if(inherits(x,"try-error")) {
                x = try(tcl("image","create","bitmap",imageID,file=iconFile),silent=TRUE)
              }
              if(inherits(x,"try-error")) {
                cat("gimage had issues. Only gif, ppm and xbm files  in gWidgetstcltk\n")
                lab <- tklabel(gp,text="")
              } else {
                lab <- tklabel(gp, image=imageID)
              }
            } else {
              ##  uninitialized
              lab <- tklabel(gp,text="")
            }
            tkpack(lab, expand=TRUE, fill="both")
            
            obj = new("gImagetcltk", block=gp, widget=lab,
              toolkit=toolkit,ID=getNewID()
              )

            tag(obj,"filename") <- iconFile
            tag(obj,"imageID") <- imageID
            
            if(!is.null(handler)) {
              id = addhandlerclicked(obj, handler=handler, action=action)
            }

            ## attach
            add(container, obj,...)
            
            invisible(obj)
          })
          
          
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gImagetcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            ## return name?
            return(tag(obj,"filename"))
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gImagetcltk"),
                 function(obj, toolkit, index=NULL,  ..., value) {
                   ## value is a full filename or icon name
                   gWidgetstcltkIcons = getStockIcons()
                   
                   if(!file.exists(value)) {
                     ## if not there, look for stock
                     if(!is.null(gWidgetstcltkIcons[[value]])) {
                       value = gWidgetstcltkIcons[[value]]
                     } else {
                       cat("File",value,"does not exist\n")
                       return(obj)
                     }
                   }

                   x = try(tcl("image","create","photo",tag(obj,"imageID"),file=value), silent=TRUE)
                   if(inherits(x,"try-error")) {
                     cat("Only gif and pnm files are possible in gWidgetstcltk\n")
                   } else {
                     tkconfigure(getWidget(obj),image=tag(obj,"imageID"))
                   }
                   
                   ## store dynamically, not with @filename
                   tag(obj,"filename") <- value
                   
                   return(obj)
                 })


## set size
setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gImagetcltk"),
                 function(obj, toolkit, ..., value) {
                   ## pixels for tkframe etc
                   tkconfigure(getWidget(obj), width=value[1], height=value[2])
                   return(obj)
                 })


setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gImagetcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerclicked(obj, toolkit, handler, action, ...)
          })
