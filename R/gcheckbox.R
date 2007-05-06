setClass("gCheckboxtcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

## constructor
setMethod(".gcheckbox",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   text, checked=FALSE,
                   handler=NULL, action=NULL,
                   container=NULL,...) {
            
            force(toolkit)
            
            if(missing(text)) text = ""

            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }
            
            tt = container@widget@block
            gp = tkframe(tt)

            ## widget
            check = tkcheckbutton(gp)
            theLabel = tklabel(gp, text=text)
            ## configure
            tclVar = tclVar(as.numeric(checked))
            tkconfigure(check,variable=tclVar)
            ## layout
            tkgrid(check, theLabel)
            tkgrid.configure(check,stick="e")
            tkgrid.configure(theLabel,stick="w")
            
            obj = new("gCheckboxtcltk",block=gp, widget=check,
              toolkit=toolkit, ID=getNewID())

            tag(obj,"check") <- check
            tag(obj,"tclVar") <- tclVar
            tag(obj,"label") <- theLabel
            
            ## add to container
            add(container, obj,...)
            
            if (!is.null(handler)) {
              id = addhandlerchanged(obj, handler, action=action)
            }
  
            invisible(obj)
          })

### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxtcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            cbVal <- as.logical(as.numeric(tclvalue(tag(obj,"tclVar"))))
            return(cbVal)
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxtcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   tclvalue(tag(obj,"tclVar")) <-
                     as.character(as.numeric(value))
                   return(obj)
                 })

## [
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gCheckboxtcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            cat("How to get text from a tklabel?")
            return("")
          })
            
setMethod("[",
          signature(x="gCheckboxtcltk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gCheckboxtcltk"),
          function(x, toolkit, i, j, ..., value) {
            label = tag(x,"label")
            tkconfigure(label, text=as.character(value[1]))
            return(x)
          })

setReplaceMethod("[",
                 signature(x="gCheckboxtcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

### no method to change the value of text???

### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxtcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandler(tag(obj,"check"),toolkit, signal="<Button-1>",handler=handler, action=action)
          })

setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxtcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandlerchanged(obj, handler, action)
          })
