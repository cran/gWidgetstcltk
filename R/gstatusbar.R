## StatusBar. Use value to push message, value to pop
setClass("gStatusbartcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )
## constructor
setMethod(".gstatusbar",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   text="", container=NULL, ...) {

            group = ggroup(horizontal = TRUE, container=container,
              expand=TRUE, anchor=c(-1,0))
            statusbar = glabel(text, container=group, anchor=c(-1,0))

            force(toolkit)            
            
            obj = new("gStatusbartcltk",block=group, widget=statusbar, toolkit=toolkit, ID=getNewID())
            
            invisible(obj)
          })

### methods

## This gets from glabel instance
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gStatusbartcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            svalue(obj@widget)
          })

## This pushes to label
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gStatusbartcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   svalue(obj@widget) <- value
                   return(obj)
                 })

