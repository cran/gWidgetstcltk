##################################################
## add a separator to a container. Needs the container

##  inspired by
##  http://search.cpan.org/src/WGDAVIS/Tk-Separator-0.50/Separator.pm

setClass("gSeparatortcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

## should this return object?
setMethod(".gseparator",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   horizontal = TRUE, container = NULL, ...) {

            force(toolkit)

            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }

            
            theArgs = list(...)
            if(!is.null(theArgs$col))
              col = theArgs$col
            else
              col = "black"

            tt <- getBlock(container)
            gp <- ttkframe(tt)

            if(horizontal)
              orient <- "horizontal"
            else
              orient <- "vertical"
            sep <- ttkseparator(gp, orient=orient)

            if(horizontal)
              tkpack(sep, expand=TRUE, fill="x")
            else
              tkpack(sep, expand=TRUE, fill="y")
            
            obj = new("gSeparatortcltk", block=gp, widget=sep,
              toolkit=toolkit, ID=getNewID(), e = new.env())


            ## add gp to container. Fixe expand argument to be TRUE
            theArgs$expand = TRUE
##            do.call(add, c(list(obj=container, value=obj), theArgs))
            add(container, obj, ...)


            invisible(obj)
            
          })


## setMethod(".add",
##           signature(toolkit="guiWidgetsToolkittcltk", obj="gLayouttcltk",
##                     value="gSeparatortcltk"),
##           function(obj, toolkit, value, ...) {
##           })



