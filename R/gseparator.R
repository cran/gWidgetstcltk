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
            gp <- tkframe(tt)


            if(horizontal) {
              tkpack(tkframe(gp),  fill="both", expand=1, pady=2)
              tkpack(tkframe(gp, background=col), expand=1, fill="both", side="top", fill="x")
              tkpack(tkframe(gp),  fill="both", expand=1, pady=2)
            } else {
              tkpack(tkframe(gp),  fill="both", expand=1, padx=2)
              tkpack(tkframe(gp, background=col), expand=1, fill="both", side="left", fill="y")
              tkpack(tkframe(gp),  fill="both", expand=1, padx=2)
            }

            obj = new("gSeparatortcltk", block=gp, widget=gp, toolkit=toolkit, ID=getNewID())


            ## add gp to container. Fixe expand argument to be TRUE
            theArgs$expand = TRUE
            do.call(add, c(list(obj=container, value=obj), theArgs))
##            add(container, obj, ...)


            invisible(obj)
            
          })


setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk", obj="gLayouttcltk",
                    value="gSeparatortcltk"),
          function(obj, toolkit, value, ...) {
          })



