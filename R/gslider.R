## FIX up for non-integer values

setClass("gSlidertcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

setMethod(".gslider",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   from=0, to=100, by = 1,
                   value=from,
                   horizontal=TRUE,
                   handler=NULL, action=NULL,
                   container=NULL, ...) {
            force(toolkit)

            ## JSlider is integer only (as far as I can tell
            ## if by < 1, call gspinbutton
            if(by < .99) {
              cat("gslider in tcltk is integer only, using gspinbutton instead\n")
              obj = gspinbutton(
                from, to, by, value, digits = 1,
                handler, action,container,..., toolkit)
              return(obj@widget)        # return gWidgettcltk; not gWidget
            }


            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }

            
            if(horizontal)
              orientation = "horizontal"
            else
              orientation = "vertical"

            tt = getBlock(container)
            gp = tkframe(tt)
            SliderValue <- tclVar(as.character(value))
            slider <- tkscale(gp, from=from, to=to,
                              showvalue=TRUE, variable=SliderValue,
                              resolution=by, orient=orientation)
            tkgrid(slider)

            
            obj = new("gSlidertcltk",block=gp, widget=slider,
              toolkit=toolkit, ID=getNewID())
            tag(obj,"tclVar") <- SliderValue
            
            add(container, obj,...)
            
            if (!is.null(handler))  {
              id = addhandlerchanged(obj, handler, action)
            }
            
            invisible(obj)
          })


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gSlidertcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            rbValue = tag(obj,"tclVar")
            return(as.numeric(tclvalue(rbValue)))
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gSlidertcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   tclvalue(tag(obj,"tclVar")) <- as.character(value)
                   return(obj)
                 })


### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gSlidertcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandler(obj,toolkit, signal="<Button-1>",handler,action)
          })
