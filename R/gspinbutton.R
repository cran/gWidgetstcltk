## Could make spinbutton slider, subclass as methods are identical
setClass("gSpinbuttontcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

setMethod(".gspinbutton",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   from=0,to=10,by=1,value=from,digits=0,
                   handler=NULL, action=NULL,
                   container=NULL, ...) {

            force(toolkit)

            ## no spinbutton in the tcltk
            vals =  as.character(seq(from,to,by=by))
            
            
            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }
            

            tt = getBlock(container)
            gp = ttkframe(tt)
            
            sb = tkwidget(gp, "spinbox", from=from, to=to, increment=by)
            tcl(sb,"set",value)
            tkpack(sb, expand=TRUE, fill="both")
            
            obj = new("gSpinbuttontcltk",block=gp, widget=sb,
              toolkit=toolkit, ID=getNewID(), e = new.env())
            
            add(container, obj,...)
            
            if (!is.null(handler))  {
              id = addhandlerchanged(obj, handler, action)
            }
            
            invisible(obj)
          })

### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gSpinbuttontcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            sb = getWidget(obj)
            val = as.numeric(tcl(sb,"get"))
            return(val)
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gSpinbuttontcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   sb = getWidget(obj)
                   tcl(sb,"set",value)
                   return(obj)
                 })

### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gSpinbuttontcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerclicked(obj, toolkit, handler, action,...)
            .addHandler(obj, toolkit, signal="<Return>",
                        handler=handler, action=action, ...)
          })
