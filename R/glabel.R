setClass("gLabeltcltk",
         contains="gComponenttcltk",
         representation = representation("gComponenttcltk",
           markup="logical"),

         prototype=prototype(new("gComponenttcltk"))
         )

## constructor
setMethod(".glabel",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   text= "", markup = FALSE, editable = FALSE, handler = NULL, 
                   action = NULL, container = NULL, 
                   ...
                   ) {

            force(toolkit)

            if(markup) {
              gwCat(gettext("In gWidgetstcltk there is no markup language. Use font()<- instead.\n"))
            }

            
            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }

            tt = getBlock(container)
            gp = ttkframe(tt)

            label = ttklabel(gp, text=text)
            tkpack(label,expand=TRUE, fill="both")
                   
            obj = new("gLabeltcltk",
              block=gp, widget=label, markup=markup,
              toolkit=toolkit,ID=getNewID(), e = new.env())

            ## add to container
            add(container, obj, ...)

            
            if(editable) {
              handler = function(h,...) {
                val = ginput(message="Change label value:",text=svalue(h$obj),
                  title="Change text for label", icon="question")
                if(!is.na(val))
                  svalue(obj) <- val
              }
            }
            
            if(!is.null(handler)) {
              id = addhandlerclicked(obj, handler=handler,action=action)
            }
            
            invisible(obj)
          })

### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gLabeltcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            val = as.character(tkcget(getWidget(obj),"-text"))
            if(length(val) == 0) val=""
            val = paste(val, collapse=" ")
            return(val)
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gLabeltcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   tkconfigure(obj@widget, text=as.character(value))
                   return(obj)
                 })


setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gLabeltcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerclicked(obj, toolkit, handler, action, ...)
          })



##################################################
## internal function -- used by gvariables in  gcommandline
setGeneric("gaddlabel", function(obj, text="", markup=FALSE, pos=1, container=NULL, ...) standardGeneric("gaddlabel"))

setMethod("gaddlabel",
          signature("guiWidget"),
          function(obj, text="", markup=FALSE, pos=1, container=NULL, ...)
          gaddlabel(obj@widget, text, markup, pos, container, ...)
        )

setMethod("gaddlabel",
          signature("gWidgettcltk"),
          function(obj, text="", markup=FALSE, pos=1, container=NULL, ...) {
            ## wrap widget into a new package with label
            if(pos %in% c(2,4)) {
              group = ggroup(horizontal=TRUE,container=container,
                toolkit=obj@toolkit)
            } else {
              group = ggroup(horizontal=FALSE,container=container,
                toolkit=obj@toolkit)
            }
            
            
            if(pos %in% 2:3) {
              glabel(text, markup=markup, container=group, toolkit=obj@toolkit)
              add(group, obj,expand=TRUE)
            } else {
              add(group, obj,expand=TRUE)
              glabel(text, markup=markup, container=group, toolkit=obj@toolkit)
            }
            ## group is returned. No methods added here, just a new package
            return(group)
          })
          
