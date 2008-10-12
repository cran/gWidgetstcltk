## TODO
## * FONTS

setClass("gTexttcltk",
         representation(tags="list"),
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

setMethod(".gtext",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   text=NULL,
                   width=NULL, height=300,
                   font.attr = NULL, wrap = TRUE,
                   handler = NULL, action=NULL,
                   container=NULL, ...) {


            force(toolkit)


            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }

            tt = getBlock(container)
            gp = ttkframe(tt)

            if(wrap) wrap="word" else wrap="none"
            
            xscr <- ttkscrollbar(gp, orient="horizontal",
                                 command=function(...)tkxview(txt,...))
            yscr <- ttkscrollbar(gp, 
                                 command=function(...)tkyview(txt,...))
            
            txt <- tktext(gp,
                          bg="white", setgrid=FALSE, #font="courier",
                          undo = TRUE,                  # undo support
                          xscrollcommand=function(...)tkset(xscr,...),
                          yscrollcommand=function(...)tkset(yscr,...),
                          wrap=wrap)
            
            ## pack into a grid
            ## see tkFAQ 10.1 -- makes for automatic resizing
            tkgrid(txt,row=0,column=0, sticky="news")
            tkgrid(yscr,row=0,column=1, sticky="ns")
            tkgrid(xscr, row=1, column=0, sticky="ew")
            tkgrid.columnconfigure(gp, 0, weight=1)
            tkgrid.rowconfigure(gp, 0, weight=1)

            ## set point
            tkmark.set(txt,"insert","0.0")
            
            obj = new("gTexttcltk", block=gp, widget=txt, tags=list(),
              toolkit=toolkit,ID=getNewID(), e = new.env())


            ## add initial text
            if(!is.null(text)) {
              add(obj, text, font.attr=font.attr)
            }

            ## set height if requested
            if(!is.null(width)) 
              size(obj) <- c(width,height)
            
##            adddropsource(obj)
            adddroptarget(obj)

            
            ## attach to container
              add(container, obj,...)

            ## add handler
            if (!is.null(handler)) {
              id = addhandlerkeystrok(obj, handler, action)
            }
            return(obj)
          })
## drop=TRUE to get only mouse selected text
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTexttcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {

            ## rongui request, if INDEX = TRUE return selected text
            ## by index in the buffer
            if(!is.null(index) && index == TRUE) {
              ## get the selected text from gtext,
              ## return the index instead of text.
              if(tclvalue(tktag.ranges(getWidget(obj),"sel")) != ""){
                val = strsplit(tclvalue(tktag.ranges(getWidget(obj),
                  "sel")), " ")[[1]]}
              else
                val =c(0,0)
              return(as.numeric(val))
            }

            ## otherwise we return text
            ## if drop=FALSE or NULL grab all text
            ## if drop=TRUE, get selected text only
            if(is.null(drop) || drop == FALSE) {
              val = tclvalue(tkget(getWidget(obj),"0.0","end"))
              ## strip off last "\n"'s
              val <- gsub("\n*$","",val)
            } else {
              range <- as.numeric(tktag.ranges(getWidget(obj),"sel"))
              ## range is numeric(0) if none
              if(length(range) > 0)
                val = tclvalue(tkget(getWidget(obj),"sel.first","sel.last"))
              else
                val = ""
            }
                                        ## val = unlist(strsplit(val,"\n"))
            return(val)
          })

##  svalue<-() replaces text
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gTexttcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   ## how to clear out old text
                   tkdelete(getWidget(obj),"0.0","end")

                   if(length(value) > 1)
                     value = paste(value, collapse="\n")
                   tkinsert(getWidget(obj),"end",value)
                            
                   return(obj)
                 })


## clear all text in buffer
setMethod("dispose",signature(obj="gTexttcltk"),
          function(obj,...)  {
            .dispose(obj, obj@toolkit, ...)
          })
setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTexttcltk"),
          function(obj, toolkit,  ...) {
            svalue(obj) <- ""
          })


### Add method is a workhorse for this class. Value can be
## * a line of text
## * a vector of lines of text
## need to do where value of "point"
## add, as a method, needs to have a consistent signature. I'

## add text
setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTexttcltk",value="character"),
          function(obj, toolkit, value,  ...) {
            theArgs = list(...)                      # look for font.attr, do.newline, where

            do.newline = ifelse(is.null(theArgs$do.newline), TRUE, as.logical(theArgs$do.newline))


            
            where = ifelse(is.null(theArgs$where), "end",theArgs$where)
            if(where != "end") where = "0.0"
            
            value = paste(value,collapse="\n")
            if(do.newline)
              value = paste(value,"\n",sep="")

            ### Handle markup here
            markup = theArgs$font.attr
            if(!is.null(markup) && !is.list(markup))
              markup = lapply(markup,function(x) x)
            
            if(!is.null(markup)) {
              ## set up tag for handling markup
              argList = list(getWidget(obj),"foo")
              fontList = c()
              if(!is.null(markup$family))
                fontList = c(fontList,family=switch(markup$family,
                                        "normal"="times",
                                        "sans" = "helvetica",
                                        "serif" = "times",
                                        "monospace"="courier",
                                        markup$family))
              if(!is.null(markup$weight))
                fontList = c(fontList,slant=switch(markup$weight,
                                        "normal"="normal",
                                        "oblique"="normal",
                                        "italic"="italic",
                                        markup$weight))
              if(!is.null(markup$style))
                fontList = c(fontList, weight=switch(markup$style,
                                         "bold"="bold",
                                         "ultra-bold"="bold",
                                         "heavy"="bold",
                                         markup$style))
              
              if(!is.null(markup$size))
                if(is.numeric(markup$size))
                  fontList = c(fontListm, size=markup$size)
                else
                  fontList = c(fontList,size = switch(markup$size,
                                          "xxx-large"=24,
                                          "xx-large"=20,
                                          "x-large"=18,
                                          "large"=16,
                                          "medium"=12,
                                          "small"=10,
                                          "x-small"=8,
                                          as.integer(markup$size)))
              


              if(length(fontList) > 0)
                argList$font = fontList

              if(!is.null(markup$color))
                argList$foreground = markup$color

              ## now configure a tag
              do.call("tktag.configure",argList)

              tkinsert(getWidget(obj),where,value,"foo")

              ## does this place the cursor? TK FAQ 10.6
              tksee(getWidget(obj),"end")
              
            } else {
              ## no markup
              tkinsert(getWidget(obj),where,value)
            }
            
          })

## add a widget
setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTexttcltk",value="guiWidget"),
          function(obj, toolkit, value,  ...) {
            .add(obj,toolkit, value@widget, ...)
          })

setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTexttcltk",value="gWidgettcltk"),
          function(obj, toolkit, value,  ...) {

            cat("gtext: implement adding a widget to text area\n")
            return()
            })


## set the font for the selected area of the gtext object
setReplaceMethod(".font",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gTexttcltk"),
                 function(obj, toolkit, ..., value) {
                   ### XXX Implement this
                   gwCat("gtext: implement font()\n")
                   return(obj)
                 })



setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTexttcltk"),
          function(obj,toolkit, handler=NULL, action=NULL,...) {
            .addhandlerkeystroke(obj,toolkit,handler,action)
          })

