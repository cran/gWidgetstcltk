setClass("gPanedgrouptcltk",
         contains="gContainertcltk",
         prototype=prototype(new("gContainertcltk"))
         )

## TODO: method obj[1 or 2 ] <- replacewidget
setMethod(".gpanedgroup",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   widget1, widget2, horizontal=TRUE, container=NULL, ...) {
            ## add a paned group

            force(toolkit)
            
            
            if(is.null(container)) {
              cat("No NULL containers in tcltk. Creating a new window\n")
              container=gwindow()
            } else if(is.logical(container) && container) {
              container = gwindow()
            }

            if(!is(container,"guiWidget")) {
              container = gwindow()
            }

            ## process args
            if(horizontal)
              orient = "horizontal"
            else
              orient = "vertical"

            
            tt <- getBlock(container)
            pg <- tkwidget(tt,"panedwindow", orient=orient)
            tkpack(pg, expand=TRUE, fill="both")

            
            ## make object -- not block is pg so that add works correctly
            ## as it calls getBlock(container)
            obj = new("gPanedgrouptcltk", block=pg, widget=pg,
              toolkit=toolkit,ID=getNewID())

            tag(obj,"horizontal") <- horizontal

            if(!missing(widget1) || !is.null(widget1))
              cat("Use gpanedgroup as a container to add widgets")
            if(!missing(widget2) || !is.null(widget2))
              cat("Use gpanedgroup as a container to add widgets")

            
            return(obj)
          })


## add -- use this rather than at construction time
setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gPanedgrouptcltk", value="gWidgettcltk"),
          function(obj, toolkit, value, ...) {

            theArgs = list(...)
            argList = list(getWidget(obj),"add",getBlock(value))

            ## args to position
            sticky = "n"
            if(!is.null(theArgs$anchor)) {
              sticky = xyToAnchor(theArgs$anchor)
            }
            if(!is.null(theArgs$expand) && theArgs$expand) {
              if(tag(obj,"horizontal"))
                sticky = "news"
              else
                sticky = "news"
            }
            argList$sticky = sticky


            do.call("tcl", argList) ## tcl(tt,"add",widget,...)
            
          })

## delete means we can readd -- in this case we actually dispose, as
## the widget doesn't get added back?
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gPanedgrouptcltk",
                    widget="gWidgettcltk"),
          function(obj, toolkit, widget, ...) {
            ## call forget

            tcl(getWidget(obj),"forget",getBlock(widget))
          })
