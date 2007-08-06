## table for selecting values
## most methods in gdf.R inherited from gGrid class
setClass("gTabletcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )


## ## constructor for selecting values from a data set -- not meant for editing
setMethod(".gtable",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   items,
                   multiple = FALSE,
                   chosencol = 1,                        # for drag and drop, value
                   icon.FUN = NULL,
                   filter.column = NULL,
                   filter.labels = NULL,
                   filter.FUN = NULL,   # two args gtable instance, filter.labels element
                   handler = NULL,
                   action = NULL,
                   container = NULL,
                   ...) {

            ## NOT IMPLEMENTED
            ## * icon.FUN
            ## * sorting
            
            force(toolkit)

            ## do we filter? If so, send to filter function.
            ## this is a hack, but seems easy enough to implement
            if(!is.null(filter.column) || !is.null(filter.FUN)) {
              obj = .gtableWithFilter(toolkit,
                items,
                multiple,
                chosencol,                        # for drag and drop, value
                icon.FUN,
                filter.column,
                filter.labels,
                filter.FUN,   # two args gtable instance, filter.labels element
                handler,
                action,
                container,
                ...)
              return(obj)
            }

            ## Not filtering
            
            theArgs = list(...)

            isVector = TRUE
            if(is.vector(items)) {
              isVector = TRUE
            } else if(is.matrix(items) || is.data.frame(items)) {
              if(dim(items)[2] > 1) {
                isVector = FALSE
              } else {
                items = items[,1,drop=TRUE] # make a vector
              }
            } else {
              warning("items must be vector, matrix or data frame.")
              return()
            }

            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }

            selectmode = if(multiple) "extended" else "single"
            
            tt = getBlock(container)
            gp = tkframe(tt)

            if(is.null(theArgs$height))
              height = 10*max(min(5,length(items)),15) # in lines
            else
              height = theArgs$height   # pixels

            if(is.null(theArgs$width))
              width = min(25,max(c(10,sapply(as.character(items),nchar))))*6
            else
              width = theArgs$width     # pixels
            
            
            xscr <- tkscrollbar(gp, repeatinterval=5,orient="horizontal",
                                command=function(...)tkxview(tl,...))
            yscr <- tkscrollbar(gp, repeatinterval=5,
                               command=function(...)tkyview(tl,...))

            tl<-tklistbox(gp,
                          selectmode=selectmode,
                          xscrollcommand= function(...)tkset(xscr,...),
                          yscrollcommand= function(...)tkset(yscr,...),
                          background="white")


            tkgrid(tl,row=0,column=0, sticky="news")
            tkgrid(yscr,row=0,column=1, sticky="ns")
            tkgrid(xscr, row=1, column=0, sticky="ew")
            ## see tkFAQ 10.1 -- makes for automatic resizing
            tkgrid.rowconfigure(gp, 0, weight=1)
            tkgrid.columnconfigure(gp, 0, weight=1)


##             tkgrid(tl,yscr)
##             tkgrid.configure(yscr,row=0,column=1, sticky="nsw")
##             tkgrid(xscr, sticky="enw",row=1, column=0)
##             ## see tkFAQ 10.1 -- makes for automatic resizing
##             tkgrid.columnconfigure(gp, 0, weight=1)
##             tkgrid.rowconfigure(gp, 0, weight=1)
            ## set point


            if((is.matrix(items) || is.data.frame(items)) &&
               dim(items)[2] > 1
               ) tkconfigure(tl, font="courier") # fixed
            

            obj = new("gTabletcltk",block=gp,widget=tl,
              toolkit=toolkit,ID=getNewID())

            tag(obj,"isVector") <- isVector
            tag(obj,"chosencol") <- chosencol
            tag(obj,"color") = if(!is.null(theArgs$color))
              theArgs$color
            else
              "gray90"
            tag(obj,"colnamesColor") = if(!is.null(theArgs$colnamesColor))
              theArgs$colnamesColor
            else
              "red"

            size(obj) <- c(width, height)
            
            obj[] <- items

            tag(obj,"items") <- items
            
            ## no cell editing
            ## *implement me
            

            ## add handler
            if (!is.null(handler)) {
              id = addhandlerchanged(obj,handler,action)
            }

            
            ## add to container
            add(container, obj,...)

            return(obj)
            
          })


## incorporate chosenval here
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTabletcltk"),
          function(obj, toolkit, index=NULL, drop=NULL,...) {

            widget = getWidget(obj)

            indices = as.numeric(tkcurselection(widget)) + 1
            if(!tag(obj,"isVector"))
              indices = indices - 1
            
            if(!is.null(index) && index == TRUE)
              return(indices)
            
            ## Now a value
            if(missing(drop) || is.null(drop))
              drop = TRUE               # default is to drop unless asked not to

            return(obj[indices, drop=drop])
          })


setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gTabletcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   widget = getWidget(obj)
                   isVector = tag(obj,"isVector")
                   
                   if(!is.null(index) && index) {
                     tkselection.clear(widget, 0,"end")
                     tkselection.set(widget, value - isVector) # offset if vector
                   } else {
                     ## set value if present

                     ## need to update this for our hack to handle data frames
                     items = tag(obj,"items")

                     if(!isVector) {
                       m = match(value,items[,tag(obj,"chosencol"),drop=TRUE])
                     } else {
                       m = match(value,items)
                     }
                     
                     if(!is.na(m)) {    # NA is nomatch
                       tkselection.clear(widget, 0,"end")
                       tkselection.set(widget,
                                       m - 1 + as.numeric(!tag(obj,"isVector")))
                     } 
                   }
                   return(obj)
                 })


## retrieve values
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTabletcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            items = tag(x,"items")
            isVector = tag(x,"isVector")
            if(missing(i)) i = if(isVector) 1:length(items) else 1:nrow(items)
            if(isVector) {
              return(items[i])
            } else {
              if(missing(j)) j = 1:ncol(items)
              return(items[i,j, drop=drop])
            }
          })
            
setMethod("[",
          signature(x="gTabletcltk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop) 
          })
## replace values
setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTabletcltk"),
          function(x, toolkit, i, j, ..., value) {
            items = tag(x,"items")


            if(tag(x,"isVector")) {
              if(missing(i)) 
                items <- value
              else
                items[i] <- value
            } else {
              ## a matrix or data frame
              if(missing(i))
                items <- value
              else
                items[i,] <- value
            }
            ## save
            tag(x,"items") <- items
            if(!tag(x,"isVector")) {
              items = .prettyPrintTable(items)
            }
              
            ## clear out previous, then add
            widget = getWidget(x)
            tkdelete(widget, 0, "end")
            ## add them
            sapply(items, function(i) tkinsert(widget,"end",i))
            is.odd = function(x) x %%2 == 1
            n = (1:length(items))-1
            if(length(items) == 0)
              return(x)                 # all done
            ## otherwise, fix colors
            sapply(n,
                   function(i) {
                     if(is.odd(i))
                       tkitemconfigure(widget,i,background=tag(x,"color"))
                   })
            if(!tag(x,"isVector"))
              tkitemconfigure(widget,0,foreground=tag(x,"colnamesColor"))
            return(x)
            
          })

setReplaceMethod("[",
                 signature(x="gTabletcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

## dim
setMethod(".dim",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTabletcltk"),
          function(x, toolkit) {
            if(tag(x,"isVector"))
              length(x)
            else
              dim(tag(x,"items"))
          })
## length
setMethod(".length",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTabletcltk"),
          function(x, toolkit) {
            length(tag(x,"items"))
          })

## size<- work on tl
setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gTabletcltk"),
                 function(obj, toolkit, ..., value) {
                   if(is.numeric(value) && length(value) == 2)
                     tkconfigure(obj@widget,
                                 width=floor(value[1]/5), # convert pixels to chars
                                 height=floor(value[2]/10)) # convert pixels to lines
                   else
                     cat("size needs a numeric vector c(width,height)\n")
                   return(obj)
                 })


## handlers

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTabletcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandlerdoubleclick(obj, handler, action,...)
          })

## when a selection is changed
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTabletcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandler(obj,toolkit,signal="<<ListboxSelect>>", handler, action,...)
          })

## pretty print table
.prettyPrintTable = function(x, do.names = TRUE, justify="left") {

  ## the columns, a matrix
  if(is.matrix(x)) x = as.data.frame(x, stringsAsFactors = FALSE)
  
  y = sapply(x, function(i) format(i, justify=justify))

  if(do.names) {
    n = names(x)
    y = rbind(n,y)
    for(j in 1:ncol(y))
      y[,j] = format(y[,j], justify=justify)
  }
  
  z = sapply(1:nrow(y), function(i) paste(y[i,],sep="", collapse=" "))

  return(z)
}


##################################################
##################################################
### for filtering


## table for selecting values
## most methods in gdf.R inherited from gGrid class
setClass("gTableWithFiltertcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )


setGeneric(".gtableWithFilter",
           function(toolkit,
                    items,
                    multiple = FALSE,
                    chosencol = 1,                        # for drag and drop, value
                    icon.FUN = NULL,
                   filter.column = NULL,
                    filter.labels = NULL,
                    filter.FUN = NULL,   # two args gtable instance, filter.labels element
                    handler = NULL,
                   action = NULL,
                    container = NULL,
                    ...)
           standardGeneric(".gtableWithFilter")
           )

setMethod(".gtableWithFilter",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   items,
                   multiple = FALSE,
                   chosencol = 1,                        # for drag and drop, value
                   icon.FUN = NULL,
                   filter.column = NULL,
                   filter.labels = NULL,
                   filter.FUN = NULL,   # two args gtable instance, filter.labels element
                   handler = NULL,
                   action = NULL,
                   container = NULL,
                   ...) {
            
            ## we only get here *if* we are filtering
 

            g = ggroup(horizontal=FALSE, container=container, ...)

            fg = ggroup(cont=g)
            filterByLabel = glabel("Filter by:", container=fg)
            filterPopup = gdroplist(c(""), container=fg)
            
            tbl = gtable(items,
              multiple=multiple,
              chosencol=chosencol,
              cont=g, expand=TRUE)

            

            
            ## make an object to return
            obj = new("gTableWithFiltertcltk",block=g,widget=tbl,
              toolkit=toolkit,ID=getNewID())

            tag(obj, "allItems") <- items
            tag(obj, "tbl") <- tbl
            tag(obj, "filterPopup") <- filterPopup
            tag(obj, "filterByLabel") <- filterByLabel

            
            ## one of filter.column or filter.fun is non-NULL
            if(is.null(filter.FUN)) {
              ## define filter.FUN
              filter.FUN = function(DF, filterBy) {
                if(filterBy == "") return(rep(TRUE,nrow(DF)))
                inds = as.character(DF[,filter.column]) == filterBy
              }
              
              ## set up droplist
              filterPopup[] <- c("",sort(unique(as.character(items[,filter.column]))))
              svalue(filterByLabel) <- paste("Filter by",names(items)[filter.column],"==",sep=" ", collapse=" ")
            } else {
              ## set up droplist
              filterPopup[] <- c("",filter.labels)
            }

            tag(obj,"filter.FUN") <- filter.FUN

            ## get obj from scoping
            addHandlerChanged(filterPopup,action=obj,
                              handler=function(h,...) {
                                DF = tag(obj, "allItems")
                                tbl = tag(obj,"tbl")
                                filter.fun = tag(obj,"filter.FUN")
                                fval = svalue(h$obj) # popup

                                inds = filter.FUN(DF, fval)
                                ## update  tbl
                                obj[,] <- DF[inds,]
                                ## but keep allItems
                                tag(obj,"allItems") <- DF
                              })
            ## add handler to gtable object, but pass in override for methods
            if(!is.null(handler)) 
             ID= addhandlerchanged(tbl,handler,action,actualobj=obj,...)
            
            return(obj)
          })


          

setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTableWithFiltertcltk"),
          function(obj, toolkit, index=NULL, drop=NULL,...) {

            if(!is.null(index) && index) {
              cat("The index refers to the visible data value, not the entire data frame\n")
            }

            return(svalue(obj@widget, toolkit=toolkit, index=index, drop=drop, ...))

          })

## refers to visible
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gTableWithFiltertcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   tbl = tag(obj,"tbl")
                   svalue(tbl, toolkit=toolkit, index=index,  ...) <- value

                   return(obj)
                 })


## retrieve values
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTableWithFiltertcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            tbl = tag(x,"tbl")
                                        # dot function
            .leftBracket(tbl, toolkit, i, j, ..., drop=drop)
          })
            
setMethod("[",
          signature(x="gTableWithFiltertcltk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop) 
          })
## replace values
setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTableWithFiltertcltk"),
          function(x, toolkit, i, j, ..., value) {
            if(!missing(i) || !missing(j)) {
              cat("[<- only replaces the entire object. Try obj[,]<-value\n")
              return(x)
            }

            ## underlying gtable object
            tbl = tag(x,"tbl")

            ## We have to a) update allItems, b) update table
            tag(x, "allItems") <- value
            ## tbl needs to be filtered
            DF = value
            fval = svalue(tag(x, "filterPopup"))
            if(fval == "") {
              tbl[,] <- DF
            } else {
              filter.FUN = tag(x,"filter.FUN")
              inds = filter.FUN(DF, fval)
              tbl[,] <- DF[inds,,drop=FALSE]
            }
              

            return(x)
           })

setReplaceMethod("[",
                 signature(x="gTableWithFiltertcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

## dim
setMethod(".dim",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTableWithFiltertcltk"),
          function(x, toolkit) {
            tbl = tag(x,"tbl")
            return(dim(tbl))
          })
## length
setMethod(".length",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTableWithFiltertcltk"),
          function(x, toolkit) {
            tbl = tag(x,"tbl")
            return(length(tbl))
          })

## size<- work on tl
setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gTableWithFiltertcltk"),
                 function(obj, toolkit, ..., value) {
                   tbl = tag(obj,"tbl")
                   size(tbl) <- value
                   return(obj)
                 })

## handlers

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTableWithFiltertcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            tbl = tag(obj,"tbl")
            .addhandlerdoubleclick(tbl, toolkit, handler, action,actualobj=obj)
          })

## same as changed
setMethod(".addhandlerdoubleclick",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTableWithFiltertcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerchanged(obj, toolkit, handler, action,...)
          })

## when a selection is changed
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTableWithFiltertcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            tbl = tag(obj,"tbl")
            .addHandler(tbl,toolkit,signal="<<ListboxSelect>>", handler, action,
                        actualobj=obj)
          })


         
         
         

