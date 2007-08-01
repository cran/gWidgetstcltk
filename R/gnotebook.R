## TODO:
## * drag and drop onto tabs, raise on motion,
## * 
## class previously defined The page is referred to below by curPage
## (the name inside the list) and by index number. Confusing! But it
## handles dispose

setMethod(".gnotebook",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   tab.pos = 3,                          # same as pos= in text
                   closebuttons = FALSE,
                   dontCloseThese = NULL,                 # integer of tabs not to close
                   container=NULL,                           # add to this container
                   ...) {
            
            force(toolkit)

            widgetList <- list()
            buttonList <- list()
            curPage = "0"                 # the character, not indexof page



            ## layout according to tab.pos
            if(tab.pos == 1) {
              g = ggroup(horizontal=FALSE, cont=container,...)
              nb = ggroup(cont=g,expand=TRUE, anchor=c(-1,1))
              buttonGroup = ggroup(horizontal=TRUE, cont=g, anchor=c(-1,0))
            } else if(tab.pos == 2) {
              g = ggroup(horizontal=TRUE, cont=container,...)
              buttonGroup = ggroup(horizontal=FALSE, cont=g, anchor=c(-1,0))
              nb = ggroup(cont=g,expand=TRUE, anchor=c(-1,1))
            } else if(tab.pos == 3) {
              g = ggroup(horizontal=FALSE, cont=container,...)
              buttonGroup = ggroup(horizontal=TRUE, cont=g, anchor=c(-1,0))
              nb = ggroup(cont=g,expand=TRUE, anchor=c(-1,1))
            } else if(tab.pos == 4) {
              g = ggroup(horizontal=TRUE, cont=container,...)
              nb = ggroup(cont=g,expand=TRUE, anchor=c(-1,1))
              buttonGroup = ggroup(horizontal=FALSE, cont=g, anchor=c(-1,0))
            }              

            ## create gnotebook object
            obj = new("gNotebooktcltk", block=g, widget=nb,
              toolkit=toolkit,ID=getNewID(),
              closebuttons = as.logical(closebuttons),
              dontCloseThese = ifelse(is.null(dontCloseThese),0,dontCloseThese))

            tag(obj,"buttonList") <- buttonList
            tag(obj,"buttonGroup") <- buttonGroup
            tag(obj,"widgetList") <- widgetList
            tag(obj,"curPage") <- curPage

            invisible(obj)
          })

### methods
## return the current tab number
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gNotebooktcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            curPage = tag(obj,"curPage") # a character
            buttonList = tag(obj,"buttonList")
            which(curPage == names(buttonList))
          })

## set the current tab to value
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gNotebooktcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   widgetList = tag(obj,"widgetList")
                   buttonList = tag(obj,"buttonList")
                   curPage = tag(obj,"curPage")

                   if(value == 0) return(obj)

                   
                   n = length(obj)
                   oldPageno = 0
                   if(n > 0)
                     oldPageno = which(curPage == names(buttonList))
                   if(length(oldPageno) == 0) oldPageno=0

                   nb = obj@widget

                   if(oldPageno > 0 && oldPageno <= length(obj))
                     delete(nb,widgetList[[oldPageno]])

                   newPageno <- value     # numeric value of page
                   n <- length(buttonList)
                   if(newPageno > n) newPageno = n
                   if(n > 0)
                     sapply(names(buttonList), function(i) enabled(buttonList[[i]]) <- FALSE)
                   if(n > 0)            # cant set if none there
                     enabled(buttonList[[newPageno]]) <- TRUE

                   ## packingOptions
                   theArgs = list(...)
                   addArgs = list(nb, widgetList[[newPageno]])
                   addArgs = c(addArgs, theArgs$packingOptions)

                   do.call("add",addArgs)

                   tag(obj,"curPage") <- names(buttonList)[newPageno]
                   return(obj)
                 })


## remove the current tab
## this should be called delete -- which is used to remove objects
setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gNotebooktcltk"),
          function(obj, toolkit,  ...) {
            buttonList = tag(obj,"buttonList")
            buttonGroup = tag(obj,"buttonGroup")
            widgetList = tag(obj,"widgetList")
            nb = obj@widget
            
            theArgs = list(...)
            to.right=ifelse(!is.null(theArgs$to.right), theArgs$to.right,FALSE)
            dontCloseThese = obj@dontCloseThese
            if(dontCloseThese == 0) dontCloseThese = NULL
            deleteOK = function(i) {
              if(is.null(dontCloseThese)) return(TRUE)
              if(i %in% dontCloseThese) return(FALSE)
              return(TRUE)
            }
            cur.pageno = svalue(obj)

            ## we clear out the current page unless there is more!
            inds = 0
            if(to.right) {
              n = length(obj)
              no.right = n - cur.pageno
              if(no.right > 0) 
                inds = no.right:0      # must work from last backwards
            }
            ## clear out
            for(i in inds) {
              j = cur.pageno + i
              if(deleteOK(j)) {
                ## delete from button list,
                svalue(obj) <- j
                delete(buttonGroup, buttonList[[j]])
                delete(nb, widgetList[[j]])
                buttonList[[j]] <- NULL
                widgetList[[j]] <- NULL
              }
            }
            tag(obj,"buttonList") <- buttonList
            tag(obj,"widgetList") <- widgetList

            if(cur.pageno <= length(obj))
              svalue(obj) <- cur.pageno
            else
              svalue(obj) <- length(obj)
          })



### add() is a workhorse method here. Several args available in ...
#add.gNotebook = functionf(obj, value,
#  label="", markup = FALSE,
#  index = NULL, override.closebutton = FALSE, ...) {
setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gNotebooktcltk",
                    value="guiWidget"),
          function(obj, toolkit, value,  ...) {
            .add(obj, toolkit, value@widget, ...)
          })

setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gNotebooktcltk",
                    value="gWidgettcltk"),
          function(obj, toolkit, value,  ...) {
            ## in ... we have many possibilies
            ## label -- for setting label  (also look for name)
            ## index for setting the index of page to add
            ## markup -- markup label
            ## override.closebutton -- to not put closebutton even if set in constructor
            widgetList = tag(obj,"widgetList")
            buttonList = tag(obj,"buttonList")
            buttonGroup = tag(obj,"buttonGroup")
            nb = obj@widget
            curPage = tag(obj,"curPage")
            
            ## process ...
            theArgs = list(...)                      # for making generic
            if(!is.null(theArgs$label)) {
              label = theArgs$label
            } else if(!is.null(theArgs$name)) {
              label = theArgs$name
            } else {
              label = id(obj)
              if(is.null(label))
                label = "unnamed"
            }
            
            index = if (is.null(theArgs$index)) NULL else theArgs$index
            if(!is.null(theArgs$pageno)) index = theArgs$pageno # also called paegno
            markup = if (is.null(theArgs$markup)) FALSE  else theArgs$markup
            override.closebutton =
              if (is.null(theArgs$override.closebutton))
                FALSE
              else
                as.logical(theArgs$override.closebutton)

            packingOptions = list()
            packingOptions$anchor = if(is.null(theArgs$anchor))
              c(0,0)
            else
              theArgs$anchor
            
            if(!is.null(theArgs$expand) && theArgs$expand) {
              packingOptions$expand = TRUE
              packingOptions$fill = "both"
            }
            
            ## let's go
            nb = obj@widget

            
            ## label -- a string in tcltk
            if(!is.character(label))
              label = svalue(label)

            
            ## closebutton
            if(!is.null(obj@closebuttons) &&
               as.logical(obj@closebuttons) &&
               !override.closebutton) {
              ## do close buttons
              cat("Implement close buttons\n")
            } 

            
            ## where to add
            ### FIX THIS
##             if(is.null(index) | !is.numeric(index)) {
##               cat("index arg. not implemented.\n")
##             } else {
##                if(index < 0) index = 1 {
##                  ##
##                }
##              }
            


            ## add widget to list. delete/add in svalue call
            tmp <- names(buttonList)
            if(length(tmp) > 0)
              n <- as.numeric(tmp[length(tmp)])
            else
              n = 0
            newPage = as.character(n+1)

            widgetList[[newPage]] <- value

            ## add button to buttonList
            buttonList[[newPage]] <- glabel(label,cont=buttonGroup, anchor=c(1,0))
            font(buttonList[[newPage]]) <- c(family="monospace")
            tkconfigure(buttonList[[newPage]]@widget@widget,
                        relief="groove", borderwidth=1)
            ## add handler to button
            addhandlerclicked(buttonList[[newPage]],
                              action=newPage,
                              handler = function(h,...) {
                                desiredPage = h$action
                                thePageno = which(desiredPage == names(tag(obj,"buttonList")))
                                svalue(obj) <- thePageno
                              })
            ## add drop motion for labels
            adddropmotion(buttonList[[newPage]],
                          action= newPage,
                          handler=function(h,...) {
                            desiredPage = h$action
                            thePageno = which(desiredPage == names(tag(obj,"buttonList")))
                            svalue(obj) <- thePageno
                          })
            
            ## Add DND actions for labels
            ## implement me. Drop value of widget
            ## DOESN"T WORK. NO TWO HANDLERS?
##             adddropsource(buttonList[[newPage]],
##                           action = newPage,
##                           handler = function(h,...) {
##                             return(obj[newPage])
##                           })
                           

            
            ## save values
            tag(obj,"widgetList") = widgetList
            tag(obj,"buttonList") = buttonList

            ## all done, now call svalue to update page
            svalue(obj, packingOptions=packingOptions) <- length(obj)
          })
            
## Regular R methods treat gnotebook like a vector

## find out number of pages
setMethod(".length",
          signature(toolkit="guiWidgetsToolkittcltk",x="gNotebooktcltk"),
          function(x, toolkit) {
            length(tag(x,"buttonList"))
          })

## return tabnames
setMethod(".names",signature(toolkit="guiWidgetsToolkittcltk",x="gNotebooktcltk"),
          function(x, toolkit) {
            buttonList = tag(x,"buttonList")
            if(length(buttonList) > 0) {
              tmp = sapply(buttonList,svalue)
              attr(tmp,"names") <- NULL
              return(tmp)
            }
            else
              return(character(0))
          })

## can assigne with names(x) <-x or even names(x)[i] <- "single name"
setReplaceMethod(".names",
                 signature(toolkit="guiWidgetsToolkittcltk",x = "gNotebooktcltk"),
                 function(x,toolkit, value) {
                   n = length(x)
                   if(length(value) != n)
                     stop("New names for notebook must have proper length")
                   
                   buttonList = tag(x,"buttonList")
                   sapply(1:n, function(i) svalue(buttonList[[i]]) <- value[i])

                   tag(x,"buttonList") <- buttonList

                   return(x)
                 })


## return widget contained in notebook page i as a  list or single widget
setMethod("[",
          signature(x="gNotebooktcltk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gNotebooktcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            widgetList = tag(x,"widgetList")
            if(missing(i))
              i = 1:length(widgetList)
            if(length(i) > 1) {
              lst = sapply(i,function(j)
                widgetList[[j]]
                )
              return(lst)
            } else {
              if(i <= length(widgetList))
                return(widgetList[[i]])
              else
                return(NULL)
            }
          })


## Puts widget into a position
setReplaceMethod("[",
                 signature(x="gNotebooktcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gNotebooktcltk"),
          function(x, toolkit, i, j, ..., value) {
            warning("[<- not implemented for gnotebook in tcltk")
            return(x)
##             n = length(x)
##             if(missing(i)) {
##               add(x,value)                        # append!
##             } else {
##               if(length(i) == 1) {
##                 add(x, value, index = i)
##               } else {
##                 warning("Can't use '[' method for more than 1 element")
##               }
##             }
          })

## size<- work on group
setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gNotebooktcltk"),
                 function(obj, toolkit, ..., value) {
                   size(obj@block) <- value
                   return(obj)
                 })

### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gNotebooktcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerexpose(obj,toolkit, handler,action)
          })


setMethod(".addhandlerexpose",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gNotebooktcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            n <- length(obj)
            if(n > 0) {
              for(i in 1:n)  {
                if(!is.null(obj[i]))
                  addhandler(obj[i],"<Expose>",handler, action)
              }
            }
          })

