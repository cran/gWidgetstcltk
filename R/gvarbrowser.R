## Use this to filter by type
## knownTypes in common
## return data frame with variables
setClass("gVarbrowsertcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

## THe main object
setMethod(".gvarbrowser",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   handler = NULL,
                   action = "summary",
                   container = NULL,
                   ...) {
            
            force(toolkit)

            if(is.null(handler) && !is.null(action)) {
              handler = function(h,...) {
                value = svalue(h$obj)   # drop = TRUE implicit
                print(value)
                if(!is.null(action))
                  print(do.call(h$action,list(svalue(value))))
              }
            }

            
            theHandler=handler
            theAction=action

            theArgs = list(...)
            width = ifelse(is.null(theArgs$width), 150, theArgs$width)
            height = ifelse(is.null(theArgs$width), 200, theArgs$height)

            
            g = ggroup(horizontal=FALSE, cont=container,...)
            
#            filterGroup = ggroup(cont=g, expand=TRUE)
            glabel("Filter workspace by:", cont=g)
            filterBy = gdroplist(names(knownTypes), cont=g)
            gseparator(cont=g)
            buttonGroup = ggroup(cont=g, expand=TRUE)

            comps = gtable(data.frame(name=c(""),type=c("")),
              width=width,height=height,
              cont=g, expand=TRUE)

            obj = new("gVarbrowsertcltk",block=g, widget=comps,
              toolkit=toolkit,ID=getNewID())




            buttonHandler = function(h,...) {
              l <- tag(obj,"l")

              if(length(l) == 1) {
              } else {
                if(h$action + 1 <= length(l)) {
                  for(i in rev((h$action+1):length(l))) {
                    delete(g,l[[i]])
                    l[[i]] <- NULL
                    tag(obj,"l") <- l
                    update(obj)
                  }
                }
              }
            }


            addButton = function(name) {
              l <- tag(obj,"l")
              n = length(l)
              
              l[[n+1]] <- gbutton(name, cont=buttonGroup, action=n+1,
                                  handler=buttonHandler)
              tag(obj,"l") <- l
            }
            

            ## save objects
            l <- list()
            l[[1]] = gbutton("Workspace",cont=buttonGroup, action=1,handler = buttonHandler)
            tag(obj,"l") <- l
            tag(obj,"filterBy") <- filterBy
            
            ## set up handlers
            addHandlerChanged(filterBy, handler = function(h,...)
                              update(h$action), action=obj)

            addHandlerChanged(comps, action=obj, handler = function(h,...) {
              if(canExpand(getCurName(h$action))) {
                addButton(svalue(comps))
                update(h$action)
              } else {
                h$obj = h$action        # change obj to obj
                h$action = theAction
                theHandler(h,...)
              }
            })

            adddropsource(comps, action=obj, handler = function(h,...) {
              return(getCurName(h$action))
            })


            ## update
            update(obj)                 # put into an idle time

            ## add idle handler
            ID = addhandleridle(obj,handler=function(h,...) update(obj))
            tag(obj,"idleHandlerID") <- ID
            addhandlerdestroy(obj,handler = function(h,...) {
              removehandler(obj,tag(obj,"idleHandlerID"))
            })
            
            ## all done
            return(obj)
          })


### functions needed
getCurBase = function(obj) {
  l <- tag(obj,"l")
  x = sapply(l, svalue)
  if(length(x) > 1) {
    curBase = paste(x[-1],collapse="$")
  } else {
    curBase = ""
  }
  return(curBase)
}
getCurName = function(obj) {
  comps <- obj@widget
  
  curBase = getCurBase(obj)
  val = svalue(comps, drop=TRUE)
  if(curBase == "")
    return(val)
  else
    return(paste(curBase,val,sep="$"))
}

canExpand = function(name) {
  
  x = getObjectFromString(name)

  if(is.list(x) && length(names(x)) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

setMethod(".update",
          signature(toolkit="guiWidgetsToolkittcltk",object="gVarbrowsertcltk"),
          function(object, toolkit, ...) {
            filterBy = tag(object,"filterBy")
            comps = object@widget
            curValue = svalue(comps)    # for setting back
            curBase = getCurBase(object)
            
            if(curBase == "") {
              filterWith = svalue(filterBy)
              if(length(filterWith) == 0 || filterWith =="")
                vals = getObjectsWithType(root=NULL)[,,drop=FALSE]
              else
                vals = getObjectsWithType(root=NULL,filter = knownTypes[[filterWith]])[,,drop=FALSE]
            } else {
              vals = getObjectsWithType(curBase)
            }
            comps[] <- vals
            if(length(curValue) > 0)
              svalue(comps) <- curValue
          }
          )



### methods
## push methods and handlers down to tree in this case

setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gVarbrowsertcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            getCurName(obj)
          })

## These should be able to get and set up known types
setMethod("[",
          signature(x="gVarbrowsertcltk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x,guiToolkit("tcltk"), i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gVarbrowsertcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            cat("No [ method for gvarbrowser\n")
          })


## push down onto gtable object
setReplaceMethod(".size",
                 signature(toolkit="guiWidgetsToolkittcltk",
                           obj="gVarbrowsertcltk"),
                 function(obj, toolkit, ..., value) {
                   size(obj@widget) <- value
                   return(obj)
                 })




## can't do this without messing up current handler
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gVarbrowsertcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            cat("gvarbrowser handlers must be added during construction of the widget.\n")
          })



