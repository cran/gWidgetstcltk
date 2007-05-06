MSG = function(...) cat("DEBUG",...,"\n")
missingMsg = function(x) {
  if(missing(x)) x = "XXX"
  cat("This method",x,"needs to be written\n")
}


## toolkit class
## register classes here for toolkits
setClass("guiWidgetsToolkittcltk",
         contains="guiWidgetsToolkit",
         prototype=prototype(new("guiWidgetsToolkit"))
         )




##################################################
## put S3 classes from tcltk into S4 classes
## got these from apropos("New") -> try(class(do.call(i,list())))

require(tcltk)
oldClasses =c("tkwin")
setClass("tcltkObject")
sapply(oldClasses, function(i) {
  setOldClass(i)
  setIs(i,"tcltkObject")
})


setOldClass("try-error")                # for handling try-errors


## a base class which is virtual


##################################################
## A virtual class to hold either RGTK or these guys

## A virtual class for our newly defined objects
## this one contains the ID for the object.
## this may better be done within the NAMESPACE

n=0;assignInNamespace("n",0,"gWidgetstcltk")
getNewID = function() {                 # get new one, incremented
  n = getFromNamespace("n",ns="gWidgetstcltk")
  assignInNamespace("n",n+1,ns="gWidgetstcltk")
  return(n+1)
}
         

setClass("gWidgettcltk",
         representation(ID="numeric"),
         )


setClassUnion("guiWidgetORgWidgettcltkORtcltkObject",
              c("guiWidget","gWidgettcltk","tcltkObject"))

## subclss
setClass("gComponenttcltk",
         representation(
                        block="guiWidgetORgWidgettcltkORtcltkObject",
                        widget="guiWidgetORgWidgettcltkORtcltkObject",
                        toolkit="guiWidgetsToolkit"
                        ),
         contains="gWidgettcltk",
         )
setClass("gContainertcltk",
         representation(
                        block="guiWidgetORgWidgettcltkORtcltkObject",
                        widget="guiWidgetORgWidgettcltkORtcltkObject",
                        toolkit="guiWidgetsToolkit"
                   ),
         contains="gWidgettcltk",
         )


## make tcltk S3 object S4 objects

oldclasses = c("tkwin")
for(i in oldclasses) {
  setOldClass(i)
  setIs(i,"guiWidgetORgWidgettcltkORtcltkObject")
}





##################################################
### Common methods.    Specific to a class are put into the file for that class

## we have two definitions. For instance, "svalue" and ".svalue". The "svalue" method dispatches on the object to the .svalue method. This allows us to use svalue instead of .svalue when defining the methods/constructors inside this package.


setMethod("svalue",signature(obj="gWidgettcltk"),
          function(obj, index=NULL, drop=NULL, ...) {
            .svalue(obj, obj@toolkit, ..., index=index, drop=drop)
          })



## svalue
## need method for character and AsIs
setMethod("svalue",signature(obj="character"),
          function(obj, index=NULL, drop=NULL, ...)  {
            ifelse(length(obj) == 1,
                   return(getObjectFromString(obj)),
                   return(obj)
                   )
          })
## method for Any is just a pass through
setMethod("svalue",signature(obj="ANY"),
          function(obj, index=NULL, drop=NULL, ...)  {
            return(obj)
          })


setMethod(".svalue",signature(toolkit = "guiWidgetsToolkittcltk", obj="character"),
          function(obj, toolkit, index=NULL, drop=NULL,  ...)  {
            ifelse(length(obj) == 1,
                   return(getObjectFromString(obj)),
                   return(NA)
                   )
          })

## svalue<- -- objec specific
setReplaceMethod("svalue",signature(obj="gWidgettcltk"),
          function(obj, index=NULL, ...,value) {
            .svalue(obj, obj@toolkit, index=index, ...) <- value
            obj
          })

                   
                 
## [
setMethod("[",
          signature(x="gWidgettcltk"),
          function(x,i,j,...,drop=TRUE) {
            
            return(.leftBracket(x, x@toolkit,i,j,...,drop=TRUE))
          })

## [<-
setReplaceMethod("[",signature(x="gWidgettcltk"),
          function(x,i,j,...,value) {
            if(missing(i) && missing(j))
              .leftBracket(x, x@toolkit,...) <- value
            else if(missing(j))
              .leftBracket(x, x@toolkit,i,...) <- value
            else 
              .leftBracket(x, x@toolkit,i,j,...) <- value
            return(x)
          })

## size ## return size -- not implemented
setMethod("size",signature(obj="gWidgettcltk"),
          function(obj, ...) {
            warning("size not defined, Set window size with size<-()")
            return()
            .size(obj, obj@toolkit,...)
          })

setMethod(".size", 
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, ...) {
            MSG("define .size")
            ##  what is size?
          })

## size<-
setReplaceMethod("size",signature(obj="gWidgettcltk"),
          function(obj, ..., value) {
            .size(obj, obj@toolkit,...) <- value
            return(obj)
          })

setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
                 function(obj, toolkit, ..., value) {
                   
                   return(obj)
                 })

## size for components is funny. For many width is characters, height
## is lines of text
setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gComponenttcltk"),
                 function(obj, toolkit, ..., value) {
                   ## width in characters, height in lines
                   ## convert Pixels to each
                   value = floor(value/c(5,15))
                   tkconfigure(getWidget(obj), width=value[1], height=value[2])
                   return(obj)
                 })

## this works if container has no children (gwindow) but fails otherwise.
setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gContainertcltk"),
                 function(obj, toolkit, ..., value) {
                   ## pixels for tkframe etc
                   tkconfigure(getWidget(obj), width=value[1], height=value[2])
                   return(obj)
                 })



## visible
setMethod("visible",signature(obj="gWidgettcltk"),
          function(obj, set=NULL, ...) {
            .visible(obj,obj@toolkit, set=set, ...)
          })

setMethod(".visible",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, set=TRUE, ...) {
            widget = obj@widget

            missingMsg(".visible")
            return()
            
            if(as.logical(set))
              widget$Show()
            else
              widget$Hide()
          })
setMethod(".visible",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="tkwin"),
                 function(obj, toolkit, set=TRUE, ...) {
                   ## visible not implemented
                 })


## visible<-
setReplaceMethod("visible",signature(obj="gWidgettcltk"),
          function(obj, ..., value) {
            .visible(obj, obj@toolkit, ...) <- value
            return(obj)
          })

setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
                 function(obj, toolkit, ..., value) {
                   .visible(getWidget(obj), toolkit, set=as.logical(value))
                   return(obj)
                 })
setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="tkwin"),
                 function(obj, toolkit, ..., value) {
                   ## visible not implemented
                   
                   return(obj)
                 })


## enabled -- TRUE If state is normal
setMethod("enabled",signature(obj="gWidgettcltk"),
          function(obj, ...) {
            .enabled(obj, obj@toolkit,...)
          })
setMethod(".enabled",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, ...) {
            state = as.character(tkcget(getWidget(obj),"-state"))
            if(state == "normal")
              return(TRUE)
            else
              return(FALSE)
          })

## enabled<-
setReplaceMethod("enabled",signature(obj="gWidgettcltk"),
          function(obj, ..., value) {
            .enabled(obj, obj@toolkit,...) <- value
            return(obj)
          })

setReplaceMethod(".enabled",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
                 function(obj, toolkit, ..., value) {
                   if(as.logical(value))
                     tkconfigure(getWidget(obj),state="normal")
                   else
                     tkconfigure(getWidget(obj),state="disabled")
                   return(obj)
                 })

## focus
setMethod("focus",signature(obj="gWidgettcltk"),
          function(obj, ...) {
            .focus(obj, obj@toolkit,...)
          })

setMethod(".focus",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, ...)
          tkfocus(getBlock(obj))
          )

## focus<-
setReplaceMethod("focus",signature(obj="gWidgettcltk"),
          function(obj, ..., value) {
            .focus(obj, obj@toolkit,...) <- value
            return(obj)
          })

setReplaceMethod(".focus",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, ..., value) {
            focus(obj@widget, toolkit, ...) <- value
            return(obj)
          })
                 

setReplaceMethod(".focus",
          signature(toolkit="guiWidgetsToolkittcltk",obj="tcltkObject"),
          function(obj, toolkit, ..., value) {
            value = as.logical(value)
            if(as.logical(value))
              tkfocus(getBlock(obj))

            return(obj)
          })

## font
## The .font method is not imported from gWidgets, or exported from gWidgetstcltk. Add this bac if you are going to use this method

setMethod("font",signature(obj="gWidgettcltk"),
          function(obj, ...) {
            warning("font() not defined. Set fonts with font<-")
            return()
            .font(obj, obj@toolkit,...)
          })

## font<-
setReplaceMethod("font",signature(obj="gWidgettcltk"),
          function(obj, ..., value) {
            .font(obj, obj@toolkit,...) <- value
            return(obj)
          })
setReplaceMethod(".font",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
                 function(obj, toolkit, ..., value) {
                   .font(obj@widget, toolkit, ...) <- value
                   return(obj)
                 })

.font.styles = list(
  families = c("normal","sans","serif","monospace"),
  weights = c("normal","oblique","italic"),
  styles = c("ultra-light","light","normal","bold","ultra-bold","heavy"),
  colors = c("black","blue","red","green","brown","yellow","pink")
)  


setReplaceMethod(".font",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="tcltkObject"),
                 function(obj, toolkit, ..., value) {
                   ## we use the styles above. So must translate
                   fontFamily = function(i)
                     switch(i,
                            "normal"="times",
                            "sans" = "helvetica",
                            "serif"="times",
                            "monospace"="courier",
                            i)
                   fontWeight = function(i) 
                     switch(i,
                            "normal"="normal",
                            "oblique"="normal",
                            "italic"="italic",
                            i)
                   fontStyle = function(i)
                     switch(i,
                            "bold"="bold",
                            "ultra-bold"="bold",
                            "heavy" = "bold",
                            i)   # all others


                   ## turn vector into list for consitency
                   if(!is.list(value))
                     value = lapply(value,function(x) x)

                   theArgs = list()
                   
                   if(!is.null(value$family)) theArgs$family = fontFamily(value$family)
                   if(!is.null(value$weight)) theArgs$slant = fontWeight(value$weight)
                   if(!is.null(value$style)) theArgs$weight = fontStyle(value$style)

                   if(!is.null(value$size)) theArgs$size = as.integer(value$size)

                   ## now call
                   theFont = do.call("tkfont.create",theArgs)
                   tkconfigure(getWidget(obj), font=theFont)

                   ## colors are different
                   if(!is.null(value$color)) tkconfigure(getWidget(obj), foreground=value$color)

                   ## all done
                   return(obj)
                   
                 })



## tag, tag<-
## In RGtk2 we used the getData() and setData() methods. In tcltk we use the
## crummy implementation from rJava -- a list which grows without bound




## create namespace object
tags = list()
assignInNamespace("tags",list(),"gWidgetstcltk")

## clear out tags for this ID. Not exported. Is this used?
Tagsclear = function(obj) {

  id = obj@ID
  
  tags = getFromNamespace("tags",ns="gWidgetstcltk")
  allKeys = names(tags)

  inds = grep(paste("^",id,"::",sep=""),allKeys)
  if(length(inds) == 0)
    return(NA)

  ## else
  tags[[inds]] <- NULL
  assignInNamespace("tags",tags,ns="gWidgetstcltk")
}


setMethod("tag",signature(obj="gWidgettcltk"),
          function(obj,i,drop=TRUE, ...) {
            if(missing(drop)) drop <- TRUE
            .tag(obj, obj@toolkit,i, drop=drop,...)
          })
## dispatch in *this* toolkit, not present in obj
setMethod("tag",signature(obj="tcltkObject"),
          function(obj,i,drop=TRUE, ...) {
            if(missing(drop)) drop <- TRUE            
            .tag(obj, guiToolkit("tcltk"),i, drop=drop,...)
          })

setMethod(".tag", signature(toolkit="guiWidgetsToolkittcltk",obj="guiWidget"),
          function(obj, toolkit, i, drop=TRUE, ...) {
            if(missing(i)) i = NULL
            if(missing(drop)) drop <- TRUE                        
            .tag(obj@widget,toolkit,  i, drop=drop,  ...)
          })
setMethod(".tag", signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, i, drop=TRUE, ...) {
            if(missing(i)) i = NULL
            if(missing(drop)) drop <- TRUE                                    


            id = obj@ID

            ## get all values for this id
            tags = getFromNamespace("tags",ns="gWidgetstcltk")
            allKeys = names(tags)

            inds = grep(paste("^",id,"::",sep=""),allKeys)
            if(length(inds) == 0)
              return(NULL)

            justTheKeys = sapply(allKeys[inds],function(keyWithID) {
              sub(paste("^",id,"::",sep=""),"",keyWithID)
            })

            tagByKey = list()
            for(key in justTheKeys) 
              tagByKey[[key]] = tags[[paste(id,key,sep="::")]]
                      
            
            
            if(is.null(i)) return(tagByKey)

            if(drop) {
              if(length(i) == 1)
                return(tagByKey[[i]])
              else
                return(sapply(i, function(j) tagByKey[j]))
            } else {
              return(sapply(i, function(j) tagByKey[j]))
            }
          })

## tag <-
setReplaceMethod("tag",signature(obj="gWidgettcltk"),
          function(obj, i, replace=TRUE, ..., value) {
            .tag(obj, obj@toolkit,i,replace, ...) <- value
            return(obj)
          })
## dispatch in *this* toolkit, not present in obj
setReplaceMethod("tag",signature(obj="tcltkObject"),
          function(obj,i, replace=TRUE, ..., value) {
            .tag(obj, guiToolkit("tcltk"),i, replace, ...) <- value
            return(obj)
          })

## objects can be in many different flavors: guiWIdget, gWidgettcltk, tcltkObject
setReplaceMethod(".tag", signature(toolkit="guiWidgetsToolkittcltk",obj="guiWidget"),
          function(obj, toolkit, i, replace=TRUE, ..., value) {
            if(missing(i)) i = NULL
            .tag(obj@widget,toolkit,  i, replace, ...) <- value
            return(obj)
          })

setReplaceMethod(".tag", signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, i, replace=TRUE, ..., value) {
            if(missing(i)) i = NULL
            

            id = obj@ID
            key = paste(id,i,sep="::")
            
            ## if we append we need to work a little harder
            tags = getFromNamespace("tags",ns="gWidgetstcltk")
  
            if(replace==FALSE) {
              value = c(tags[[key]],value)
            }

            tags[[key]] <- value
            assignInNamespace("tags", tags,ns="gWidgetstcltk")

            return(obj)

          })
## setReplaceMethod(".tag", signature(toolkit="guiWidgetsToolkittcltk",obj="tcltkObject"),
##           function(obj, toolkit, i, replace=TRUE, ..., value) {
##             if(missing(i) || is.null(i)) {
##               warning("Need to specify a key to the 'i' argument of tag<-")
##             } else {
##               theArgs = list(...)
##               replaceIt = as.logical(replace)

##               missingMsg(".tag<-");return()
              
##               allData = obj$GetData(".tagKey")
##               if(is.null(allData)) allData = list()
              
##               if(replaceIt) {
##                 allData[[i]] <- value
##               } else {
##                 allData[[i]] <- c(allData[[i]], value)
##               }
##               obj$SetData(".tagKey", allData)
##             }
##             return(obj)
##           })


##################################################
## id -- define for "ANY" as well
setMethod("id",signature(obj="gWidgettcltk"),
          function(obj, ...) {
            tag(obj,".tcltkID")
          })
setMethod("id",signature(obj="tcltkObject"),
          function(obj, ...) {
            tag(obj, ".tcltkID", ...)
            return(obj)
          })
setMethod("id",signature(obj="ANY"),
          function(obj, ...) {
            if(!is.null(theID<- attr(obj,"id"))) {
              return(theID)
            } else {
              if(is.character(obj)) {
                return(obj[1])
              } else {
                dps = deparse(substitute(obj))
                attr(obj,"id") <- dps
                return(dps)
              }
            }
          })


setMethod(".id", signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, ...) {
            tag(obj,".tcltkID", ...)
          })
setMethod(".id", signature(toolkit="guiWidgetsToolkittcltk",obj="tcltkObject"),
          function(obj, toolkit,  ...) {
            return(tag(obj,".tcltkID"))
          })


## id<-
setReplaceMethod("id",signature(obj="gWidgettcltk"),
          function(obj, ..., value) {
            tag(obj,".tcltkID", ...) <- value
            return(obj)
          })
## dispatch in *this* toolkit, not present in obj
setReplaceMethod("id",signature(obj="tcltkObject"),
          function(obj, ..., value) {
            tag(obj, ".tcltkID", ...) <- value
            return(obj)
          })
setReplaceMethod("id",signature(obj="ANY"),
          function(obj, ..., value) {
            attr(obj,"id") <- value
            return(obj)
          })


## we need a .id to handle dispatch from guiWidgets, otherwise, we use id()
setReplaceMethod(".id", signature(toolkit="guiWidgetsToolkittcltk",
                                  obj="gWidgettcltk"),
          function(obj, toolkit, ..., value) {
            id(obj, ...) <- value
            return(obj)
          })



## add method is biggie
## we have several levels of classes here guiWidget -- gWidgetRGkt -- tcltkObject, when
## we get down to that level we can finally add
setMethod("add",signature(obj="gWidgettcltk"),
          function(obj, value, ...) {
            .add(obj, obj@toolkit,value,...)
          })
setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",
                    obj="guiWidget", value="ANY"),
          function(obj, toolkit, value, ...) {
            cat("Can't add without a value\n")
          })
setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",
                    obj="gWidgettcltk", value="try-error"),
          function(obj, toolkit, value, ...) {
            gmessage(paste("Error:",x))
          })
## pushdonw
setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",
                    obj="guiWidget", value="guiWidgetORgWidgettcltkORtcltkObject"),
          function(obj, toolkit, value, ...) {
            .add(obj@widget, toolkit, value, ...)
          })

## for gWindow
setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",
                    obj="gContainertcltk", value="guiWidget"),
          function(obj, toolkit, value, ...) {
            .add(obj, toolkit, value@widget, ...)
          })

## for gContainer
setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk", obj="gContainertcltk",value="gWidgettcltk"),
          function(obj, toolkit, value, ...) {
            theArgs = list(...)

            argList = list(getBlock(value))

            ## expand. use fill, expand didn't
            if(!is.null(theArgs$expand) && theArgs$expand) {
              argList$expand = TRUE
              argList$fill = "both"
            }

            ## anchor
            if(is.null(theArgs$anchor))
              anchor = c(-1,1)          # low and inside!
            else
              anchor = theArgs$anchor ## anchor = c(a,b) a,b in {-1,0,1}

            argList$anchor = xyToAnchor(anchor)

            if(obj@horizontal)
              argList$side = "left"
            else
              argList$side = "top"

            ## call tkpack
            do.call("tkpack",argList)
          })

## setMethod(".add",
##           signature(toolkit="guiWidgetsToolkittcltk",obj="gContainertcltk", value="gWidgettcltk"),
##           function(obj, toolkit, value, ...) {
##             .add(obj, toolkit, value@block, ...)
##           })




## addSPring, addSpace
setMethod("addSpring",signature(obj="gWidgettcltk"),
          function(obj, ...) {
            .addSpring(obj, obj@toolkit,...)
          })

setMethod(".addSpring",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gContainertcltk"),
          function(obj, toolkit, ...) {


            tt <- getBlock(obj)

            if(obj@horizontal)
              tkpack(tklabel(tt,text=" "),expand=TRUE,fill="x",side="left")
            else
              tkpack(tklabel(tt,text=" "),expand=TRUE,fill="y",side="top")
            invisible()
          })

setMethod("addSpace",signature(obj="gWidgettcltk"),
          function(obj, value, ...) {
            .addSpace(obj,obj@toolkit,value,...)
          })

setMethod(".addSpace",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gContainertcltk"),
          function(obj, toolkit, value, ...) {
            theArgs = list(...)
            horizontal = ifelse(is.null(theArgs$horizontal),
              TRUE,
              as.logical(theArgs$horizontal))

            tt <- getBlock(obj)
            value = as.integer(value)
            if(horizontal)
              tkpack(tklabel(tt, text=""),side="left",padx=value)
            else
              tkpack(tklabel(tt, text=""),side="top",pady=value)
            invisible()
          })

## delete -- get down to two tcltkObjects
setMethod("delete",signature(obj="gWidgettcltk"),
          function(obj, widget, ...) {
            .delete(obj, obj@toolkit,widget,...)
          })

## push down to tcltk vs tcltk. Can be 9 possiblities!
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gContainertcltk",widget="guiWidget"),
          function(obj, toolkit, widget, ...) {
            .delete(obj, toolkit, widget@widget, ...)
          })
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gContainertcltk",widget="gWidgettcltk"),
          function(obj, toolkit, widget, ...) {
            ## call remove on container
            tkpack.forget(getBlock(widget))
          })

## dispose -- delete the parent window, or something else
setMethod("dispose",signature(obj="gWidgettcltk"),
          function(obj, ...) {
            .dispose(obj, obj@toolkit,...)
          })

setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, ...) {
            tcl("after",5,function() {
              tt <- getTopParent(getBlock(obj))
              tkgrab.release(tt)
              tkdestroy(tt)
            })                          # wait a pause 
          })




## update
setMethod("update",signature(object="gWidgettcltk"),
          function(object, ...) {
            .update(object, object@toolkit, ...)
          })

setMethod(".update",
          signature(toolkit="guiWidgetsToolkittcltk",object="gComponenttcltk"),
          function(object, toolkit, ...) {

            missingMsg(".update");return()
            
            object@widget$QueueDraw()
          })

##
##
##################################################


##################################################
## handlers. Also in aaaHandlers
##
## basic handler for adding with a signal. Not exported.
setGeneric("addhandler", function(obj, signal, handler, action=NULL, ...)
           standardGeneric("addhandler"))
setMethod("addhandler",signature(obj="guiWidget"),
          function(obj, signal, handler, action=NULL, ...) {
            .addHandler(obj@widget, obj@toolkit, signal, handler, action, ...)
          })
setMethod("addhandler",signature(obj="gWidgettcltk"),
          function(obj, signal, handler, action=NULL, ...) {
            .addHandler(obj, obj@toolkit, signal, handler, action, ...)
          })
setMethod("addhandler",signature(obj="tcltkObject"),
          function(obj, signal, handler, action=NULL, ...) {
            .addHandler(obj, guiToolkit("tcltk"), signal, handler, action, ...)
          })

## method for dispatch
setGeneric(".addHandler",
           function(obj, toolkit,
                  signal, handler, action=NULL, ...)
           standardGeneric(".addHandler"))


setMethod(".addHandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="guiWidget"),
          function(obj, toolkit,
                   signal, handler, action=NULL, ...) {
            .addHandler(obj@widget, force(toolkit), signal, handler, action, ...)
          })


## in aaaHandlers
## setMethod(".addHandler",
##           signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
##           function(obj, toolkit,
##                    signal, handler, action=NULL, ...) {
##             ## use tkbind
##             tkbind(obj@widget,signal,
##                    function(...) {
##                      h = list(ref=obj, action=action)
##                      handler(h,...)
##                      })
##           })


## removew handler
## removehandler
setMethod("removehandler", signature("gWidgettcltk"),
          function(obj, ID=NULL, ...) {
            .removehandler(obj, obj@toolkit, ID, ...)
          })
setMethod("removehandler", signature("tcltkObject"),
          function(obj, ID=NULL, ...) {
            .removehandler(obj, guiToolkit("tcltk"), ID, ...)
          })

## in aaaHandlers
## setMethod(".removehandler",
##           signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
##           function(obj, toolkit, ID=NULL, ...) {

##             ## ID here has two components
##             type = ID[1]
##             handlerID=as.character(ID[2])
##             ID = as.character(obj@ID)

##             ## remove from list
##             allHandlers = getFromNamespace("allHandlers",ns="gWidgetstcltk")

##             ## is this a idleHandler
##             if(type == "addIdleListener") {
##               t = allHandlers[[ID]][[type]][[handlerID]]$timer
##               t = .jcall(t,"V","stopTimer")
##             }
##             allHandlers[[ID]][[type]][[handlerID]]<-NULL
##               ## now store the hash
##             assignInNamespace("allHandlers",allHandlers,ns="gWidgetstcltk")
##           })


## addhandlerchanged
setMethod("addhandlerchanged",signature(obj="gWidgettcltk"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerchanged(obj, obj@toolkit, handler, action, ...)
          })
setMethod("addhandlerchanged",signature(obj="tcltkObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerchanged(obj, guiToolkit("tcltk"), handler, action, ...)
          })
setMethod("addhandlerchanged",signature(obj="ANY"),
          function(obj, handler=NULL, action=NULL, ...) {
            warning("No method addhandlerchanged for object of class",class(obj),"\n")
          })

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="<KeyPress>",
                        handler=handler, action=action, ...)
          })


## expose: expose-event or realize
setMethod("addhandlerexpose",signature(obj="gWidgettcltk"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerexpose(obj,obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerexpose",signature(obj="tcltkObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerexpose(obj, guiToolkit("tcltk"), handler, action, ...)
          })

setMethod(".addhandlerexpose",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="<Expose>",
                        handler=handler, action=action, ...)
          })

setMethod(".addhandlerexpose",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gComponenttcltk"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj,toolkit, signal="<Realize>",
                        handler=handler, action=action, ...)
          })

## unrealize: unrealize
setMethod("addhandlerunrealize",signature(obj="gWidgettcltk"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerunrealize(obj, obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerunrealize",signature(obj="tcltkObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerunrealize(obj, guiToolkit("tcltk"),handler, action, ...)
          })

setMethod(".addhandlerunrealize",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="<Unrealize>",
                        handler=handler, action=action, ...)
          })

## destroy: destroy
setMethod("addhandlerdestroy",signature(obj="gWidgettcltk"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerdestroy(obj, obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerdestroy",signature(obj="tcltkObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerdestroy(obj, guiToolkit("tcltk"),handler, action, ...)
          })

setMethod(".addhandlerdestroy",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="<Destroy>",
                        handler=handler, action=action, ...)
          })

## keystroke: changed
setMethod("addhandlerkeystroke",signature(obj="gWidgettcltk"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerkeystroke(obj, obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerkeystroke",signature(obj="tcltkObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerkeystroke(obj, guiToolkit("tcltk"),handler, action, ...)
          })

setMethod(".addhandlerkeystroke",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="<KeyPress>",
                        handler=handler, action=action, ...)
          })

## clicked: clicked
setMethod("addhandlerclicked",signature(obj="gWidgettcltk"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerclicked(obj, obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerclicked",signature(obj="tcltkObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerclicked(obj, guiToolkit("tcltk"),handler, action, ...)
          })

setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="<Button-1>",
                        handler=handler, action=action, ...)
          })

## doubleclick: no default
setMethod("addhandlerdoubleclick",signature(obj="gWidgettcltk"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerdoubleclick(obj,obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerdoubleclick",signature(obj="tcltkObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerdoubleclick(obj,guiToolkit("tcltk"),handler, action, ...)
          })

setMethod(".addhandlerdoubleclick",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="<Double-Button-1>",
                        handler=handler, action=action, ...)
          })

## rightclick: button-press-event -- handle separately
setMethod("addhandlerrightclick",signature(obj="gWidgettcltk"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerrightclick(obj,obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerrightclick",signature(obj="tcltkObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerrightclick(obj,guiToolkit("tcltk"),handler, action, ...)
          })

setMethod(".addhandlerrightclick",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
             .addHandler(obj, toolkit, signal="<Button-3>",
                        handler=handler, action=action, ...)
          })

## idle
setMethod("addhandleridle",signature(obj="gWidgettcltk"),
          function(obj, handler=NULL, action=NULL, interval=1000, ...) {
            .addhandleridle(obj, obj@toolkit,
                            handler=handler, action=action, interval=interval, ...)
          })
setMethod("addhandleridle",signature(obj="tcltkObject"),
          function(obj, handler=NULL, action=NULL, interval=1000, ...) {
            .addhandleridle(obj, guiToolkit("tcltk"),
                            handler=handler, action=action, interval=interval, ...)
          })

## in aaaHandlers
## setMethod(".addhandleridle",
##           signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
##           function(obj, toolkit,
##                    handler=NULL, action=NULL, interval=1000, ...) {
##             cat("IMPLEMENT idle handler")
##           })


## addpopumenu
## ## this does not get exported
.addPopupMenu = function(obj,   menulist, action=NULL,...) {
  editPopupMenu <- getWidget(gmenu(menulist, popup=TRUE, action=action,container=obj, ...))
            
  RightClick <- function(x,y) # x and y are the mouse coordinates
    {
      V = getWidget(obj)
      rootx <- as.integer(tkwinfo("rootx",V))
      rooty <- as.integer(tkwinfo("rooty",V))
      xTxt <- as.integer(x)+rootx
      yTxt <- as.integer(y)+rooty
      tcl("tk_popup",editPopupMenu,xTxt,yTxt)
              }
  tkbind(getWidget(obj), "<Button-1>",RightClick)
}
  
.add3rdMousePopupMenu = function(obj,  menulist, action=NULL, ...) {

   
  editPopupMenu <- getWidget(gmenu(menulist, popup=TRUE, action=action,container=obj, ...))
            
  RightClick <- function(x,y) # x and y are the mouse coordinates
    {
      V = getWidget(obj)
      rootx <- as.integer(tkwinfo("rootx",V))
      rooty <- as.integer(tkwinfo("rooty",V))
      xTxt <- as.integer(x)+rootx
      yTxt <- as.integer(y)+rooty
      tcl("tk_popup",editPopupMenu,xTxt,yTxt)
              }
  tkbind(getWidget(obj), "<Button-3>",RightClick)
}

setMethod("addpopupmenu",signature(obj="gWidgettcltk"),
          function(obj, menulist, action=NULL, ...) {
            .addpopupmenu(obj, obj@toolkit,menulist, action, ...)
          })
setMethod("addpopupmenu",signature(obj="tcltkObject"),
          function(obj, menulist, action=NULL, ...) {
            .addpopupmenu(obj, guiToolkit("tcltk"), menulist, action, ...)
          })


  
### 
setMethod(".addpopupmenu",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, menulist, action=NULL, ...) {
            .addPopupMenu(obj, menulist, action=action, ...)
})


## add3rdmousepopupmenu
setMethod("add3rdmousepopupmenu",signature(obj="gWidgettcltk"),
          function(obj, menulist, action=NULL, ...) {
            .add3rdmousepopupmenu(obj, obj@toolkit,menulist, action, ...)
          })

setMethod("add3rdmousepopupmenu",signature(obj="tcltkObject"),
          function(obj, menulist, action=NULL,...) {
            .add3rdmousepopupmenu(obj, guiToolkit("tcltk"),menulist, action,...)
          })

setMethod(".add3rdmousepopupmenu",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, menulist,action=NULL, ...) {
            .add3rdMousePopupMenu(obj,  menulist, action, ...)
          })
setMethod(".add3rdmousepopupmenu",
          signature(toolkit="guiWidgetsToolkittcltk",obj="tcltkObject"),
          function(obj, toolkit, menulist, action=NULL, ...) {
            .add3rdMousePopupMenu(obj, menulist, action, ...)
          })


## "dotmethods" defined in dnd.R
## adddropsource
setMethod("adddropsource",signature(obj="gWidgettcltk"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            .adddropsource(obj, obj@toolkit,targetType=targetType,
                           handler=handler, action=action, ...)
          })
setMethod("adddropsource",signature(obj="tcltkObject"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            .adddropsource(obj, guiToolkit("tcltk"),targetType=targetType,
                           handler=handler, action=action, ...)
          })

## adddropmotion
setMethod("adddropmotion",signature(obj="gWidgettcltk"),
          function(obj,  handler=NULL, action=NULL, ...) {
            .adddropmotion(obj, obj@toolkit,
                           handler=handler, action=action, ...)
          })
setMethod("adddropmotion",signature(obj="tcltkObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .adddropmotion(obj, guiToolkit("tcltk"),
                           handler=handler, action=action, ...)
          })

## adddroptarget
setMethod("adddroptarget",signature(obj="gWidgettcltk"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            .adddroptarget(obj, obj@toolkit,targetType=targetType,
                           handler=handler, action=action, ...)
          })

setMethod("adddroptarget",signature(obj="tcltkObject"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            .adddroptarget(obj, guiToolkit("tcltk"),targetType=targetType,
                           handler=handler, action=action, ...)
          })


## R Methods
setMethod("dim", "gWidgettcltk", function(x) .dim(x,x@toolkit))
setMethod(".dim",
          signature(toolkit="guiWidgetsToolkittcltk",x="gWidgettcltk"),
          function(x,toolkit) {
            cat("Define dim for x of class:")
            print(class(x))
            return(NULL)
})
setMethod("length", "gWidgettcltk", function(x) .length(x,x@toolkit))
setMethod(".length",
          signature(toolkit="guiWidgetsToolkittcltk",x="gWidgettcltk"),
          function(x,toolkit) {
            cat("Define length for x of class:")
            print(class(x))
            return(NULL)            
})
          
setMethod("dimnames", "gWidgettcltk", function(x) .dimnames(x,x@toolkit))
setReplaceMethod("dimnames",
                 signature(x="gWidgettcltk"),
                 function(x,value) {
                   .dimnames(x,x@toolkit) <- value
                   return(x)
                 })
## as of 2.5.0 this became primiive
if(as.numeric(R.Version()$major) <= 2 &
   as.numeric(R.Version()$minor) <= 4.1) {
  setGeneric("names")
  setGeneric("names<-")
}

setMethod("names", "gWidgettcltk", function(x) .names(x,x@toolkit))
setReplaceMethod("names",
                 signature(x="gWidgettcltk"),
                 function(x,value) {
                   .names(x,x@toolkit) <- value
                   return(x)
                 })
