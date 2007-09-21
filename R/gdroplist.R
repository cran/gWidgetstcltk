## Poor mans version of this, do it yourself version


## editable has entry widget that can be edited
setClass("gDroplisttcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

setMethod(".gdroplist",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   items, selected = 1, # use 0 for blank
                   editable=FALSE,
                   coerce.with = NULL,
                   handler=NULL, action=NULL,
                   container=NULL,
                   ...               # do.quote = TRUE for quote of answer
                   ) {

            force(toolkit)

            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }

            ## items could be a data frame, in which case
            ## row 1 has the data, row 2 the icon name

            if(inherits(items,"data.frame")) {
              items <- items[,1, drop=TRUE]
            }
            
             ## items must be a vector here
            items = as.vector(items)              # undoes factor
            items = unique(items)                 # unique
            
            theArgs = list(...)
            ## keep this, but don't advertise
            if(!is.null(theArgs$do.quote)) {
              coerce.with = function(x) paste("'",x,"'",sep="") # no space
            }
            
            ## we hack together this, otherwise we need to call in a new package using tclRequire, meaning more hassle for new users

            tt = getBlock(container)
            gp = tkframe(tt)
            tkconfigure(gp,padx=0,pady=0, relief="groove",borderwidth=2)
            tkpack(gp, side="left", fill="x")

            txtVar = tclVar("")

            if(editable) {
              txt = tkentry(gp, width=max(10,max(nchar(items))),
                textvariable=txtVar)
              if(selected >0) tclvalue(txtVar) <- items[selected]
            } else {
              txt = tklabel(gp, text = "   ")
              if(selected > 0) tkconfigure(txt, text = items[selected])
            }
            
            ## icon for expanding. Could be improved
            f = system.file("images/1downarrow.gif",
              package="gWidgets")
            imageID = paste(txt$ID,"combo",sep="")
            tcl("image","create","photo",imageID,file=f)
            
            V <- tklabel(gp,image=imageID)

            ## get Layout
            tkpack(txt, fill="x", side="left",anchor="e")
            line <- tkframe(gp, background = "black")
            tkpack(line, expand=TRUE, fill="y", side="left", anchor="e")
            tkpack(V, side="left",anchor="e")
            
#            tkgrid(V,txt)
#            tkgrid.configure(txt,sticky="w")
#            tkgrid.configure(V, sticky="e")

            ## could have used gWidgets here, didn't
            editPopupMenu <- tkmenu(txt, tearoff=FALSE)
            
            RightClick <- function(x,y) # x and y are the mouse coordinates
              {
                rootx <- as.integer(tkwinfo("rootx",V))
                rooty <- as.integer(tkwinfo("rooty",V))
                xTxt <- as.integer(x)+rootx
                yTxt <- as.integer(y)+rooty
                tcl("tk_popup",editPopupMenu,xTxt,yTxt)
              }
            tkbind(V, "<Button-1>",RightClick)

            
            obj = new("gDroplisttcltk",block=gp,widget=txt, toolkit=toolkit,ID=getNewID())

            tag(obj,"coerce.with") <- coerce.with
            tag(obj,"editable") <- editable
            tag(obj,"popupMenu") <- editPopupMenu
            tag(obj,"txtVar") <- txtVar
            tag(obj,"items") <- items
            tag(obj,"itemWidth") <- max(nchar(items))

            
            sapply(items, function(i) addItemToPopupMenu(obj,i))
            
            addDropTarget(obj, handler = function(h,...)
                           svalue(obj) <- h$dropdata)

            
            add(container, obj, ...)
            
            if (!is.null(handler)) {
              id <- addhandlerchanged(obj, handler, action)
              tag(obj, "handler.id") <- id
            }
            
            invisible(obj)
          })
          
### methods
## value is for getting/setting the selected value
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gDroplisttcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {

            if(tag(obj,"editable")) {
              ## a text entry
              val = tclvalue(tag(obj,"txtVar"))
            } else {
             ## a label
              val = paste(as.character(tkcget(getWidget(obj),"-text")),sep=" ",collapse=" ")
            }
            if(length(val) == 0) val = ""

            
            ## add in an as.numeric flag, getwidget when editable
            theArgs = list(...)         # deprecated
            coerce.with = tag(obj, "coerce.with")
            editable = tag(obj,"editable")

            if(!is.null(index)) index=as.logical(index)
            
            
            ## return index if asked for
            if(is.null(editable) || editable == FALSE) {
              if(!is.null(index) && index==TRUE) {
                return(match(val,tag(obj,"items"))) 
              }
            }

            ## do we coerce return value?
            if(is.null(coerce.with))
              return(val)
            else if(is.function(coerce.with))
              return(coerce.with(val))
            else if(is.character(coerce.with))
              return(do.call(coerce.with,list(val)))
            else
              return(val)               # what else?
            
          })

## set the displayed value to value
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gDroplisttcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   theArgs = list(...)

                   n = length(obj)
                   if(is.null(index)) index = FALSE
                   index = as.logical(index)

                   ##  if editable do differently
                   ## editable not implented
                   editable = tag(obj,"editable")

                   if(!is.null(editable) && editable) {
                     ## text box
                     if(index == TRUE)  {
                       ## set text to value
                       tclvalue(tag(obj,"txtVar")) <-
                         tag(obj,"items")[value]
                     } else {
                       tclvalue(tag(obj,"txtVar")) <-
                         value
                     }
                   } else {
                     ## not editable
                     items = tag(obj,"items")
                     maxSize = max(nchar(items))
                     if(index) {
                       tkconfigure(obj@widget,text=items[value])
                     } else {
                       if(any(value == items)) {
                         ind = match(value,items)
                         svalue(obj, index=TRUE) <- ind #recurse
                       } else {
                         ## add to end
                         item = as.character(value)
                         items = c(items,item)
                         tag(obj,"items") <- items
                         tag(obj,"itemWidth") <- max(nchar(items))
                         svalue(obj,index=TRUE) <- length(obj)
                       }
                     }
                   }

                   tkevent.generate(getWidget(obj),"<<ValueChanged>>")
                   
                   return(obj)
                 })

setMethod("length",
          signature(x="gDroplisttcltk"),
          function(x) {
            .length(x, x@toolkit)
          })
setMethod(".length",
          signature(toolkit="guiWidgetsToolkittcltk",x="gDroplisttcltk"),
          function(x, toolkit) {
            return(length(tag(x,"items")))
          })


## the methods [ and [<- refer to the pre-defined values in the drop list.
## [
setMethod("[",
          signature(x="gDroplisttcltk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gDroplisttcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {

            n = length(x)               # no. items
            if(n == 0)
              return(NA)
            
            items = tag(x,"items")
            
            if(missing(i))
              return(items)
            else
              return(items[i])
          })


## replaces the values in droplist
## values is a vector of values -- not a dataframe
#set.values.gDropList = function(obj, values, ...) {
setReplaceMethod("[",
                 signature(x="gDroplisttcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gDroplisttcltk"),
          function(x, toolkit, i, j, ..., value) {


            if(missing(i)) {
              if(length(x) > 0)
                tkdelete(tag(x,"popupMenu"),0,"end")

              ## add one by one using addItem
              if(length(value) > 0) 
                sapply(value, function(i) addItemToPopupMenu(x,i))
              tag(x,"items") <- value
              tag(x,"itemWidth") <- max(5,max(nchar(value)))
            } else {
              items = x[]
              items[i] <- value
              x[] <- items ## recurse
            }

            return(x)
          })

###################################################
  
### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gDroplisttcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            widget = getWidget(obj)
            tkbind(getWidget(obj),"<<ValueChanged>>", function(...) {
              h=list(); h$obj=obj; h$action=action
              handler(h)
            })

            if(tag(obj,"editable"))
              .addHandler(obj, toolkit, signal="<Return>", handler, action)

          })

setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gDroplisttcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerchanged(obj,"changed",handler,action)
          })


######
### private methods
addItemToPopupMenu = function(obj,item) {

  copyText <- function(x) {
    if(tag(obj,"editable")) {
      tclvalue(tag(obj,"txtVar")) <- x
    } else {
      tkconfigure(obj@widget, text=.padString(x,tag(obj,"itemWidth")))
    }

    tkevent.generate(getWidget(obj),"<<ValueChanged>>")
  }

  tkadd(tag(obj,"popupMenu"), "command", label=item,
            command=function() copyText(item))
            

}

removeAllItems = function(obj) {
  mb = tag(obj,"popupMenu")
  tkdelete(mb,0,"end")
}

.padString = function(string, desiredChars) {
  l = nchar(string)
  if(l < desiredChars)
    string = paste(string,paste(rep(" ",desiredChars - l),collapse=""), sep="")
      
  return(string)
}
