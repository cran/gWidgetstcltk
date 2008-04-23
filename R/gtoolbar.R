## gtoolbar, similar to gmenu
## need to incorporate delete/add methods as in imenu


setClass("gToolbartcltk",
         representation = representation("gComponenttcltk",
           style="character"),
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

## turn a list into a uimgr object
setMethod(".gtoolbar",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   toolbarlist,
                   style = c("both","icons","text","both-horiz"),
                   action=NULL,
                   container=NULL, ...) {

            force(toolkit)

            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }


            style = match.arg(style)
            tkstyle <- c("both"="top",
                         "icons"="image",
                         "text"="text",
                         "both-horiz"="left")

            tt <- getBlock(container)
            tb <- ttkframe(tt)


            
            toolbar = ggroup(horizontal=TRUE, cont=container, expand=TRUE)
            .mapListToToolBar(tb, toolbarlist, tkstyle[style])

            obj = new("gToolbartcltk",block=tb, widget=tb,
              toolkit=toolkit, ID=getNewID(),e = new.env(),
              style=style)

            tag(obj,"toolbarlist") <- toolbarlist

            add(container, obj, ...)
            invisible(obj)
  
          })


## helpers
.addToolbarButton <- function(tb, style, text=NULL, icon=NULL,handler=NULL, action=NULL) {

  ## get icon
  if(!is.null(icon)) {
    file <- findTkIcon(icon)
    icon <- tcl("image","create","photo",file=file)
  }
  
  ## make a button put in icon if there
  b <- ttkbutton(tb, image=icon, text=text, compound=style)

  ## add in handler
  handler = force(handler)              # need to force so scoping works in this call
  if(!is.null(handler)) {
    tkbind(b,"<Button-1>", function(...) {
      h = list(obj=b, action=action)
      handler(h,...)
    })
  }

  slaves <- tclvalue(tcl("grid","slaves",tb))
  slaves <- unlist(strsplit(slaves," "))
  n <- length(slaves)
  tkgrid(b, row=0, column=n, sticky="ns")
  
#  tkpack(b, side="left",anchor="w",expand=TRUE,fill="y")

}


.mapListToToolBar = function(tb, lst, style) {
  ## list is simple compared to menubar
  for(i in names(lst)) {
    if(!is.null(lst[[i]]$separator)) {
      ## add separator
      gseparator(horizontal=FALSE, cont=tb)
    } else if(!is.null(lst[[i]]$handler)) {
      ## how to decide there are no text parts?
      .addToolbarButton(tb, style, i, lst[[i]]$icon, lst[[i]]$handler, lst[[i]]$action)
    }
  }
}


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gToolbartcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            tag(obj, "toolbarlist")
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gToolbartcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   if(!is.list(value)) 
                     stop("A toolbar requires a list to define it.")

                   toolbar = obj@widget
                   ## delete from toolbar
                   n = length(tag(obj,"toolbarlist"))

                   ## how to delete from group
                   gwCat(gettext("No method to delete toolbar components\n"))
                   
                   .mapListToToolBar(toolbar, value, obj@style)

                   tag(obj,"toolbarlist") <- value
                   
                   ##  all done
                   return(obj)
                 })

## returns list, or part of list
setMethod("[",
          signature(x="gToolbartcltk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gToolbartcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            lst = tag(x,"toolbarlist")
            if(missing(i))
              return(lst)
            else
              return(lst[[i]])
          })

setReplaceMethod("[",
                 signature(x="gToolbartcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gToolbartcltk"),
          function(x, toolkit, i, j, ..., value) {
            if(!is.list(value))
              stop("assignment must be a list defining a (part) of a toolbar.")
            lst = tag(x,"toolbarlist")
            if(missing(i))
              lst = value
            else
              lst[[i]] = value
            
            svalue(x) <- lst
            
            return(x)
          })


setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gToolbartcltk", value="list"),
          function(obj, toolkit, value,  ...) {
            svalue(obj) <- c(svalue(obj), value)
          })

## (from gmenu)
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gToolbartcltk"),
          function(obj, toolkit, widget,  ...) {
            ## widget can be gToolBar or a list
            if(is.character(widget)) {
              lst = widget                    # else assume its a character
            } else if(is(widget,"gComponenttcltk")) {
              lst = svalue(widget)
              lst = names(lst)
            } else if(is.list(widget)) {
              lst = names(widget)
            } else {
              warning("Must be either a vector of names, a list, or a gToolbar instance")
              return()
            }
            
            cur.list = svalue(obj)             
            for(i in lst) {
              ## we delete *last* entry with this name, hence this awkwardness
              theNames = names(cur.list)
              if(i %in% theNames) {
                j = max(which(i == theNames))
                if(!is.null(cur.list[[j]])) cur.list[[j]] <- NULL
              }
            }
            ## now update toolbar
            svalue(obj) <- cur.list
          })

### no method to set style, use tag(obj,"style")<-"theStyle" instead
