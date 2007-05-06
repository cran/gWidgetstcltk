setClass("gMenutcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )
setClass("gMenuItemtcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )



## menulist is a list of lists with named components. Each named sub
## is a submenu.  a leaf consistis of handler= (required), lab

## put menu in group,
## a menubar is a map from a list into a menubar
## constructor
setMethod(".gmenu",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   menulist, 
                   popup = FALSE,
                   action = NULL,
                   container=NULL, ...) {
            
            force(toolkit)

            if(is(container,"logical") && container)
              container = gwindow()

            
            ##
            if(popup)
              tt <- getBlock(container)
            else
              tt = getTopParent(getBlock(container))
            topMenu <- tkmenu(tt, tearoff=FALSE)

            mapListToMenuBar(menulist, topMenu)

            
            ## unlike RGtk2 use removeall to make changes
            obj = new("gMenutcltk", block=tt, widget=topMenu,
              toolkit=toolkit,ID=getNewID())
  
            tag(obj, "menulist") <- menulist

            if(!popup)
              add(container, obj,...)

            invisible(obj)
          })


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gMenutcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            tag(obj, "menulist")
          })

## three cases for value: list, gMenutcltk, guiWidget push down
## make a menubar, then replace current -- isn't working for popup case
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gMenutcltk",
                           value="list"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   menulist = value            # value is a list
                   if(!is.list(menulist))
                     stop("value is not a menubar or a list")
                   
                   
                   mb = obj@widget
                   removeAllItems(mb)
                   mapListToMenuBar(menulist, mb)

                   parentWidget = mb$getParent()
                   parentWidget$validate()
                   
                   ## store for later?
                   tag(obj,"menulist") <- menulist

                   return(obj)
                 })

## get list, and then call previous
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gMenutcltk",
                           value="gMenutcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   .svalue(obj,toolkit, index, ...) <- svalue(value)
                   return(obj)
                 })

## call previous after getting list
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gMenutcltk",
                           value="guiWidget"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   .svalue(obj,toolkit,index, ...) <- svalue(value@widget)
                   return(obj)
                 })

## this is for adding a menu to a menu
setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk", obj="gMenutcltk", value="guiWidget"),
          function(obj, toolkit,  value, ...) {
            .add(obj, toolkit, value@widget)
          })

setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk", obj="gMenutcltk", value="gMenutcltk"),
          function(obj, toolkit,  value, ...) {
            orig.list = svalue(obj)
            add.list = svalue(value)
            new.list = c(orig.list, add.list)
            svalue(obj) <- new.list
          })


setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",
                    obj="gMenutcltk", value="list"),
          function(obj, toolkit,  value, ...) {
            mb = getWidget(obj)
            mapListToMenuBar(value, mb)
          })

###  This is for adding a gmenu to a container. In rjava, menus must
### be added to the toplevel frame. We 
setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",
                    obj="gWindowtcltk", value="gMenutcltk"),
          function(obj, toolkit,  value, ...) {
            tkconfigure(getBlock(value), menu=getWidget(value))
          })


setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk",
                    obj="gContainertcltk", value="gMenutcltk"),
          function(obj, toolkit,  value, ...) {
            tkconfigure(getBlock(value), menu=getWidget(value))
          })



## "wdget" is either a gMenu, list or just names to delete
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkittcltk", obj="gMenutcltk",
                    widget="guiWidget"),
          function(obj, toolkit, widget, ...) {
            .delete(obj,toolkit,widget@widget,...)
          })
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkittcltk", obj="gMenutcltk",
                    widget="gWidgettcltk"),
          function(obj, toolkit, widget, ...) {
            .delete(obj,toolkit,widget@widget, ...)
          })
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkittcltk", obj="gMenutcltk",
                    widget="gMenutcltk"),
          function(obj, toolkit, widget, ...) {
            .delete(obj,toolkit,svalue(widget), ...)
          })

setMethod(".delete",
          signature(toolkit="guiWidgetsToolkittcltk", obj="gMenutcltk",
                    widget="list"),
          function(obj, toolkit, widget, ...) {
            lst = widget                    # else assume its a character
            
            cur.list = svalue(obj)
            for(i in lst) {
              ## we delete *last* entry with this name, hence this awkwardness
              theNames = names(cur.list)
              if(i %in% theNames) {
                j = max(which(i == theNames))
                if(!is.null(cur.list[[j]])) cur.list[[j]] <- NULL
              }
            }
            ## now update menubar
            svalue(obj) <- cur.list
          })

## give vector notation
setMethod("[",
          signature(x="gMenutcltk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gMenutcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            lst = svalue(x)
            if(missing(i))
              return(lst)
            else
              return(lst[i])
          })

setReplaceMethod("[",
                 signature(x="gMenutcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gMenutcltk"),
          function(x, toolkit, i, j, ..., value) {
            lst = svalue(obj)
            theNames = names(lst)
            if(is.character(i))
              i = max(which(i %in% theNames))
            lst[[i]] <- value[[1]]
            theNames[i] = names(value)
            names(lst) = theNames
            svalue(obj) <- lst
            return(obj)
          })

##################################################
## helper functions

makeSubMenu = function(lst, label, parentMenu) {
  subMenu = tkmenu(parentMenu, tearoff = FALSE)
  tkadd(parentMenu,"cascade",label=label, menu = subMenu)

  sapply(names(lst),function(i)
         if(!is.null(lst[[i]]$handler)) {
           ## add item, what to do with $icon term>
           tkadd(subMenu,"command",label=i,command = function() {
             l = force(lst[[i]])
             h = list()
             h$action = l$action
             l$handler(h)
           })
         } else if(!is.null(lst[[i]]$separator)) {
           tkadd(subMenu,"separator")
         } else {
           ## a submenu
           makeSubMenu(lst[[i]], i, subMenu)
         }
         )
}

mapListToMenuBar = function(menulist, topMenu) {
  if(is.null(menulist[[1]]$handler)) {
    sapply(names(menulist), function(i)
           makeSubMenu(menulist[[i]],label=i,topMenu))
  } else {
    ## toplevel
    sapply(names(menulist), function(i) {
      tkadd(topMenu,"command",label=i,command = function() {
        l = force(lst[[i]])
        h = list()
        h$action = l$action
        l$handler(h)
      })
    })
  }
}


removeAllItems = function(topMenu) {
  tkdelete(topMenu,0,"end")
}
    
