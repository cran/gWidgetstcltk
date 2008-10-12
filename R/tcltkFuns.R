## return tk widget from obj
getWidget = function(obj) {
  if(is(obj,"tkwin")) return(obj)

  if(is(obj,"gWidgettcltk"))
    return(getWidget(obj@widget))
  else if(is(obj,"guiWidget"))
    return(getWidget(obj@widget))
  else
    return(NA)
}

getBlock = function(obj) {

  if(is(obj,"tkwin")) return(obj)
  if(is(obj,"gWidgettcltk"))
    return(getBlock(obj@block))
  else if(is(obj,"guiWidget"))
    return(getBlock(obj@widget))
  else
    return(NA)
}


getTopParent = function(tkobj) {
  ## in env is parent variable if present
  ans <- NULL
  
  while(is.null(ans)) {
    if(tkobj$env$parent$ID=="")
      ans <- tkobj
    else tkobj <- tkobj$env$parent
  }
  return(ans)
}


setMethod(".getToolkitWidget",
          signature(obj="gWidgettcltk", toolkit="guiWidgetsToolkittcltk"),
          function(obj, toolkit) getWidget(obj))



## Does the top level window exists
windowExists = function(obj) {
  win = getTopParent(getWidget(obj))
  as.logical(tkwinfo("exists", win))
}

findTkIcon <- function(i) {
  if(is.null(i)) return("")
  
  stock <- getStockIcons()
  gwi <- system.file(paste("image/",i,".png",sep=""), package="gWidgetstcltk")
  if(is.null(i) || is.na(i) || i == "")
    val <- ""
  else if(file.exists(i))
    val <- i
  else if (file.exists(gwi))
    val <- gwi
  else
    val <- stock[[i, exact=TRUE]]
  ## what to return
  if(is.null(val))
    return("")
  else
    return(val)
}
