## gGrid cover gDf and gTable
setClass("gGridtcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )
setClass("gDftcltk",
         contains="gGridtcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

##################################################
### Gdf 

## * Colors not implemented
## * This is **SLOW** for larger data sets (Cars93 large)


## constructor for editing a data frame
setMethod(".gdf",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   items = NULL,
                   name = deparse(substitute(items)),
                   do.subset = FALSE,
                   container=NULL,...)  {

            force(toolkit)

            return(glabel("No gdf widget available", cont=container)@widget)
          })


##
####################################################



## gWidget methods
setReplaceMethod(".size",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gGridtcltk"),
          function(obj, toolkit,  ..., value) {
            return(obj)
          })



## data frame methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gGridtcltk"),
          function(obj, toolkit, index=NULL, drop=NULL,...) {

          })
          
          
## set by index value selected value
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gGridtcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   return(obj)
                 })


## refers to the entire data frame
## index returned by svalue(index=T) works here
setMethod("[",
          signature(x="gGridtcltk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j,..., drop=drop)
          })

setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gGridtcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {

          })

## [<-
setReplaceMethod("[",
                 signature(x="gGridtcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j,...) <- value
                   return(x)
                 })


setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gGridtcltk"),
          function(x, toolkit, i, j, ..., value) {
            
          })
                 
## first column is the visible row


## data frame like
setMethod(".dim", 
          signature(toolkit="guiWidgetsToolkittcltk",x="gGridtcltk"),
          function(x,toolkit) {
          })

## no dimnames for gGrid, only names
setMethod(".dimnames",
          signature(toolkit="guiWidgetsToolkittcltk",x="gGridtcltk"),
          function(x,toolkit) {
          })
          

setReplaceMethod(".dimnames",
                 signature(toolkit="guiWidgetsToolkittcltk",x="gDftcltk"),
                 function(x, toolkit,  value) {
                 })

setMethod(".length",
          signature(toolkit="guiWidgetsToolkittcltk",x="gGridtcltk"),
          function(x,toolkit) return(dim(x)[2]))


setMethod(".names",
          signature(toolkit="guiWidgetsToolkittcltk",x="gGridtcltk"),
          function(x, toolkit) {
          })


setReplaceMethod(".names",
                 signature(toolkit="guiWidgetsToolkittcltk",x="gGridtcltk"),
                 function(x, toolkit, value) {
                   return(x)
                 })




