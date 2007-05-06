.onLoad <- function(libname,pkgname,...) {
  require(methods)
  require(tcltk)
  oldClasses =c("tkwin")
  setClass("tcltkObject")
  sapply(oldClasses, function(i) {
    setOldClass(i)
    setIs(i,"tcltkObject")
  })
}
         

       

.onAttach <- function(...) {
   #  loadGWidgetIcons()
}
