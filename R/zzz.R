.onLoad <- function(libname,pkgname,...) {
  require(methods)
  require(tcltk)

  ## version check
  if(as.numeric(.Tcl("info tclversion")) < 8.5) {
    cat("\n\n *** gWidgetstcltk needs tcl/tk version 8.5 or newer ***\n\n")
  }
  
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
