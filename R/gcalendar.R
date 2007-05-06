## add calendar widget: shoule I have gcalendar, gcalendarbrowser?
## no handler function, can add to entry object with addhandler

## setClass("gCalendartcltk",
##          representation = representation("gComponenttcltk",
##            format="character"),
##          contains="gEdittcltk",
##          prototype=prototype(new("gEdittcltk"))
##          )


setMethod(".gcalendar",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   text="",
                   format="%Y-%m-%d",
                   handler = NULL, action=NULL,
                   container=NULL,...) {

            force(toolkit)

            if(text == "" && format != "")
              text = format(Sys.Date(), format)

            text = as.character(text)
            
            ## use a text widget
            if(format == "") {
              obj = gedit(text, cont=container, ...)
            } else {
              obj = gedit(text,container=container, coerce.with = function(x) format(as.Date(x,format=format)),
                ...)
            }
            return(obj@widget)          # drop down to tcltk widget
          })

## gcalendar is a gedit instance. It inherits those methods.
