setClass("gTreetcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

## offspring takes two argument

## map a list to a tree
setMethod(".gtree",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   offspring = NULL,
                   hasOffspring = NULL,                 # for defining offspring. FUN
                                        # of children only.
                   offspring.data = NULL,
                   col.types = NULL, # data frame with logical
                   icon.FUN = NULL,                      # return stock name --called
                                        # on offspring, returns a
                                        # vector of length nrow
                   chosencol = 1,
                   multiple = FALSE,
                   handler = NULL,
                   action=NULL,
                   container=NULL,
                   ...
                   ) {

            force(toolkit)
            return(glabel("gtree not implemented\n", container=container)@widget)
          })

