\name{gWidgetstcltk-package}
\alias{gWidgetstcltk-package}
\alias{gWidgetstcltk}
\docType{package}
\title{
Toolkit implementation of gWidgets for tcltk
}
\description{
Port of gWidgets API to tcltk. The gWidgets API is an abstract,
lightweight means to interact with GUI toolkits. In this case, the tcltk
toolkit. 
}
\details{
  
This file documents differences between \pkg{gWidgetstcltk} and the
\pkg{gWidgets} API, which is documented both in the man pages for that
package and in its vignette.  The \pkg{gWidgetstcltk} package is not as
complete as gWidgetsRGtk2. This is due to limitations in the base
libraries implementing tcl/tk. This package was designed to work with the
limited version that comes with the standard Windows installation of R.

Notes on this implementation:

The primary difference in this interface, as opposed to that for
\pkg{RGtk2}, is that each widget requires a container when being
constructed. The container is given to the \code{container}
argument. The value may be the logical \code{TRUE} indicating that a
new window is constructed, or a container widget.

Other differences are that tcltk does not seem to have a markup language
like Pango for GTK or HTML for JAVA, as such the \code{markup} argument
for several widgets that is used to format text is not available. The
\code{font<-} method can substitute.

Until version 8.5 of tk, the basic tcltk installation did not include
several widgets that appear in other toolkits. For instance a grid
widget, a notebook widget, etc. This package now requires tk 8.5 to work
and R 2.12-0 or newer.
\cr

\bold{Containers:}
\cr

\code{gwindow()}: The \code{width=} and \code{height=} arguments refer
to the minimum window size, not the preferred default size. It is best
to set \code{visible=FALSE} for the constructor and then when the GUI
is layed out call \code{visible<-}. This will get the proper size for
the window. Otherwse, the \code{update} method can be called to resize
the window to accomodate the child widgets.  

The methods \code{addHandlerDestroy} and \code{addHandlerUnrealize} can
only add one handler, new ones overwrite the old. These handlers can
also not be removed. 
\cr

\code{ggroup()} has the \code{expand=} \code{fill=} and \code{anchor=}
arguments. If \code{expand=TRUE} the widget is allocated as much space as
possible. (The default is \code{expand=FALSE}, unless the option
\code{"gw:tcltkDefaultExpand"} overrides this.) When a widget has
expand, then the widget may stretch to fill the expanding space (in
tcltk, all widgets that have expand=TRUE are allocated evenly any
additional space). The fill may be TRUE (or both), FALSE, or "x" and
"y".  The \code{anchor=} argument adjusts a widget left or right, up
or down, within its space. Only one component works at a time for the
anchor argument. In a horizontal box, only the y component can be used
to move a value up or down. In a vertical box, only the x component
can be used to move a value left or right. The default is c(-1,0) so
that horizontal layouts are towards the middle, and vertical layouts
towards the left. (This can be overridden:
\code{options("gw:tcltkDefaultAnchor"=c(0,0))}, say.)

The \code{use.scrollwindows} feature is now implemented. (but seems buggy)

The \code{addSpring} method only works if the parent container is set to expand.
\cr


\code{gframe()} The \code{markup} argument is ignored. Use \code{font<-} to give the title
markup. 
\cr


\code{gexpandgroup()} Works as expected, although sizing issues may frustrate.


\code{gdfnotebook()} Works with the addition of ttknotebook.
The \code{add} method, which is used to add pages, is called when the
notebook is given as a container during the construction of a
widget. Hence, to add a page something like this is done:
\preformatted{
  nb <- gnotebook(cont=gwindow("Notebook example"))
  gbutton("Page 1", cont=nb, label = "tab1")
  glabel("Page 2", cont=nb, label = "tab2")
  gedit("Page 3", cont=nb, label = "tab3")
}


\code{glayout()} has two additional arguments: \code{expand=TRUE} is like \code{expand=} for \code{ggroup()}, in
that the attached widget expands to fill the possible space in the
container. If this isn't given the \code{anchor=} argument can be used to
adjust the location of the widget withing the cell. A value of c(-1,1)
is the lower left, c(-1,1) the upper left (the default), c(1,-1) the
lower right, and c(1,1) the upper right. The value 0 for either is also
possible.
\cr


\code{gpanedgroup()} The constructor is called with no widgets. To add a
widget to the paned group the paned group is passed as a container, as
in
\preformatted{
  pg <- gpanedgroup(container=gwindow("example"), horizontal = FALSE)
  b1 = gbutton("button 1", container=pg)
  b2 = gbutton("button 2", container=pg)
}

The paned window can be adjusted manually or using the \code{svalue}
method. The \code{svalue} method uses the current window size. If the
widget is not realized, the method will not work as expected, so call
this after showing the GUI. The \code{delete} method can be used to
delete a widget. It may be added back with the \code{add} method.  \cr


\bold{The basic widgets or components:} (These are also known as controls)
\cr

\code{gbutton()} mostly works. The button won't resize to take up all
the possible space for a widget, even if \code{expand=TRUE} is given.

\code{gcalendar()} is a hack.
\cr

\code{gcheckbox()} works as advertised, \code{use.togglebutton} implemented.
\cr

\code{gcheckboxgroup()} works as advertised, except the \code{use.table} argument is ignored. One can now resize the list.
\cr

\code{gcombobox()} Works as expected, although no icons or tooltips are available.
\cr


\code{gdf()} is implemented if the user has installed the
\code{tktable} package in Tcl. This is an additional download from
\url{tktable.sourceforge.net}. Most of the code comes second hand from
\pkg{tcltk2}'s \code{dfedit} function.  \cr

\code{gedit()}: The widget does not resize automatically. Set the
width with \code{width=} at time of construction or with
\code{size<-}. There is now type ahead support, although the pop-down
menu only pops down, so don't use near the bottom of a screen ;) The
hidden argument \code{init_msg} can be used to place an initial
message for the event there is no text in the box.  \cr

\code{gfilebrowse()} works.  \cr

\code{ggraphics()} Not implemented. The \pkg{tkrplot} package could be
used in some way, but this does not provide a fully embeddable
graphics device. The \pkg{tkrplot} package provides a means to create
interactive graphics with \pkg{tcltk}. This is not a device, so ins't
directly supported. However, a \code{ggroup} object can be used as a
parent container. Just call \code{getToolkitWidget} on the object
first: \cr
\preformatted{
g <- ggroup(cont=gwindow())
l <- tkrplot(getToolkitWidget(g), function() hist(rnorm(100)))
add(g, l)
}


\code{ghelp()} Works as advertised. Uses a popup menu instead of a
notebook, as gWidgetsRGtk2. Best to just use \pkg{helpr} though.  \cr

\code{gimage()} Only works with gif and pnm files, as the underlying
tcltk widget only uses these by default.  \cr

\code{glabel()} No markup available. Use \code{font<-} instead.  \cr

\code{gmenu()} adds only to the top window, not any container. This is
a tcltk limitation. Use a popupmenu instead.

Under Mac OS X, menus display in the top menu bar area, not in the
parent window.  \cr

\code{gtoolbar()} A hack made from a \code{ggroup} object that packs
in \code{gbutton} instances. The buttons take up alot of screen real
estate, on the default Aqua them of OS X the buttons are rounded, so
the toolbar looks odd, ...  \cr


\code{gaction()} is implemented for buttons, menubars and
toolbars. The \code{key.accel} component is now implemented but one
must pass in a parent argument (The binding is to the top-level window
containing the parent). \cr

\code{gradio()} has an extra argument \code{coerce.with=}, as
otherwise it would treat everything as a character vector. It tries to
guess when instantiated, if not explicitly given. One can now resize
the number of items to select from.  \cr

\code{gseparator()} works as expected but must be in a container with
\code{expand=TRUE}.

\code{gslider()} now works with non-integer steps. If first argument
\code{from} is a vector it will slide over those values after
sorting. This uses a themed widget which might be buggy under some
styles.  
\cr

\code{gspinbutton()} Works as expected. The change handler responds to
clicks of the arrows or to the return key in the text area. Unless one
has a new Tk version, this is a non-themed widget and can look a bit
odd.\cr

\code{gstatusbar()} A hack. Just a \code{ggroup()} instance in
disguise. By default it must have a gwindow instance for a parent
container. If the hidden argument \code{not.toplevel=TRUE} is
specified, a \code{ggroup} container may be used.  \cr

\code{gtable()} This is built on the underlying tree widget. It is not
ideal, but avoids needing to have a separate library (eg. Tktable)
installed.  If the hidden argument \code{round} is passed to the
constructor, this will be passed to the \code{format} function's
\code{digits} argument when formatting numeric values for display.
\cr

Sizing is an issue. There may be a bug in the widget regarding
horizontal scrolling (for Mac OS X anyways, where this is being
developed), or more likely something is just coded wrong.  There is
some Tk code for "autoscrolling" that works (with an idiosyncrasy) so
that the initial size of the widget is correct, but only when this
size is set via arguments \code{width} and \code{height} passed to the
constructor -- not with the \code{size<-} method. This feature is not
on by default, as when it is used any widgets on the right of the
table are not shown in the initial window, and are only exposed by
resizing the window. If you want to try it, pass in the hidden
argument \code{do.autoscroll=TRUE}.  However, the \code{size<-} method
has another use. It can also take a list for value. This list has
optional components \code{width}, \code{height}, \code{columnWidths}
(to specify each column width individually), and \code{noRowsVisible}
(to specify height by number of rows, not pixels).  \cr

\code{gtext()} The \code{size<-} method can be used to resize the
widget. The initial default size is quite large. This method guesses at
the converstion from pixels to characters (width) and lines of text
(height) used by the underlying widget. The \code{svalue()} method
returns all the text unless some text is selected by the mouse and :
\code{index=TRUE} in which case the indices of the selected text are
returned or \code{drop=TRUE} in which case only the selected text is
returned.  \cr


\code{gtree()} Implemented using ttktreeview. It is slow however, so
use on smaller trees only. Has same issues with scrollbars as
\code{gtable}.  \cr



\bold{Compound components:}
\cr

\code{gcommandline()} is implemented, but could definitely be
improved. 
\cr

\code{ghelpbrowser()} just calls \code{ghelp}
\cr

\code{ggenericwidget()} Some kinks need ironing out when the main
variable is a formula.  \cr

\code{gdfnotebook()} Not implemented.\cr

\code{ggraphicsnotebook()} No \code{ggraphics} so no notebook.
\cr

\code{gvarbrowser()} Uses a tree to show heirarchical structure of
workspace. Does not poll to update workspace. It does reread workspace
when Filter by: value is changed.  \cr

\bold{Dialogs:} (These are modal, hence they have no methods
(basically).)  \cr 

\code{gfile()} works as advertised.  \cr

\code{galert()} works.\cr

\code{gmessage()} works.
\cr

\code{gconfirm()} works.
\cr

\code{ginput()} works.
\cr 

\code{gbasicdialog()} is implemented. It it is a container. When the
\code{visible(obj,TRUE)} command is issued, the container is shown and
made modal.  

\preformatted{
dlg <- gbasicdialog("A modal dialog", handler=function(h,...) print("hi"))
l = glabel("some widget in the dialog", cont=dlg)
visible(dlg, set=TRUE)
}



\bold{Handlers:}
\cr


Handlers were rewritten so that one can have more than one handler per
signal. The \code{blockHandler}, \code{unblockHandler} and
\code{removeHandler} methods are now working.  Handler code different
for those widgets which use an R5 backend and those which don't, but
the end user shouldn't notice. (Well, if you do let me know!)\cr

The \code{addHandlerBlur} method should be called when a widget loses
focuses, but here is called whenever a widget loses focus \emph{and}
whenever the mouse leaves the widget. This can mean the handler is
called twice. If you don'l like that, you can add the callback through
\code{addHandler(obj, signal, handler)} where \code{signal} is
\code{<FocusOut>} or \code{<Leave>}.  \cr

\code{adddroptarget()}, \code{adddropsource()}, and
\code{adddropmotion} work for tcltk widgets. The cursor changes to a
funny looking cursor, but nothing resembling a drag and drop
cursor. One was chosen from the standard cursors. Dragging from other
applications is not supported.



}
\author{
  
  John Verzani. Several code segments were inspired by the examples of
  Wettenhall and the Dalgaard article referenced below. The drag and
  drop code was modified from code at
  \url{http://wiki.tcl.tk/416}. Icons were "borrowed" from several
  places: the scigraphica project, KDE, and elsewhere.

Maintainer: John Verzani <gwidgetsrgtk@gmail.com>
}
\references{

  Peter Dalgaard's RNews article on the tcltk pagkage
  \url{http://cran.r-project.org/doc/Rnews/Rnews_2001-3.pdf}; Several
  examples on
  \url{http://bioinf.wehi.edu.au/~wettenhall/RTclTkExamples/}; The PERL
  documentation was useful:
  \url{http://search.cpan.org/~ni-s/Tk-804.027/}, although this is for
  the most part a translation of the TK documentation.

  For a package somewhat similar in intent see Bowman, Crawford,
  Alexander, and Bowman's \pkg{rpanel} package:
  \url{http://www.jstatsoft.org/v17/i09/v17i09.pdf} or the \pkg{tkWidgets}
  package of Zhang \url{www.bioconductor.org}.

  The \pkg{fgui} package provides a similar functionality as \code{ggenericwidget} in a standalone package.
  
}
\keyword{ package }
\examples{
\dontrun{
##
options(guiToolkit="tcltk")
## select CRAN miror example
setMirror = function(URL) {
   repos = getOption("repos")
   repos["CRAN"] = gsub("/$", "", URL)
   options(repos = repos)
}

  win = gwindow("Select a CRAN mirror")
  tbl = gtable(utils:::getCRANmirrors(), 
  container=win,
  chosencol=4, 
  handler = function(h,...) {
    URL = svalue(h$obj)
    setMirror(URL)
    dispose(win)
  })
##
}
}
