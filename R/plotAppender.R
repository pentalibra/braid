# Add comment
# 
# Author: Andrie
#----------------------------------------------------------------------------------

### Define a class of functions with a static variable (i)
### Important: This must be initialised before use


#' Creates a counter. 
#' 
#' Must be initialised, e.g. braid_counter <- newCounter()
#' 
#' @keywords internal
#' @param start The starting value of the counter, defaults to 1
newCounter <- function(start=1) {
  i <- start - 1
# i <- i - 1
  function() {
    # do something useful, then ...
    i <<- i + 1
    i
  }
}

#' Creates a closure for the temporary storage of braidWrite output.
#' 
#' Must be initialised, e.g. braid_appender <- newAppender()
#' 
#' @keywords internal
newAppender <- function() {
  text <- ""
  function(new_text=NULL, reset=FALSE) {
    if(reset) text <<- ""
    if(!is.null(new_text)) text <<- paste(text, new_text, sep="")
    invisible(text)
  }
}


#------------------------------------------------------------------------------

#' Updates braid counter.
#' 
#' Each braid object has a static counter that can be used to generated automatic filenames. This function increments the counter and returns the result.
#' 
#' @param braid A braid object
#' @keywords internal
braidIncCounter <- function(braid){
  eval(braid$counter(), envir=parent.frame(n=3))
}

#------------------------------------------------------------------------------

#' Appends braid text.
#' 
#' The output from braidWrite is stored in a character vector, for later dumping to file.
#' 
#' @param braid A braid object
#' @keywords internal
braidAppendText <- function(braid, text="", reset=FALSE){
  eval(braid$appender(text, reset), envir=parent.frame(n=1))
}







#' Saves braid plot to pdf.
#' 
#' Saves braid plot to pdf.  Uses either ggsave() or pdf(), depending on the class of plot.  Supports plots of class ggsave and trellis (i.e. lattice plots).
#' 
#' @param braid A braid object
#' @param plotcode A plot object (either ggplot or lattice)
#' @param filename Filename without path. The path is obtained from the braid defaults
#' @param width Width in inches
#' @param height Height in inches
#' @param Qid Optional identifying text that is used to print a message in the event the plot fails
#' @export
braidPlot <- function(braid, plotcode, filename=braidFilename(braid), 
    width=braid$defaultPlotSize[1], 
    height=braid$defaultPlotSize[2], Qid=NA){
  braidWrite(braid, paste("  \\PlaceGraph{", "graphics", "/", filename, "}", sep=""))
  braidAppendPlot(braid, plotcode, filename, width, height, Qid)
  invisible(NULL)
}

#------------------------------------------------------------------------------

#' Creates a closure for the temporary storage of braidPlot output.
#' 
#' Must be initialised, e.g. braidPlotAppender <- newAppender()
#' 
#' @keywords internal
newPlotAppender <- function() {
  plots <- NULL
  function(newPlot=NULL, newFilename="", width=4, height=3, Qid=NA, reset=FALSE) {
    if(reset) plots <<- list()
    if(!is.null(newPlot)) {
      plots <<- c(plots, 
          list(list(
              plotcode=newPlot, 
              filename=newFilename, 
              width=width, 
              height=height,
              Qid=Qid)
      ))
    }
    invisible(plots)
  }
}

#----------------------------------------------------------------------------------

#' Appends braid plot.
#' 
#' The output from braidWrite is stored in a character vector, for later dumping to file.
#' 
#' @inheritParams braidPlot
#' @keywords internal
braidAppendPlot <- function(braid, plot=NULL, filename="", width=4, height=3, Qid=NA, reset=FALSE){
  eval(braid$plotAppender(plot, filename, width, height, Qid, reset), envir=parent.frame(n=1))
}

