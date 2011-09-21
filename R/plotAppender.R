# Add comment
# 
# Author: Andrie
#----------------------------------------------------------------------------------


#' Creates a closure for the temporary storage of braidPlot output.
#' 
#' Must be initialised, e.g. braidPlotAppender <- newAppender()
#' 
#' @keywords internal
newPlotAppender <- function() {
  plots <- NULL
  function(newPlot=NULL, newFilename="", width=4, height=3, reset=FALSE) {
    if(reset) plots <<- list()
    if(!is.null(newPlot)) {
      plots <<- c(plots, 
          list(list(
              plot=newPlot, 
              filename=newFilename, 
              width=width, 
              height=height)
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
#' @param braid A braid object
#' @keywords internal
braidAppendPlot <- function(braid, plot=NULL, filename="", width=4, height=3, reset=FALSE){
  eval(braid$plotAppender(plot, filename, width, height, reset), envir=parent.frame(n=1))
}

