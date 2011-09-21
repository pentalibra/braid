# Add comment
# 
# Author: Andrie
#----------------------------------------------------------------------------------


#------------------------------------------------------------------------------
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




