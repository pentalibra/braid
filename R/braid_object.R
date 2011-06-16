# TODO: Add comment
# 
# Author: Andrie
###############################################################################


#' Creates object of class braid.
#' 
#' A braid object describes the data and meta-data in the survey that will be analysed by the analysis and reporting functions in the braid package.
#' 
#' @param path_latex Path where latex will be saved
#' @param path_graphics Path where graphics files will be saved
#' @param output_filename Filename where latex output will be saved
#' @param counter_start The starting number for a counter used to store graphs, 
#' @param default_plot_size Plot size in inches, e.g. c(4, 3)
#' @param dpi Dots per inch, passed to ggsave()
#' @return A list object of class braid
#' @export
as.braid <- function(
    path_latex = Sys.getenv()["TEMP"],
    path_graphics = path_latex,
    output_filename = file.path(path_latex, "braid.tex"),
    counter_start = 1,
    default_plot_size = c(5,3),
    dpi = 600
){
  ### test that paths exist
  if (!file_test("-d", path_latex)){
    stop(paste("The latex file path doesn't exist:", path_latex))
  }
  if (!file_test("-d", path_graphics)){
    stop(paste("The graphics file path doesn't exist:", path_latex))
  }
  
  if(file.exists(output_filename)) file.remove(output_filename)
  file.create(output_filename)
  
  structure(
      list(
          path_latex           = path_latex,
          path_graphics        = path_graphics,
          output_filename      = output_filename,
          default_plot_size    = default_plot_size,
          dpi                  = dpi,
          counter              = new_counter(counter_start) ,
          appender             = new_appender()
    ), 
      class = "braid"
  )
}


################################################################################
### Define a class of functions with a static variable (i)
### Important: This must be initialised before use
################################################################################


#' Creates a counter. 
#' 
#' Must be initialised, e.g. braid_counter <- new_counter()
#' 
#' @keywords internal
#' @param start The starting value of the counter, defaults to 1
new_counter <- function(start=1) {
  i <- start - 1
# i <- i - 1
  function() {
    # do something useful, then ...
    i <<- i + 1
    i
  }
}

#' Creates a closure for the temporary storage of braid_write output.
#' 
#' Must be initialised, e.g. braid_appender <- new_appender()
#' 
#' @keywords internal
new_appender <- function() {
  text <- ""
  function(new_text=NULL, reset=FALSE) {
    if(reset) text <<- ""
    if(!is.null(new_text)) text <<- paste(text, new_text, sep="")
    invisible(text)
  }
}


################################################################################

#' Updates braid counter.
#' 
#' Each braid object has a static counter that can be used to generated automatic filenames. This function increments the counter and returns the result.
#' 
#' @param braid A braid object
#' @keywords internal
braid_inc_counter <- function(braid){
  eval(braid$counter(), envir=parent.frame(n=3))
  #eval(braid$counter(), envir=.GlobalEnv)
  #braid$counter()
}

################################################################################

#' Appends braid text.
#' 
#' The output from braid_write is stored in a character vector, for later dumping to file.
#' 
#' @param braid A braid object
#' @keywords internal
braid_append_text <- function(braid, text="", reset=FALSE){
  eval(braid$appender(text, reset), envir=parent.frame(n=1))
}


