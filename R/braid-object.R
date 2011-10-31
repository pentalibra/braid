# Braid object definition
# 
# Author: Andrie
#------------------------------------------------------------------------------


#' Creates object of class braid.
#' 
#' A braid object describes the data and meta-data in the survey that will be analysed by the analysis and reporting functions in the braid package.
#' 
#' @param path Character vector: File path where latex output will be saved
#' @param graphics Character vector: File path relative to \code{pathOutput} where to save graphics (this gets appended to pathOutput.)
#' @param file Filename where latex output will be saved (This gets appended to \code{pathOutput})
#' @param counterStart The starting number for a counter used to store graphs, defaults to 1
#' @param defaultPlotSize Numeric vector of length 2: Plot size in inches, e.g. c(4, 3)
#' @param dpi Dots per inch, passed to ggsave()
#' @return A list object of class braid
#' @export
as.braid <- function(
    path = Sys.getenv()["TEMP"],
    graphics = "graphics",
    file = "braid.tex",
    counterStart = 1,
    defaultPlotSize = c(5,3),
    dpi = 600
){
  ### test that paths exist
  if (!file_test("-d", path)){
    stop(paste("The latex file path doesn't exist:", path))
  }
  graphics <- file.path(path, graphics)
  if (!file_test("-d", graphics)){
    dir.create(path=graphics)
  }
  if (!file_test("-d", graphics)){
    stop(paste("The graphics file path doesn't exist:", path))
  }
  file <- file.path(path, file)
  if(file.exists(file)) file.remove(file)
  file.create(file)
  
  structure(
      list(
          pathLatex           = path,
          pathGraphics        = graphics,
          outputFilename      = file,
          defaultPlotSize     = defaultPlotSize,
          dpi                 = dpi,
          counter             = newCounter(counterStart) ,
          appender            = newAppender(),
          plotAppender        = newPlotAppender()
    ), 
      class = "braid"
  )
}


