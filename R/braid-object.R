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
#' @param fileOuter Filename of latex outline file (used by \code{\link{braidCompile}}
#' @param fileInner Filename where latex output will be saved (This gets appended to \code{pathOutput})
#' @param counterStart The starting number for a counter used to store graphs, defaults to 1
#' @param defaultPlotSize Numeric vector of length 2: Plot size in inches, e.g. c(4, 3)
#' @param dpi Dots per inch, passed to ggsave()
#' @param outputType Character string specifying the destination of output: "latex", "ppt" or "device".  If "device", graphs are sent to the default device (typically the RGgui plot terminal)
#' @param graphicFormat Device type for saving graphic plots.  Currently only pdf and wmf is supported.
#' @return A list object of class braid
#' @export 
as.braid <- function(
    path = tempdir(),
    graphics = "graphics",
    fileOuter = "outline.tex",
    fileInner = "braid.tex",
    counterStart = 1,
    defaultPlotSize = c(5,3),
    dpi = 600,
    outputType = c("latex", "ppt", "device"),
    graphicFormat = c("pdf", "wmf")
    
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
  fileOuter <- file.path(path, basename(fileOuter))
  fileInner <- file.path(path, basename(fileInner))
  if(file.exists(fileInner)) file.remove(fileInner)
  file.create(fileInner)
  
  outputType <- match.arg(outputType)
  graphicFormat <- match.arg(graphicFormat)
  if(!is.null(outputType) && outputType=="ppt") graphicFormat <- "wmf"
  
  structure(
      list(
          pathLatex           = path,
          pathGraphics        = graphics,
          fileOuter           = fileOuter,
          fileInner           = fileInner,
          defaultPlotSize     = defaultPlotSize,
          dpi                 = dpi,
          outputType          = outputType,
          graphicFormat       = graphicFormat,
          counter             = newCounter(counterStart) ,
          appender            = newAppender(),
          plotAppender        = newPlotAppender()
    ), 
      class = "braid"
  )
}

#' Tests that object is of class braid.
#' 
#' @param x Object to be tested
#' @export
is.braid <- function(x){
  inherits(x, "braid")
}


