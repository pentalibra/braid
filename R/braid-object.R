# Braid object definition
# 
# Author: Andrie
#------------------------------------------------------------------------------


#' Creates object of class braid.
#' 
#' A braid object describes the data and meta-data in the survey that will be analysed by the analysis and reporting functions in the braid package.
#' 
#' @param pathLatex Path where latex will be saved
#' @param pathGraphics Path where graphics files will be saved
#' @param outputFilename Filename where latex output will be saved
#' @param counterStart The starting number for a counter used to store graphs, 
#' @param defaultPlotSize Plot size in inches, e.g. c(4, 3)
#' @param dpi Dots per inch, passed to ggsave()
#' @return A list object of class braid
#' @export
as.braid <- function(
    pathLatex = Sys.getenv()["TEMP"],
    pathGraphics = pathLatex,
    outputFilename = file.path(pathLatex, "braid.tex"),
    counterStart = 1,
    defaultPlotSize = c(5,3),
    dpi = 600
){
  ### test that paths exist
  if (!file_test("-d", pathLatex)){
    stop(paste("The latex file path doesn't exist:", pathLatex))
  }
  if (!file_test("-d", pathGraphics)){
    stop(paste("The graphics file path doesn't exist:", pathLatex))
  }
  
  if(file.exists(outputFilename)) file.remove(outputFilename)
  file.create(outputFilename)
  
  structure(
      list(
          pathLatex           = pathLatex,
          pathGraphics        = pathGraphics,
          outputFilename      = outputFilename,
          defaultPlotSize     = defaultPlotSize,
          dpi                 = dpi,
          counter             = newCounter(counterStart) ,
          appender            = newAppender(),
          plotAppender        = newPlotAppender()
    ), 
      class = "braid"
  )
}


