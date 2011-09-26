# Add comment
# 
# Author: Andrie
#----------------------------------------------------------------------------------


#' Writes result to braid output file.
#' 
#' Writes result to braid output file.
#' 
#' @param braid A braid object
#' @export
braidSave <- function(braid){
  text <- braidAppendText(braid)
  sfile <- file(braid$outputFilename, "at")  ### Open file in append mode
  on.exit(close(sfile))
  cat(text, file=sfile, append=TRUE)
  braidAppendText(braid, reset=TRUE)
  
  braidSavePlot(braid)
  braidAppendPlot(braid, reset=TRUE)
  invisible(NULL)
}

#------------------------------------------------------------------------------

braidSavePlotOne <- function(plotElement, braid){
  plotcode <- plotElement$plot
  filename <- plotElement$filename
  width    <- plotElement$width
  height   <- plotElement$height
  Qid      <- plotElement$Qid
  
  pEx <- plotExists(braid, plotcode, filename, width, height)
  if(!pEx){
    
    if(inherits(plotcode, "ggplot")){
      require(ggplot2)
      errorMessage <- paste("\nError in saving ggplot\nQid = ", Qid, "\nFilename = ", filename, "\n")
      res <- tryCatch(
          ggplot2::ggsave(
              filename = filename, 
              plot     = plotcode, 
              width    = width, 
              height   = height,
              dpi      = braid$dpi, 
              path     = braid$pathGraphics
          ),
          error = function(e) e
      )
      if(inherits(res, "error")) message(paste(errorMessage, "\n", res, "\n"))
    } 
    if(inherits(plotcode, "trellis")){
      require(lattice)
      on.exit(dev.off())
      pdf(
          file=file.path(braid$pathGraphics, filename),
          width    = width, 
          height   = height
      )
      print(plotcode)
    } 
    savePlotRecord(braid, plotcode, filename, width, height)
  }
  ret <- file.exists(file.path(braid$pathGraphics, filename))
#  if(ret) message(paste("Saved", filename))
  ret
  
}

#------------------------------------------------------------------------------

braidSavePlot <- function(braid){
  #multiCore <- braid$multiCore
  plotlist <- braidAppendPlot(braid, NULL)
  if(!is.null(plotlist)){
#    if(multiCore){
#      require(doSMP)
#      workers <- doSMP::startWorkers(2)
#      on.exit(doSMP::stopWorkers(workers))
#      doSMP::registerDoSMP(workers)
#      ret <- laply(plotlist, braidSavePlotOne, braid, .parallel=multiCore)
#    } else {
      progressBar <- ifelse(length(plotlist) >= 5, "tk", "none")
      ret <- plyr::laply(plotlist, braidSavePlotOne, braid, .progress=progressBar)
#    }
    ret <- all(ret)
  } else {
    ret <- FALSE
  }
  ret
}


