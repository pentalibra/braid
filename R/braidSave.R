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
  x        <- plotElement$plot
  filename <- plotElement$filename
  width    <- plotElement$width
  height   <- plotElement$height
  if(inherits(x, "ggplot")){
    require(ggplot2)
    ggplot2::ggsave(
        filename = filename, 
        plot=x, 
        width    = width, 
        height   = height,
        dpi      = braid$dpi, 
        path     = braid$pathGraphics
    )
  } 
  if(inherits(x, "trellis")){
    require(lattice)
    on.exit(dev.off())
    pdf(
        file=file.path(braid$pathGraphics, filename),
        width    = width, 
        height   = height
    )
    print(x)
  } 
  
  file.exists(file.path(braid$pathGraphics, filename))
  
}

#------------------------------------------------------------------------------

braidSavePlot <- function(braid){
  plotlist <- braidAppendPlot(braid, NULL)
  if(!is.null(plotlist)){
    ret <- sapply(plotlist, braidSavePlotOne, braid)
    if(all(ret)) TRUE else FALSE
  } else {
    ret <- FALSE
  }
  ret
}


