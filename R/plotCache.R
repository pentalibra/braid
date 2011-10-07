# Add comment
# 
# Author: Andrie
#----------------------------------------------------------------------------------

filenamePlot <- function(b, filename){
  file.path(b$pathGraphics, filename)
}

filenamePlotCache <- function(b, filename){
  file.path(b$pathGraphics, paste("braid_", filename, ".rds", sep=""))
}

#----------------------------------------------------------------------------------

plotCache <- function(braid, plotcode, filename, width, height){
  cache <- list(
      plotcode = plotcode, 
      filename = filename, 
      width = width, 
      height = height,
      md5sum=tools::md5sum(filenamePlot(braid, filename))
  )
  class(cache) <- "braidPlotCache"
  cache
}

#----------------------------------------------------------------------------------

readPlotCache <- function(b, filename){
  readRDS(file=filenamePlotCache(b, filename))
}

#----------------------------------------------------------------------------------

savePlotCache <- function(b, plotcode, filename, width, height){
  newPlotCache <- plotCache(
      b,
      plotcode,
      filename,  
      width, 
      height
  )
  saveRDS(newPlotCache, 
      file=filenamePlotCache(b, filename))
  invisible(NULL)
}

#----------------------------------------------------------------------------------

cacheExists <- function(b, plotcode, filename, width, height){
  if(file.exists(filenamePlotCache(b, filename))){
    oldPlotCache <- readPlotCache(b, filename)
    newPlotCache <- plotCache(
        b,
        plotcode, 
        filename, 
        width, 
        height)
    if(identical(
        class(oldPlotCache$plotcode),
        class(newPlotCache$plotcode)
      )){
      test <- c(
        isTRUE(all.equal(oldPlotCache$plotcode, newPlotCache$plotcode)), # environments may differ
        identical(oldPlotCache$filename, newPlotCache$filename),
        identical(oldPlotCache$width,    newPlotCache$width),
        identical(oldPlotCache$height,   newPlotCache$height),
        identical(oldPlotCache$md5sum,   newPlotCache$md5sum)
      )
    } else {
      test <- FALSE
    }
    all(test)
  } else {
    FALSE
  }
}


