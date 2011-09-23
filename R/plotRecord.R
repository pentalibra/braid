# Add comment
# 
# Author: Andrie
#----------------------------------------------------------------------------------

filenamePlot <- function(b, filename){
  file.path(b$pathGraphics, filename)
}

filenamePlotRecord <- function(b, filename){
  file.path(b$pathGraphics, paste("braid_", filename, ".rds", sep=""))
}

#----------------------------------------------------------------------------------

plotRecord <- function(braid, plotcode, filename, width, height){
  record <- list(
      plotcode = plotcode, 
      filename = filename, 
      width = width, 
      height = height,
      md5sum=tools::md5sum(filenamePlot(braid, filename))
  )
  class(record) <- "braidPlotRecord"
  record
}

#----------------------------------------------------------------------------------

readPlotRecord <- function(b, filename){
  readRDS(file=filenamePlotRecord(b, filename))
}

#----------------------------------------------------------------------------------

savePlotRecord <- function(b, plotcode, filename, width, height){
  newPlotRecord <- plotRecord(
      b,
      plotcode,
      filename,  
      width, 
      height
  )
  saveRDS(newPlotRecord, 
      file=filenamePlotRecord(b, filename))
  invisible(NULL)
}

#----------------------------------------------------------------------------------

plotExists <- function(b, plotcode, filename, width, height){
  if(file.exists(filenamePlotRecord(b, filename))){
    oldPlotRecord <- readPlotRecord(b, filename)
    newPlotRecord <- plotRecord(
        b,
        plotcode, 
        filename, 
        width, 
        height)
    test <- c(
        isTRUE(all.equal(oldPlotRecord$plotcode, newPlotRecord$plotcode)), # environments may differ
        identical(oldPlotRecord$filename, newPlotRecord$filename),
        identical(oldPlotRecord$width,    newPlotRecord$width),
        identical(oldPlotRecord$height,   newPlotRecord$height),
        identical(oldPlotRecord$md5sum,   newPlotRecord$md5sum)
    )
#    if(any(test)){
#      cat("\n",
#          paste(
#              ifelse(
#                test, 
#                rep("=", length(test)),
#                c("-plotcode-", "-filename-", "-width-", "-height-", "-md5sum-")), 
#          collapse=""))
#      #browser()
#      #cat("\nfilename:", oldPlotRecord$filename, "<>", newPlotRecord$filename, "\n")
#    }

    all(test)
  } else{
    FALSE
  }
}


