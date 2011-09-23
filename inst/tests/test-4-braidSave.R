# Add comment
# 
# Author: Andrie
#----------------------------------------------------------------------------------


path <- file.path("f:", "git", "braid", "test")
latex_path <- file.path(path, "latex")
graph_path <- file.path(latex_path, "graphics")
sinkfile   <- file.path(latex_path, "braid_test.tex")

clearFiles <- function(){
  file.remove(list.files(graph_path, full.names=TRUE))
  if (file.exists(file.path(latex_path, sinkfile))){
    file.remove(file.path(latex_path, sinkfile))
  }
}

#----------------------------------------------------------------------------------

context("braidSave")

test_that("braidSave works in clean environment", {
      clearFiles()
      b <- as.braid(
          pathLatex    = latex_path,
          pathGraphics = graph_path,
          outputFilename=sinkfile
      )
      
      filename <- braidFilename(b, counter=1, prefix="fig", suffix="a", ext=".pdf")
      
      Depth <- lattice::equal.count(quakes$depth, number=8, overlap=.1)
      t <- lattice::xyplot(lat ~ long | Depth, data = quakes)
      
      braidPlot(b, t, filename=filename)
      braidSave(b)
      
      plotfile <- file.path(graph_path, "fig0001a.pdf")
      expect_true(file.exists(plotfile))
      expect_true(file.exists(filenamePlotRecord(b, "fig0001a.pdf")))
    })


test_that("braidSave still works when rerun on existing files", {
      b <- as.braid(
          pathLatex    = latex_path,
          pathGraphics = graph_path,
          outputFilename=sinkfile
      )
      
      filename <- braidFilename(b, counter=1, prefix="fig", suffix="a", ext=".pdf")
      
      Depth <- lattice::equal.count(quakes$depth, number=8, overlap=.1)
      t <- lattice::xyplot(lat ~ long | Depth, data = quakes)
      
      plotfile <- file.path(graph_path, "fig0001a.pdf")
      expect_true(file.exists(plotfile))
      expect_true(file.exists(filenamePlotRecord(b, "fig0001a.pdf")))
      
      braidPlot(b, t, filename=filename)
      braidSave(b)
     
      plotfile <- file.path(graph_path, "fig0001a.pdf")
      expect_true(file.exists(plotfile))
      expect_true(file.exists(filenamePlotRecord(b, "fig0001a.pdf")))
    })
