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
      b <- as.braid(path=latex_path, fileInner="braid_test.tex")
      
      filename <- braidFilename(b, counter=1, prefix="fig", suffix="a", ext=".pdf")
      
      Depth <- lattice::equal.count(quakes$depth, number=8, overlap=.1)
      t <- lattice::xyplot(lat ~ long | Depth, data = quakes)
      
      braidPlot(b, t, filename=filename)
      braidSave(b)
      
      plotfile <- file.path(graph_path, "fig0001a.pdf")
      expect_true(file.exists(plotfile))
      expect_true(file.exists(filenamePlotCache(b, "fig0001a.pdf")))
    })


test_that("braidSave still works when rerun on existing files", {
      b <- as.braid(path=latex_path, fileInner="braid_test.tex")
      
      filename <- braidFilename(b, counter=1, prefix="fig", suffix="a", ext=".pdf")
      
      Depth <- lattice::equal.count(quakes$depth, number=8, overlap=.1)
      t <- lattice::xyplot(lat ~ long | Depth, data = quakes)
      
      plotfile <- file.path(graph_path, "fig0001a.pdf")
      expect_true(file.exists(plotfile))
      expect_true(file.exists(filenamePlotCache(b, "fig0001a.pdf")))
      
      braidPlot(b, t, filename=filename)
      braidSave(b)
     
      plotfile <- file.path(graph_path, "fig0001a.pdf")
      expect_true(file.exists(plotfile))
      expect_true(file.exists(filenamePlotCache(b, "fig0001a.pdf")))
    })

test_that("fileExtension correctly identifies file type",{
      expect_equal(fileExtension("somename.pdf"), "pdf")
      expect_equal(fileExtension("somename.wmf"), "wmf")
    })

test_that("braidSave saves files in correct format", {
      clearFiles()
      b <- as.braid(path=latex_path, fileInner="braid_test.tex")
      Depth <- lattice::equal.count(quakes$depth, number=8, overlap=.1)
      t <- lattice::xyplot(lat ~ long | Depth, data = quakes)
      
      braidPlot(b, t, filename="fig01.pdf")
      braidPlot(b, t, filename="fig02.wmf")
      braidSave(b)
      expect_true(file.exists(file.path(graph_path, "fig01.pdf")))
      expect_true(file.exists(file.path(graph_path, "fig02.wmf")))
      
    })
