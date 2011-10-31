0# Tests plotCache functions
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

#------------------------------------------------------------------------------

context("plotCache")

      
      clearFiles()
      b <- as.braid(path=latex_path, file="braid_test.tex")
      
      Depth <- lattice::equal.count(quakes$depth, number=8, overlap=.1)
      t1 <- lattice::xyplot(lat ~ long | Depth, data = quakes)
      t2 <- lattice::barchart(yield ~ variety | site, data = barley)

      f1 <- "plot_t1.pdf"
      f2 <- "plot_t2.pdf"

      braidPlot(b, t1, filename=f1)
      braidPlot(b, t2, filename=f2)
      plotlist <- braidAppendPlot(b, NULL)
      plot1 <- plotlist[[1]]
      plot2 <- plotlist[[2]]
      
      
test_that("plotCache creates correct file names", { 
      expect_equal(filenamePlotCache(b, f1), file.path(b$pathGraphics, "braid_plot_t1.pdf.rds"))
      expect_equal(filenamePlotCache(b, f2), file.path(b$pathGraphics, "braid_plot_t2.pdf.rds"))
    })

test_that("plotCache correctly determines plot doesn't exist", { 
      
      
      expect_false(cacheExists(b, plot1$plotcode, plot1$filename, plot1$width, plot1$height))
      expect_false(cacheExists(b, plot2$plotcode, plot2$filename, plot2$width, plot2$height))
      braidSave(b)
    })
      
      #print(plotlist)
test_that("plotCache correctly determines plot exists", { 
            
      savePlotCache(b, plot1$plotcode, plot1$filename, plot1$width, plot1$height)
      savePlotCache(b, plot2$plotcode, plot2$filename, plot2$width, plot2$height)
      expect_true(file.exists(file.path(b$pathGraphics, f1)))
      expect_true(file.exists(file.path(b$pathGraphics, f2)))
      expect_true(cacheExists(b, plot1$plotcode, plot1$filename, plot1$width, plot1$height))
      expect_true(cacheExists(b, plot2$plotcode, plot2$filename, plot2$width, plot2$height))
      
    })

test_that("plotCache gives same answer as individual tests", {
      
      pRec <- readPlotCache(b, plot1$filename)
      expect_equal(pRec$plotcode, plot1$plotcode)
      expect_equal(pRec$filename, plot1$filename)
      expect_equal(pRec$width, 5)
      expect_equal(pRec$height, 3)
      expect_equal(pRec$md5sum, tools::md5sum(file.path(b$pathGraphics, plot1$filename)))
      expect_is(pRec, "braidPlotCache")
    })

test_that("plotCache individual tests are correct", {
      pRec <- readPlotCache(b, plot1$filename)
      expect_equal(pRec, 
              plotCache(
                  b,
                  plot1$plotcode, 
                  plot1$filename, 
                  plot1$width, 
                  plot1$height
                  ))
      expect_true(cacheExists(b, plot1$plotcode, plot1$filename, plot1$width, plot1$height))
      
      savePlotCache(b, plot2$plotcode, plot2$filename, plot2$width, plot2$height)
      expect_true(cacheExists(b, plot2$plotcode, plot2$filename, plot2$width, plot2$height))
      
    })


