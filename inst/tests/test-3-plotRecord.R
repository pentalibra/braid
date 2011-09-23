0# Tests plotRecord functions
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

context("plotRecord")

      
      clearFiles()
      b <- as.braid(
          pathLatex    = latex_path,
          pathGraphics = graph_path,
          outputFilename=sinkfile
      )
      
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
      
      
test_that("plotRecord creates correct file names", { 
      expect_equal(filenamePlotRecord(b, f1), file.path(b$pathGraphics, "braid_plot_t1.pdf.rds"))
      expect_equal(filenamePlotRecord(b, f2), file.path(b$pathGraphics, "braid_plot_t2.pdf.rds"))
    })

test_that("plotRecord correctly determines plot doesn't exist", { 
      
      
      expect_false(plotExists(b, plot1$plotcode, plot1$filename, plot1$width, plot1$height))
      expect_false(plotExists(b, plot2$plotcode, plot2$filename, plot2$width, plot2$height))
      braidSave(b)
    })
      
      #print(plotlist)
test_that("plotRecord correctly determines plot exists", { 
            
      savePlotRecord(b, plot1$plotcode, plot1$filename, plot1$width, plot1$height)
      savePlotRecord(b, plot2$plotcode, plot2$filename, plot2$width, plot2$height)
      expect_true(file.exists(file.path(b$pathGraphics, f1)))
      expect_true(file.exists(file.path(b$pathGraphics, f2)))
      expect_true(plotExists(b, plot1$plotcode, plot1$filename, plot1$width, plot1$height))
      expect_true(plotExists(b, plot2$plotcode, plot2$filename, plot2$width, plot2$height))
      
    })

test_that("plotRecord does something else", {
      
      pRec <- readPlotRecord(b, plot1$filename)
      expect_equal(pRec$plotcode, plot1$plotcode)
      expect_equal(pRec$filename, plot1$filename)
      expect_equal(pRec$width, 5)
      expect_equal(pRec$height, 3)
      expect_equal(pRec$md5sum, tools::md5sum(file.path(b$pathGraphics, plot1$filename)))
      expect_is(pRec, "braidPlotRecord")
    })

test_that("plotRecord individual tests are correct", {
      pRec <- readPlotRecord(b, plot1$filename)
      expect_equal(pRec, 
              plotRecord(
                  b,
                  plot1$plotcode, 
                  plot1$filename, 
                  plot1$width, 
                  plot1$height
                  ))
      expect_true(plotExists(b, plot1$plotcode, plot1$filename, plot1$width, plot1$height))
      
      savePlotRecord(b, plot2$plotcode, plot2$filename, plot2$width, plot2$height)
      expect_true(plotExists(b, plot2$plotcode, plot2$filename, plot2$width, plot2$height))
      
    })


