# Test functionality using testthat library
# 
# Author: Andrie
#------------------------------------------------------------------------------




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

context("braidWrite")

test_that("braidWrite appends output to file", {

    clearFiles()
    b <- as.braid(
        pathLatex    = latex_path,
        pathGraphics = graph_path,
        outputFilename=sinkfile
    )
      
    x <- braidWrite(b, "Test output")
    braidSave(b)
    expect_that(x, equals(NULL))
    expect_that(file.exists(sinkfile), equals(TRUE))
    
    braidWrite(b, "Line 2\n")
    braidSave(b)
    ret <- readLines(con=sinkfile)
    expect_that(ret, equals(c("Test output", "Line 2", "")))
    
		})

#------------------------------------------------------------------------------

context("braidHeading")

test_that("braidHeading writes correct output to file",{
      
      clearFiles()
      b <- as.braid(
        pathLatex    = latex_path,
        pathGraphics = graph_path,
        outputFilename=sinkfile
      )
      
      braidHeading(b, "Heading level 1")
      braidSave(b)
      ret <- readLines(con=sinkfile)
      expect_equal(ret, c("\\chapter{Heading level 1}", ""))
})

#------------------------------------------------------------------------------

context("braidPlot - ggplot")

test_that("braidPlot saves ggplot", {
      clearFiles()
      b <- as.braid(
          pathLatex    = latex_path,
          pathGraphics = graph_path,
          outputFilename=sinkfile
      )
      filename <- braidFilename(b, counter=1, prefix="fig", suffix="a", ext=".pdf")
      expect_equal(braidAppendPlot(b), NULL)
      
      require(ggplot2)
      t <- ggplot(mtcars, aes(factor(cyl))) + geom_bar()

      braidPlot(b, t, filename)
      test <- braidAppendPlot(b)
      rest <- list(list(plotcode=t, filename=filename, width=5, height=3))
      expect_equal(test, rest)
      
      braidSave(b)
      plotfile <- file.path(graph_path, "fig0001a.pdf")
      expect_true(file.exists(plotfile))

      test <- readLines(con=sinkfile)
      rest <- c("  \\PlaceGraph{graphics/fig0001a.pdf}")
      expect_equal(test, rest)
    })

#------------------------------------------------------------------------------

context("braidPlot - trellis")

test_that("braidPlot saves trellis plot", {
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
      
      test <- braidAppendPlot(b)
      rest <- list(list(plotcode=t, filename=filename, width=5, height=3))
      expect_equal(test, rest)
      
      braidSave(b)
      plotfile <- file.path(graph_path, "fig0001a.pdf")
      expect_true(file.exists(plotfile))
      
      test <- readLines(con=sinkfile)
      rest <- c("  \\PlaceGraph{graphics/fig0001a.pdf}")
      expect_equal(test, rest)
    })


