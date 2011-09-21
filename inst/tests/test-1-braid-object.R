# Test functionality using testthat library
# 
# Author: Andrie
#------------------------------------------------------------------------------

path <- file.path("f:", "git", "braid", "test")
latex_path <- file.path(path, "latex")
graph_path <- file.path(latex_path, "graphics")
sinkfile   <- file.path(latex_path, "braid_test.tex")

file.remove(list.files(graph_path, full.names=TRUE))
if (file.exists(file.path(latex_path, sinkfile))){
  file.remove(file.path(latex_path, sinkfile))
}

#------------------------------------------------------------------------------

context("Test braid appender")

test_that("braid counter incrementing works", {
      b <- as.braid(
          pathLatex    = latex_path,
          pathGraphics = graph_path,
          outputFilename=sinkfile
      )
      expect_that(braidAppendText(b, "a"), equals("a"))
      expect_that(braidAppendText(b, "b"), equals("ab"))
      expect_that(braidAppendText(b, "c"), equals("abc"))
    })

#------------------------------------------------------------------------------

context("braidCounter and file naming")

test_that("braid counter incrementing works", {
      b <- as.braid(
          pathLatex    = latex_path,
          pathGraphics = graph_path,
          outputFilename=sinkfile
      )
      expect_that(braidIncCounter(b), equals(1))
      expect_that(braidIncCounter(b), equals(2))
      expect_that(braidIncCounter(b), equals(3))
    })

test_that("braid filenames are correct", {
      b <- as.braid(
          pathLatex    = latex_path,
          pathGraphics = graph_path,
          outputFilename=sinkfile
      )
      expect_that(braidFilename(b), equals("fig0001.pdf"))
      fn <- braidFilename(b, prefix="Gr", suffix="_x", ext=".png")
      #print(fn)
      expect_that(fn, equals("Gr0002_x.png"))
      expect_that(braidFilename(b, format="%02d"), equals("fig03.pdf"))
    })

#------------------------------------------------------------------------------

context("plotAppender")

test_that("braid plotAppender works", {
      b <- as.braid(
          pathLatex    = latex_path,
          pathGraphics = graph_path,
          outputFilename=sinkfile
      )
      
      require(ggplot2)
      t1 <- ggplot(mtcars, aes(factor(cyl))) + geom_bar()
      t2 <- ggplot(mtcars, aes(factor(cyl))) + geom_point()
      
      #browser()
      expect_equal(braidAppendPlot(b), NULL)
      
      braidAppendPlot(b, t1, filename="plot_t1.pdf")
      test <- braidAppendPlot(b)
      rest <- list(list(plot=t1, filename="plot_t1.pdf", width=4, height=3))
      expect_equal(test, rest)
      
      braidAppendPlot(b, t2, filename="plot_t2.png")
      test <- braidAppendPlot(b)
      rest <- list(
          list(plot=t1, filename="plot_t1.pdf", width=4, height=3),
          list(plot=t2, filename="plot_t2.png", width=4, height=3)
      )
      expect_equal(test, rest)
      expect_is(test, "list")
      
      expect_equal(braidAppendPlot(b), rest)
    })




