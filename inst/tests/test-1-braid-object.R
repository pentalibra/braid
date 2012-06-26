# Test functionality using testthat library
# 
# Author: Andrie
#------------------------------------------------------------------------------

path <- tempdir()
latex_path <- file.path(path, "latex")
dir.create(latex_path, recursive=TRUE)
graph_path <- file.path(latex_path, "graphics")
dir.create(graph_path, recursive=TRUE)
sinkfile   <- file.path(latex_path, "braid_test.tex")

clearFiles <- function(){
  file.remove(list.files(graph_path, full.names=TRUE))
  if (file.exists(file.path(latex_path, sinkfile))){
    file.remove(file.path(latex_path, sinkfile))
  }
}

clearFiles()

#------------------------------------------------------------------------------

context("Test as.braid")

test_that("as.braid sets up correct file names", {
      b <- as.braid(path=path)
      expect_equal(b$pathLatex, path)
      expect_equal(b$fileOuter, file.path(path, "outline.tex"))
      expect_equal(b$fileInner, file.path(path, "braid.tex"))
    })



#------------------------------------------------------------------------------

context("Test braid appender")

test_that("braid counter incrementing works", {
      b <- as.braid(path=latex_path, fileInner="braid_test.tex")
      expect_that(braidAppendText(b, "a"), equals("a"))
      expect_that(braidAppendText(b, "b"), equals("ab"))
      expect_that(braidAppendText(b, "c"), equals("abc"))
    })

#------------------------------------------------------------------------------

context("braidCounter and file naming")

test_that("braid counter incrementing works", {
      b <- as.braid(path=latex_path, fileInner="braid_test.tex")
      expect_that(braidIncCounter(b), equals(1))
      expect_that(braidIncCounter(b), equals(2))
      expect_that(braidIncCounter(b), equals(3))
    })

test_that("braid filenames are correct", {
      b <- as.braid(path=latex_path, fileInner="braid_test.tex")
      expect_that(braidFilename(b), equals("fig0001.pdf"))
      fn <- braidFilename(b, prefix="Gr", suffix="_x", ext=".png")
      #print(fn)
      expect_that(fn, equals("Gr0002_x.png"))
      expect_that(braidFilename(b, format="%02d"), equals("fig03.pdf"))
    })

#------------------------------------------------------------------------------

context("plotAppender")
b <- as.braid(path=latex_path, fileInner="braid_test.tex")

require(ggplot2)
t1 <- ggplot(mtcars, aes(factor(cyl))) + geom_bar()
t2 <- ggplot(mtcars, aes(x=hp, y=cyl)) + geom_point()

test_that("plotAppender returns correct value", {
      
      #browser()
      expect_equal(braidAppendPlot(b), NULL)
      
      braidAppendPlot(b, t1, filename="plot_t1.pdf")
      test <- braidAppendPlot(b)
      rest <- list(list(plotcode=t1, filename="plot_t1.pdf", width=4, height=3, Qid=NA))
      expect_equal(test, rest)
    })

test_that("plotAppender correctly appends second value", {

      braidAppendPlot(b, t1, filename="plot_t1.pdf", Qid="Test of Qid")
      test <- braidAppendPlot(b)
      rest <- list(
          list(plotcode=t1, filename="plot_t1.pdf", width=4, height=3, Qid=NA),
          list(plotcode=t1, filename="plot_t1.pdf", width=4, height=3, Qid="Test of Qid")
      )
      expect_equal(test, rest)
    })

      
test_that("plotAppender correctly appends third value", {
      
      braidAppendPlot(b, t2, filename="plot_t2.png", Qid="Another test")
      test <- braidAppendPlot(b)
      rest <- list(
          list(plotcode=t1, filename="plot_t1.pdf", width=4, height=3, Qid=NA),
          list(plotcode=t1, filename="plot_t1.pdf", width=4, height=3, Qid="Test of Qid"),
          list(plotcode=t2, filename="plot_t2.png", width=4, height=3, Qid="Another test")
      )
      expect_equal(test, rest)
      expect_is(test, "list")
      
      expect_equal(braidAppendPlot(b), rest)
    })

braidSave(b)
clearFiles()
unlink(path, recursive=TRUE)



