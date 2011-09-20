# Test functionality using testthat library
# 
# Author: Andrie
###############################################################################




path <- file.path("f:", "git", "braid", "test")
latex_path <- file.path(path, "latex")
graph_path <- file.path(latex_path, "graphics")
sinkfile   <- file.path(latex_path, "braid_test.tex")

file.remove(list.files(graph_path, full.names=TRUE))
if (file.exists(file.path(latex_path, sinkfile))){
  file.remove(file.path(latex_path, sinkfile))
}




###############################################################################

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

###############################################################################

context("Test braid counter and file naming")

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

context("braidWrite")

test_that("braidWrite appends output to file", {

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

context("braidPlot - trellis")

test_that("braidPlot saves trellis plot", {
      b <- as.braid(
          pathLatex    = latex_path,
          pathGraphics = graph_path,
          outputFilename=sinkfile
      )
      filename <- braidFilename(b, counter=1, prefix="fig", suffix="%04d", ext=".pdf")
      
      Depth <- lattice::equal.count(quakes$depth, number=8, overlap=.1)
      t <- lattice::xyplot(lat ~ long | Depth, data = quakes)
      
      braidPlot(b, t)
      braidSave(b)
      
      test <- readLines(con=sinkfile)
      rest <- c("  \\PlaceGraph{graphics/fig0001.pdf}")
      expect_equal(test, rest)
    })

#------------------------------------------------------------------------------

context("braidPlot - ggplot")

test_that("braidPlot saves ggplot", {
      b <- as.braid(
          pathLatex    = latex_path,
          pathGraphics = graph_path,
          outputFilename=sinkfile
      )
      filename <- braidFilename(b, counter=1, prefix="fig", suffix="%04d", ext=".pdf")
      
      require(ggplot2)
      t <- ggplot(mtcars, aes(factor(cyl))) + geom_bar()
      braidPlot(b, t)
      braidSave(b)
      test <- readLines(con=sinkfile)
      rest <- c("  \\PlaceGraph{graphics/fig0001.pdf}")
      expect_equal(test, rest)
    })

#------------------------------------------------------------------------------

context("Create and compile braid object")

file.remove(list.files(graph_path, full.names=TRUE))
if (file.exists(file.path(latex_path, sinkfile))){
  file.remove(file.path(latex_path, sinkfile))
}

outline_file <- file.path(latex_path, "Outline.tex")
content_file <- "Content.tex"
pdf_file <- "Outline.pdf"
if (file.exists(outline_file)){
  file.remove(outline_file)
}
if (file.exists(file.path(latex_path, content_file))){
  file.remove(file.path(latex_path, content_file))
}
if (file.exists(file.path(latex_path, pdf_file))){
  file.remove(file.path(latex_path, pdf_file))
}


test_that("braid_outline is created", {
      braid_latex_outline(
          #pathLatex=latex_path, 
          outputFilename=outline_file,
          content_filename=content_file,
          title="Test",
          author="I am the author")
      expect_that(file.exists(outline_file), is_true())
})

test_that("braid file gets compiled",{
      b <- as.braid(
          pathLatex    = latex_path,
          pathGraphics = graph_path,
          outputFilename=file.path(latex_path, content_file)
      )
      braidHeading(b, "This is a test")
      braidWrite(b, "This should be a normal paragraph.")
      braidWrite(b, "And another")
      t <- ggplot(mtcars, aes(factor(cyl))) + geom_bar()
      braidPlot(b, t)
      
      braidSave(b)
      braidCompile(outline_file)
      expect_that(file.exists(file.path(latex_path, pdf_file)), is_true())
      
    })

