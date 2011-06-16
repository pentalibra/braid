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
          path_latex    = latex_path,
          path_graphics = graph_path,
          output_filename=sinkfile
      )
      expect_that(braid_append_text(b, "a"), equals("a"))
      expect_that(braid_append_text(b, "b"), equals("ab"))
      expect_that(braid_append_text(b, "c"), equals("abc"))
    })

###############################################################################

context("Test braid counter and file naming")

test_that("braid counter incrementing works", {
      b <- as.braid(
          path_latex    = latex_path,
          path_graphics = graph_path,
          output_filename=sinkfile
      )
      expect_that(braid_inc_counter(b), equals(1))
      expect_that(braid_inc_counter(b), equals(2))
      expect_that(braid_inc_counter(b), equals(3))
    })

test_that("braid filenames are correct", {
      b <- as.braid(
          path_latex    = latex_path,
          path_graphics = graph_path,
          output_filename=sinkfile
      )
      expect_that(braid_filename(b), equals("fig0001.pdf"))
      fn <- braid_filename(b, prefix="Gr", suffix="_x", ext=".png")
      #print(fn)
      expect_that(fn, equals("Gr0002_x.png"))
      expect_that(braid_filename(b, format="%02d"), equals("fig03.pdf"))
    })

###############################################################################

context("Test output to Latex")

test_that("braid_write appends output to file", {

    b <- as.braid(
        path_latex    = latex_path,
        path_graphics = graph_path,
        output_filename=sinkfile
    )
      
    x <- braid_write(b, "Test output")
    expect_that(x, equals("Test output\n"))
    braid_save(b)
    expect_that(file.exists(sinkfile), equals(TRUE))
    braid_write(b, "Line 2\n")
    braid_save(b)
    ret <- scan(file=sinkfile, what="character", sep="\n")
    #print(ret)
    expect_that(ret, equals(c("Test output", "Line 2")))
    
		})

test_that("braid_heading writes correct output to file",{
      
    b <- as.braid(
        path_latex    = latex_path,
        path_graphics = graph_path,
        output_filename=sinkfile
    )
    
    braid_heading(b, "Heading level 1")
    braid_save(b)
    expect_that(file.exists(sinkfile), equals(TRUE))
    ret <- scan(file=sinkfile, what="character", sep="\n")
    print(ret)
    expect_that(ret, equals("\\chapter{Heading level 1}"))
})

###############################################################################

context("Test braid_plot")
test_that("braid_plot saves trellis plot", {
      b <- as.braid(
          path_latex    = latex_path,
          path_graphics = graph_path,
          output_filename=sinkfile
      )
      filename <- braid_filename(b, counter=1, prefix="fig", suffix="%04d", ext=".pdf")
      
      Depth <- lattice::equal.count(quakes$depth, number=8, overlap=.1)
      t <- lattice::xyplot(lat ~ long | Depth, data = quakes)
      
      braid_plot(b, t)
      braid_save(b)
      expect_that(file.exists(sinkfile), equals(TRUE))
      ret <- scan(file=sinkfile, what="character", sep="\n")
      print(ret)
    })

test_that("braid_plot saves ggplot", {
      b <- as.braid(
          path_latex    = latex_path,
          path_graphics = graph_path,
          output_filename=sinkfile
      )
      filename <- braid_filename(b, counter=1, prefix="fig", suffix="%04d", ext=".pdf")
      
      t <- ggplot(mtcars, aes(factor(cyl))) + geom_bar()
      braid_plot(b, t)
      braid_save(b)
      expect_that(file.exists(sinkfile), is_true())
      ret <- scan(file=sinkfile, what="character", sep="\n")
      expect_that(identical(ret, character(0)), is_false())
      print(ret)
    })

###############################################################################

file.remove(list.files(graph_path, full.names=TRUE))
if (file.exists(file.path(latex_path, sinkfile))){
  file.remove(file.path(latex_path, sinkfile))
}

context("Create and compile braid object")

outline_file <- "Outline.tex"
content_file <- "Content.tex"
pdf_file <- "Outline.pdf"
if (file.exists(file.path(latex_path, outline_file))){
  file.remove(file.path(latex_path, outline_file))
}
if (file.exists(file.path(latex_path, content_file))){
  file.remove(file.path(latex_path, content_file))
}
if (file.exists(file.path(latex_path, pdf_file))){
  file.remove(file.path(latex_path, pdf_file))
}


test_that("braid_outline is created", {
      braid_latex_outline(
          path_latex=latex_path, 
          output_filename=outline_file,
          content_filename=content_file,
          title="Test",
          author="I am the author")
      expect_that(file.exists(file.path(latex_path, outline_file)), is_true())
})

test_that("braid file gets compiled",{
      b <- as.braid(
          path_latex    = latex_path,
          path_graphics = graph_path,
          output_filename=file.path(latex_path, content_file)
      )
      braid_heading(b, "This is a test")
      braid_write(b, "This should be a normal paragraph.")
      braid_write(b, "And another")
      braid_save(b)
      braid_compile(file.path(latex_path, outline_file))
      expect_that(file.exists(file.path(latex_path, pdf_file)), is_true())
      
    })

