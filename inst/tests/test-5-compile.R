# Add comment
# 
# Author: Andrie
#----------------------------------------------------------------------------------

path <- file.path("f:", "git", "braid", "test")
latexPath <- file.path(path, "latex")
graph_path <- file.path(latexPath, "graphics")
sinkfile   <- file.path(latexPath, "braid_test.tex")

clearFiles <- function(){
  file.remove(list.files(graph_path, full.names=TRUE))
  if (file.exists(file.path(latexPath, sinkfile))){
    file.remove(file.path(latexPath, sinkfile))
  }
}


clearFiles()

outlineFile <- file.path(latexPath, "Outline.tex")
contentFile <- "Content.tex"
pdfFile <- "Outline.pdf"
if (file.exists(outlineFile)){
  file.remove(outlineFile)
}
if (file.exists(file.path(latexPath, contentFile))){
  file.remove(file.path(latexPath, contentFile))
}
if (file.exists(file.path(latexPath, pdfFile))){
  file.remove(file.path(latexPath, pdfFile))
}


context("braidLatexOutline")

test_that("braid_outline is created", {
      b <- as.braid(path=latexPath, fileOuter=outlineFile, fileInner=contentFile)
      braidLatexOutline(
          b,
          fileOuter=outlineFile,
          fileInner=contentFile,
          title="Test",
          author="I am the author")
      expect_true(file.exists(outlineFile))
    })


context("braidCompile")

test_that("braid file gets compiled",{
      b <- as.braid(path=latexPath, fileOuter=outlineFile, fileInner=contentFile)
      braidHeading(b, "This is a test")
      braidWrite(b, "This should be a normal paragraph.")
      braidWrite(b, "And another")
      t <- ggplot(mtcars, aes(factor(cyl))) + geom_bar()
      braidPlot(b, t)
      
      braidSave(b)

      expect_false(file.exists(file.path(latexPath, pdfFile)))
      braidCompile(b)
      expect_true(file.exists(file.path(latexPath, pdfFile)))
      
    })


