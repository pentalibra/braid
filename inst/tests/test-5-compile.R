# Add comment
# 
# Author: Andrie
#----------------------------------------------------------------------------------

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

outlineFile <- file.path(latex_path, "Outline.tex")
contentFile <- "Content.tex"
pdfFile <- "Outline.pdf"
if (file.exists(outlineFile)){
  file.remove(outlineFile)
}
if (file.exists(file.path(latex_path, contentFile))){
  file.remove(file.path(latex_path, contentFile))
}
if (file.exists(file.path(latex_path, pdfFile))){
  file.remove(file.path(latex_path, pdfFile))
}


context("braidLatexOutline")

test_that("braid_outline is created", {
      b <- as.braid(path=latex_path, fileOuter=outlineFile, fileInner=contentFile)
      braidLatexOutline(
          b,
          fileOuter=outlineFile,
          fileInner=contentFile,
          title="Test",
          author="I am the author")
      expect_true(file.exists(outlineFile))
      braidSave(b)
    })


context("braidCompile")

test_that("braid file gets compiled",{
      b <- as.braid(path=latex_path, fileOuter=outlineFile, fileInner=contentFile)
      braidHeading(b, "This is a test")
      braidWrite(b, "This should be a normal paragraph.")
      braidWrite(b, "And another")
      t <- ggplot(mtcars, aes(factor(cyl))) + geom_bar()
      braidPlot(b, t)
      
      braidSave(b)

      expect_false(file.exists(file.path(latex_path, pdfFile)))
      braidCompile(b)
      expect_true(file.exists(file.path(latex_path, pdfFile)))
      
    })


clearFiles()
unlink(path, recursive=TRUE)
