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

context("Create and compile braid object")

clearFiles()

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
      braidLatexOutline(
          #pathLatex=latex_path, 
          outputFilename=outline_file,
          content_filename=content_file,
          title="Test",
          author="I am the author")
      expect_true(file.exists(outline_file))
    })

test_that("braid file gets compiled",{
      b <- as.braid(path=latex_path, file="braid_test.tex")
      braidHeading(b, "This is a test")
      braidWrite(b, "This should be a normal paragraph.")
      braidWrite(b, "And another")
      t <- ggplot(mtcars, aes(factor(cyl))) + geom_bar()
      braidPlot(b, t)
      
      braidSave(b)
      braidCompile(outline_file)
      expect_that(file.exists(file.path(latex_path, pdf_file)), is_true())
      
    })


