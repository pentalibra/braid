# Braid function definitions
#
# Author: Andrie
#------------------------------------------------------------------------------


#' Creates braid heading in latex format and writes results to output file. 
#' 
#' Creates braid heading
#' 
#' @param braid A braid object
#' @param x A character vector
#' @param headinglevel Character vector corresponding to latex heading, e.g. Chapter, Section, etc.
#' @param pagebreak Forces a page break if TRUE
#' @export
braidHeading <- function(braid, x, headinglevel="chapter", pagebreak=FALSE){
  text <- paste("\\", headinglevel, "{", latexTranslate(x), "}", "\n", sep="")
  if(pagebreak) text <- paste(text, "\\pagebreak[4]\n")
  braidWrite(braid, text)
  invisible(NULL)
}

#------------------------------------------------------------------------------

#' Appends text to braid output file.
#' 
#' Appends text to braid output file.
#' 
#' @param braid A braid object
#' @param x A character vector of text
#' @param suffix A character vector to be appended to x, by default a line break
#' @export
braidWrite <- function(braid, x, suffix="default"){
  if(suffix=="default") suffix <- "\n"
  braidAppendText(braid, paste(x, suffix, sep=""))
  invisible(NULL)
}

#------------------------------------------------------------------------------

#' Generates filename to use when saving plots.
#' 
#' Creates a filename in the form prefix+counter+suffix+ext, e.g. "fig0001.pdf".
#' 
#' @param b A braid object
#' @param counter A numeric value.  By default it will increment the braid counter
#' @param prefix Filename prefix
#' @param format A format that will be passed to the sprintf function, e.g. "[percent]04d" will pad the counter with zeroes and create a character string of length 4, e.g. "0001"
#' @param suffix Filename suffix
#' @param ext Filename extension, by default ".pdf"
#' @export
braidFilename <- function(b, counter=braidIncCounter(b), 
    prefix="fig", format="default", suffix="", ext=".pdf"){
  if(format=="default") format <- "%04d"
  paste(prefix, sprintf(format, counter), suffix, ext, sep="")
}

#------------------------------------------------------------------------------

#' Compiles braid latex file to PDF.
#' 
#' This is a wrapper around \code{\link[tools]{texi2dvi}} to convert a latex file to PDF output.  No other formats are currently supported.
#' 
#' @param b A braid object
#' @param fileOuter File location of a latex file
#' @param output Determines what type of output to produce.  Default to "pdf", currently the only supported format
#' @param useXelatex If TRUE, uses \code{xelatex} to compile the latex.  If FALSE, uses \code{\link[tools]{texi2dvi}}.  If "guess", it uses a heuristic to see whether \code{xelatex} should be used or not: it searches for \code{\\usepackage\{xe*\}} in the latexfile; if found, uses \code{xelatex} otherwise texi2dvi.
#' @export
braidCompile <- function(b, fileOuter=b$fileOuter, output="pdf", useXelatex = TRUE){
  # Determine whether file exists
  if(!file.exists(fileOuter)) stop(paste("In braidCompile: file doesn't exist:", fileOuter))
  old_wd <- getwd()
  setwd(dirname(fileOuter))
  on.exit(setwd(old_wd))
  # Determine whether to use xelatex
  if(useXelatex == "guess"){
    zz <- scan(fileOuter, what="character")
    g1 <- grep("\\usepackage\\{xe.*?\\}", zz)
    g2 <- grep("^%", zz)
    useXelatex <- length(setdiff(g1, g2)) > 0
  }
  
  if(output!="pdf") stop(paste("In braidCompile: Output format", output, "not supported"))
  message("Starting to compile PDF document")
  if(useXelatex){
    # Checks that xelatex is installed
    if(as.logical(Sys.which("xelatex")=="")) stop("braidCompile: xelatex is not installed")
    # Compile latex file to PDF using xelatex
    res <- shell(cmd=paste("xelatex --interaction=batchmode", 
            basename(fileOuter)), mustWork=TRUE, intern=TRUE)
    print(tail(res, 2))
  } else {
    # Compile latex file to PDF using texi2dvi
    suppressWarnings(tools::texi2dvi(fileOuter, pdf=TRUE, clean=FALSE))
    suppressWarnings(tools::texi2dvi(fileOuter, pdf=TRUE, clean=TRUE))
  }
  message("All done.  Your file should now be ready:")
  message(paste(sub(".tex$", ".pdf", fileOuter), "\n"))
  invisible(NULL)
}




