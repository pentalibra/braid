# TODO: Add comment
# 
# Author: Andrie
###############################################################################


#' Creates braid heading in latex format and writes results to output file. 
#' 
#' Creates braid heading
#' 
#' @param braid A braid object
#' @param x A character vector
#' @param headinglevel Character vector corresponding to latex heading, e.g. Chapter, Section, etc.
#' @param pagebreak Forces a page break if TRUE
#' @export
braid_heading <- function(braid, x, headinglevel="chapter", pagebreak=FALSE){
  text <- paste("\\", headinglevel, "{", latexTranslate(x), "}", "\n", sep="")
  if(pagebreak) text <- paste(text, "\\pagebreak[4]\n")
  braid_write(braid, text)
  invisible(NULL)
}


###############################################################################

#' Appends text to braid output file.
#' 
#' Appends text to braid output file.
#' 
#' @param braid A braid object
#' @param x A character vector of text
#' @param suffix A character vector to be appended to x, by default a line break
#' @export
braid_write <- function(braid, x, suffix="default"){
  if(suffix=="default") suffix <- "\n"
  braid_append_text(braid, paste(x, suffix, sep=""))
  invisible(NULL)
}
###############################################################################

#' Writes result to braid output file.
#' 
#' Writes result to braid output file.
#' 
#' @param braid A braid object
#' @export
braid_save <- function(braid){
  text <- braid_append_text(braid)
  sfile <- file(braid$output_filename, "at")  ### Open file in append mode
  on.exit(close(sfile))
  cat(text, file=sfile, append=TRUE)
  braid_append_text(braid, reset=TRUE)
  invisible(NULL)
}


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
braid_filename <- function(b, counter=braid_inc_counter(b), 
    prefix="fig", format="default", suffix="", ext=".pdf"){
  if(format=="default") format <- "%04d"
  paste(prefix, sprintf(format, counter), suffix, ext, sep="")
}

###############################################################################

#' Saves braid plot to pdf.
#' 
#' Saves braid plot to pdf.  Uses either ggsave() or pdf(), depending on the class of plot.  Supports plots of class ggsave and trellis (i.e. lattice plots).
#' 
#' @param braid A braid object
#' @param x A plot object
#' @param filename Filename without path. The path is obtained from the braid defaults
#' @param width Width in inches
#' @param height Heigh in inches
#' @export
braid_plot <- function(braid, x, filename=braid_filename(braid), 
    width=braid$default_plot_size[1], 
    height=braid$default_plot_size[2]){
  if(inherits(x, "ggplot")){
    require(ggplot2)
    ggplot2::ggsave(
        filename = filename, 
        plot=x, 
        width    = width, 
        height   = height,
        dpi      = braid$dpi, 
        path     = braid$path_graphics
    )
  } 
  if(inherits(x, "trellis")){
    require(lattice)
    on.exit(dev.off())
    pdf(
        file=file.path(braid$path_graphics, filename),
        width    = width, 
        height   = height
    )
    print(x)
  } 
  braid_write(braid, paste("  \\PlaceGraph{", "graphics", "/", filename, "}", sep=""))
  invisible(NULL)
}

###############################################################################

#' Compiles braid latex file to PDF or other output
#' 
#' This is a wrapper around \code{\link{tools::texi2dvi}} to convert a latex file to PDF output.  No other formats are currently supported.
#' 
#' @param latexfile File location of a latex file
#' @param output Determines what type of output to produce.  Default to "pdf", currently the only supported format
#' @export
braid_compile <- function(latexfile, output="pdf")
{
  old_wd <- getwd()
  setwd(dirname(latexfile))
  on.exit(setwd(old_wd))
  if(output=="pdf"){
    ### Compile latex file to PDF
    message("Starting to compile PDF document")
    suppressWarnings(tools::texi2dvi(latexfile, pdf=TRUE, clean=FALSE))
    suppressWarnings(tools::texi2dvi(latexfile, pdf=TRUE, clean=TRUE))
    message("All done.  Your file should now be ready:")
    message(paste(sub(".tex$", ".pdf", latexfile), "\n"))
  } else {
    stop(paste("In braid_compile: Output format", output, "not supported"))
  }
  invisible(NULL)
}




