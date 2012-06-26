# Creates Latex outline file
# 
# Author: Andrie
#------------------------------------------------------------------------------


#braid_packages <- function(){
#  
#}
#  
#
#braid_author <- function(x){
#  
#}

#' Creates latex outline file
#' 
#' This is a convenience function to generate a standard latex outline file.  Users with Latex experience may wish to generate the outline file by hand.  The main requirements of this outline file are:
#'  - Contain all the document pre-amble
#'  - In the main body section, have a \\input{} line that calls the braid content file.
#' 
#' @param b braid object
#' @param fileOuter The full file path and name to use when writing the outline to disk
#' @param fileInner The name of the braid content file (this is embedded in the outline file using a \\input{} statement in Latex
#' @param title Document title.  You can use valid latex, including newline \\ - remember to use escape sequences
#' @param author Document author.  You can use valid latex.
#' @export
braidLatexOutline <- function(b, fileOuter=b$fileOuter, fileInner=b$fileInner, title, author){
  if(!inherits(b, "braid")) stop("braidLatexOutline: argument b must be a braid object")
  ret <- paste("
  \\documentclass[a4paper, 10pt]{report}
  
  \\usepackage[portrait, hmargin=2cm, top=2cm, bottom=2cm]{geometry}
  \\renewcommand{\\familydefault}{\\sfdefault}  %specify sans serif font

  \\usepackage{fontspec}
  %\\setmainfont{Arial Unicode MS}
  %\\setmainfont{Unifont}      
  \\usepackage{fancyhdr}
  \\pagestyle{fancy}
  \\lhead{} %Sets top left header to nothing
      
  \\usepackage{placeins}
  \\usepackage{caption} %for non-floating figures
  \\usepackage{array} %extends functionality and formatting of tabular environment
  \\usepackage{rotating} %for vertical orientation of labels in tabular environments
  \\usepackage{graphicx} %to include .png graphics
  \\usepackage{float} %to force floats to appear HERE [H]
      %package float:  http://www.tex.ac.uk/tex-archive/macros/latex/contrib/float/
  \\DeclareGraphicsExtensions{.pdf, .eps, .png, .jpg}
  \\usepackage{datetime}
  
  \\usepackage[bookmarks=true, breaklinks=true]{hyperref} %create bookmarks - must be last package in list
  ")
  
  ret <- paste(ret, "\\title{", title, "}\n")
  ret <- paste(ret, "\\author{", author, "}\n")
  ret <- paste(ret, "\\date{\\today \\\\ \\currenttime}\n\n")
  
  ret <- paste(ret, 
  "\\newcommand\\PlaceGraph[1]
  {
    \\begin{center}
    \\begin{figure}[H]
    \\includegraphics{#1}
      \\end{figure}
      \\end{center}
    }\n\n")

  ret <- paste(ret, 
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     % Begin document
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     \\begin{document}

    \\maketitle
    \\setcounter{secnumdepth}{-1} % specify heading numbering depth
        \\tableofcontents
    \\clearpage
    
    %%% This is where the actual content goes\n\n")

  ret <- paste(ret, "\\input{", fileInner, "}\n\n")
    
  ret <- paste(ret, "\\end{document}\n")
  
  ### Now prepare file
#  r_braid <- as.braid(
#    path = b$path, 
#    fileOuter = fileOuter,
#    fileInner = fileOuter # Yes this is correct
#  )
#  braidWrite(r_braid, ret)
#  braidSave(r_braid)

  if(file.exists(fileOuter)) file.remove(fileOuter)
  file.create(fileOuter)
  
  sfile <- file(fileOuter, "wt")  ### Open file in append mode
  on.exit(close(sfile))
  cat(ret, file=sfile)

  message("Latex outline file created")
  invisible(NULL)
}
