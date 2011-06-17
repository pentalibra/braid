# Creates Latex outline file
# 
# Author: Andrie
###############################################################################


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
#' @param output_filename The file name to use when writing the outline to disk
#' @param content_filename The name of the braid content file (this is embedded in the outline file using a \\input{} statement in Latex
#' @param title Document title.  You can use valid latex, including newline \\ - remember to use escape sequences
#' @param author Document author.  You can use valid latex.
#' @export
braid_latex_outline <- function(output_filename, content_filename, title, author){
  ret <- paste("
  \\documentclass[a4paper, 10pt]{report}
  
  \\usepackage[portrait, hmargin=2cm, top=2cm, bottom=2cm]{geometry}
  \\renewcommand{\\familydefault}{\\sfdefault}  %specify sans serif font
      
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
  \n")
  
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

  ret <- paste(ret, "\\input{", content_filename, "}\n\n")
    
  ret <- paste(ret, "\\end{document}\n")
  
  ### Now prepare file
  
  r_braid <- as.braid(
      path_latex = dirname(output_filename), 
      #output_filename=file.path(path_latex, output_filename)
      output_filename=output_filename
  )
  braid_write(r_braid, ret)
  braid_save(r_braid)
  message("Latex outline file created")
  invisible(NULL)
}
