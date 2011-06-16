\name{braid_compile}
\alias{braid_compile}
\title{Compiles braid latex file to PDF or other output...}
\usage{braid_compile(latexfile, output="pdf")}
\description{Compiles braid latex file to PDF or other output}
\details{This is a wrapper around \code{\link{tools::texi2dvi}} to convert a latex file to PDF output.  No other formats are currently supported.}
\arguments{\item{latexfile}{File location of a latex file}
\item{output}{Determines what type of output to produce.  Default to "pdf", currently the only supported format}}
