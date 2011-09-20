\name{braidCompile}
\alias{braidCompile}
\title{Compiles braid latex file to PDF or other output}
\usage{
  braidCompile(latexfile, output = "pdf")
}
\arguments{
  \item{latexfile}{File location of a latex file}

  \item{output}{Determines what type of output to produce.
  Default to "pdf", currently the only supported format}
}
\description{
  This is a wrapper around \code{\link[tools]{texi2dvi}} to
  convert a latex file to PDF output.  No other formats are
  currently supported.
}

