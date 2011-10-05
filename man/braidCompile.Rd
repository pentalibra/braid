\name{braidCompile}
\alias{braidCompile}
\title{Compiles braid latex file to PDF.}
\usage{
  braidCompile(latexfile, output = "pdf", useXelatex =
  TRUE)
}
\arguments{
  \item{latexfile}{File location of a latex file}

  \item{output}{Determines what type of output to produce.
  Default to "pdf", currently the only supported format}

  \item{useXelatex}{If TRUE, uses xelatex to compile the
  latex.  If FALSE, uses \code{\link[tools]{texi2dvi}}.  If
  "guess", it uses a heuristic to see whether xelatex
  should be used or not: it searches for
  \code{\\usepackage\{xe*\}} in the latexfile; if found,
  uses xelatex otherwise texi2dvi.}
}
\description{
  This is a wrapper around \code{\link[tools]{texi2dvi}} to
  convert a latex file to PDF output.  No other formats are
  currently supported.
}

