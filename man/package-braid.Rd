\name{package-braid}
\title{Tools for reporting on survey objects.}
\description{A report writing system that creates and compiles LaTEX and pdf output from R.}
\details{This package tries to solve the problem of writing simple reports in R, without having to learn LaTEX and SWeave first.  At the moment it supports creation of PDF files via LaTEX only.

Create a new braid:
\itemize{
\code{\link{as.braid}}
}

Report writing commands:
\itemize{
\code{\link{braid_heading}}
\code{\link{braid_write}}
\code{\link{braid_plot}}
}

Save results to file: 
\itemize{
\code{\link{braid_save}}
}

Compile to PDF:
\itemize{
\code{\link{braid_latex_outline}}
\code{\link{braid_compile}}
}
\alias{braid}
\alias{package-braid}
\docType{package}
\author{Andrie de Vries \email{andrie.de.vries@pentalibra.com}}
\keyword{package}
