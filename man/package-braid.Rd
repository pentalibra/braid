\name{package-braid}
\title{Tools for reporting on survey objects.}
\description{A report writing system that creates and compiles LaTEX and pdf output from R.}
\details{This package tries to solve the problem of writing simple reports in R, without having to learn LaTEX and SWeave first.  At the moment it supports creation of PDF files via LaTEX only.

Create a new braid:
\itemize{
\item{\code{\link{as.braid}}}
}

Report writing commands:
\itemize{
\item{\code{\link{braid_heading}}}
\item{\code{\link{braid_write}}}
\item{\code{\link{braid_plot}}}
}

Save results to file: 
\itemize{
\item{\code{\link{braid_save}}}
}

Compile to PDF:
\itemize{
\item{\code{\link{braid_latex_outline}}}
\item{\code{\link{braid_compile}}}
}}
\alias{braid}
\alias{package-braid}
\docType{package}
\author{Andrie de Vries \email{andrie.de.vries@pentalibra.com}}
\keyword{package}
