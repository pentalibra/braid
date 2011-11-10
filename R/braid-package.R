# Braid package documentation
# 
# Author: Andrie
#------------------------------------------------------------------------------


#' A report writing system that creates and compiles LaTEX and pdf output from R.
#'
#' This package tries to solve the problem of writing simple reports in R, without having to learn LaTEX and SWeave first.  At the moment it supports creation of PDF files via LaTEX only.
#' 
#' Create a new braid:
#' \itemize{
#' \item{\code{\link{as.braid}}}
#' }
#' 
#' Report writing commands:
#' \itemize{
#' \item{\code{\link{braidHeading}}}
#' \item{\code{\link{braidWrite}}}
#' \item{\code{\link{braidPlot}}}
#' }
#' 
#' Save results to file: 
#' \itemize{
#' \item{\code{\link{braidSave}}}
#' }
#' 
#' Compile to PDF:
#' \itemize{
#' \item{\code{\link{braidLatexOutline}}}
#' \item{\code{\link{braidCompile}}}
#' } 
#' @name braid-package
#' @aliases braid braid-package
#' @docType package
#' @import Hmisc xtable tools
#' @title A report writing system to create latex output from R.
#' @author Andrie de Vries \email{andrie.de.vries@@pentalibra.com}
#' @keywords package

NULL

