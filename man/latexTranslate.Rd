\name{latexTranslate}
\alias{latexTranslate}
\title{Prints a string to Latex, dealing with special characters.}
\usage{
  latexTranslate(object, inn = NULL, out = NULL,
    pb = FALSE, greek = FALSE, ...)
}
\arguments{
  \item{object}{A string containing question text}

  \item{inn}{specify additional input strings over the
  usual defaults}

  \item{out}{specify additional translated strings over the
  usual defaults}

  \item{pb}{If pb=TRUE, latexTranslate also translates [()]
  to math mode using \\left, \\right.}

  \item{greek}{set to TRUE to have latexTranslate put names
  for greek letters in math mode and add backslashes}

  \item{...}{ignored}
}
\description{
  Translates particular items in character strings to LaTeX
  format, e.g., makes a^2 = a\$^2\$ for superscript within
  variable labels. LaTeX names of greek letters (e.g.,
  "alpha") will have backslashes added if greek==TRUE. Math
  mode is inserted as needed. \code{latexTranslate} assumes
  that input text always has matches, e.g. [) [] (] (), and
  that surrounding by \$\$ is OK.
}
\note{
  This code is copied from Hmisc.  Hmisc used to export
  this function.
}

