\name{braidLatexOutline}
\alias{braidLatexOutline}
\title{Creates latex outline file}
\usage{
  braidLatexOutline(b, fileOuter = b$fileOuter,
    fileInner = b$fileInner, title, author)
}
\arguments{
  \item{b}{braid object}

  \item{fileOuter}{The full file path and name to use when
  writing the outline to disk}

  \item{fileInner}{The name of the braid content file (this
  is embedded in the outline file using a \\input{}
  statement in Latex}

  \item{title}{Document title.  You can use valid latex,
  including newline \\ - remember to use escape sequences}

  \item{author}{Document author.  You can use valid latex.}
}
\description{
  This is a convenience function to generate a standard
  latex outline file.  Users with Latex experience may wish
  to generate the outline file by hand.  The main
  requirements of this outline file are: - Contain all the
  document pre-amble - In the main body section, have a
  \\input{} line that calls the braid content file.
}

