\name{braid_latex_outline}
\alias{braid_latex_outline}
\title{Creates latex outline file...}
\usage{braid_latex_outline(output_filename, content_filename, title, author)}
\description{Creates latex outline file}
\details{This is a convenience function to generate a standard latex outline file.  Users with Latex experience may wish to generate the outline file by hand.  The main requirements of this outline file are:
- Contain all the document pre-amble
- In the main body section, have a \\input{} line that calls the braid content file.}
\arguments{\item{output_filename}{The file name to use when writing the outline to disk}
\item{content_filename}{The name of the braid content file (this is embedded in the outline file using a \\input{} statement in Latex}
\item{title}{Document title.  You can use valid latex, including newline \\ - remember to use escape sequences}
\item{author}{Document author.  You can use valid latex.}}
