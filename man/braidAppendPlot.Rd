\name{braidAppendPlot}
\alias{braidAppendPlot}
\title{Appends braid plot.}
\usage{
  braidAppendPlot(braid, plot = NULL, filename = "",
    width = 4, height = 3, Qid = NA, reset = FALSE)
}
\arguments{
  \item{braid}{A braid object}

  \item{filename}{Filename without path. The path is
  obtained from the braid defaults}

  \item{width}{Width in inches}

  \item{height}{Height in inches}

  \item{Qid}{Optional identifying text that is used to
  print a message in the event the plot fails}
}
\description{
  The output from braidWrite is stored in a character
  vector, for later dumping to file.
}
\keyword{internal}

