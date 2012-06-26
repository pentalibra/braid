\name{as.braid}
\alias{as.braid}
\title{Creates object of class braid.}
\usage{
  as.braid(path = tempdir(), graphics = "graphics",
    fileOuter = "outline.tex", fileInner = "braid.tex",
    counterStart = 1, defaultPlotSize = c(5, 3), dpi = 600,
    outputType = c("latex", "ppt", "device"),
    graphicFormat = c("pdf", "wmf"))
}
\arguments{
  \item{path}{Character vector: File path where latex
  output will be saved}

  \item{graphics}{Character vector: File path relative to
  \code{pathOutput} where to save graphics (this gets
  appended to pathOutput.)}

  \item{fileOuter}{Filename of latex outline file (used by
  \code{\link{braidCompile}}}

  \item{fileInner}{Filename where latex output will be
  saved (This gets appended to \code{pathOutput})}

  \item{counterStart}{The starting number for a counter
  used to store graphs, defaults to 1}

  \item{defaultPlotSize}{Numeric vector of length 2: Plot
  size in inches, e.g. c(4, 3)}

  \item{dpi}{Dots per inch, passed to ggsave()}

  \item{outputType}{Character string specifying the
  destination of output: "latex", "ppt" or "device".  If
  "device", graphs are sent to the default device
  (typically the RGgui plot terminal)}

  \item{graphicFormat}{Device type for saving graphic
  plots.  Currently only pdf and wmf is supported.}
}
\value{
  A list object of class braid
}
\description{
  A braid object describes the data and meta-data in the
  survey that will be analysed by the analysis and
  reporting functions in the braid package.
}

