\name{braidPlot}
\alias{braidPlot}
\title{Saves braid plot to pdf.}
\usage{
  braidPlot(braid, plotcode,
    filename = braidFilename(braid),
    width = braid$defaultPlotSize[1],
    height = braid$defaultPlotSize[2], Qid = NA)
}
\arguments{
  \item{braid}{A braid object}

  \item{plotcode}{A plot object (either ggplot or lattice)}

  \item{filename}{Filename without path. The path is
  obtained from the braid defaults}

  \item{width}{Width in inches}

  \item{height}{Height in inches}

  \item{Qid}{Optional identifying text that is used to
  print a message in the event the plot fails}
}
\description{
  Saves braid plot to pdf.  Uses either ggsave() or pdf(),
  depending on the class of plot.  Supports plots of class
  ggsave and trellis (i.e. lattice plots).
}

