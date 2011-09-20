\name{braidPlot}
\alias{braidPlot}
\title{Saves braid plot to pdf.}
\usage{
  braidPlot(braid, x, filename = braidFilename(braid),
  width = braid$defaultPlotSize[1], height =
  braid$defaultPlotSize[2])
}
\arguments{
  \item{braid}{A braid object}

  \item{x}{A plot object}

  \item{filename}{Filename without path. The path is
  obtained from the braid defaults}

  \item{width}{Width in inches}

  \item{height}{Heigh in inches}
}
\description{
  Saves braid plot to pdf.  Uses either ggsave() or pdf(),
  depending on the class of plot.  Supports plots of class
  ggsave and trellis (i.e. lattice plots).
}

