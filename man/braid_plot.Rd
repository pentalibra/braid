\name{braid_plot}
\alias{braid_plot}
\title{Saves braid plot to pdf.}
\usage{braid_plot(braid, x, filename=braid_filename(braid),
    width=braid$default_plot_size[1],
    height=braid$default_plot_size[2])}
\description{Saves braid plot to pdf.}
\details{Saves braid plot to pdf.  Uses either ggsave() or pdf(), depending on the class of plot.  Supports plots of class ggsave and trellis (i.e. lattice plots).}
\arguments{\item{braid}{A braid object}
\item{x}{A plot object}
\item{filename}{Filename without path. The path is obtained from the braid defaults}
\item{width}{Width in inches}
\item{height}{Heigh in inches}}
