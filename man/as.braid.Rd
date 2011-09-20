\name{as.braid}
\alias{as.braid}
\title{Creates object of class braid.}
\usage{
  as.braid(pathLatex = Sys.getenv()["TEMP"], pathGraphics =
  pathLatex, outputFilename = file.path(pathLatex,
  "braid.tex"), counterStart = 1, defaultPlotSize = c(5,
  3), dpi = 600)
}
\arguments{
  \item{pathLatex}{Path where latex will be saved}

  \item{pathGraphics}{Path where graphics files will be
  saved}

  \item{outputFilename}{Filename where latex output will be
  saved}

  \item{counterStart}{The starting number for a counter
  used to store graphs,}

  \item{defaultPlotSize}{Plot size in inches, e.g. c(4, 3)}

  \item{dpi}{Dots per inch, passed to ggsave()}
}
\value{
  A list object of class braid
}
\description{
  A braid object describes the data and meta-data in the
  survey that will be analysed by the analysis and
  reporting functions in the braid package.
}

