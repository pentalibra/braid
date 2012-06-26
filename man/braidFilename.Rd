\name{braidFilename}
\alias{braidFilename}
\title{Generates filename to use when saving plots.}
\usage{
  braidFilename(b, counter = braidIncCounter(b),
    prefix = "fig", format = "default", suffix = "",
    ext = ".pdf")
}
\arguments{
  \item{b}{A braid object}

  \item{counter}{A numeric value.  By default it will
  increment the braid counter}

  \item{prefix}{Filename prefix}

  \item{format}{A format that will be passed to the sprintf
  function, e.g. "[percent]04d" will pad the counter with
  zeroes and create a character string of length 4, e.g.
  "0001"}

  \item{suffix}{Filename suffix}

  \item{ext}{Filename extension, by default ".pdf"}
}
\description{
  Creates a filename in the form prefix+counter+suffix+ext,
  e.g. "fig0001.pdf".
}

